//==============================================================================
//  Unit:        LocationDetailsData
//
//  Implements:  TMapSheet
//               TdmLocationDetails
//               TAdminAreaItem
//               TAdminAreaList
//               TBoundaryItem
//               TBoundaryList
//               TDesignationItem
//               TDesignationList
//               TGridSquareList
//               TLandParcelItem
//               TLandParcelList
//               TLocationNameItem
//               TLocationNameList
//               TRelationItem
//               TRelationList
//               TTenureItem
//               TTenureList
//               TUseItem
//               TUseList
//
//  Description: Implements data access functionality for the location details
//               screen.
//
//               TAdminAreaItem and TAdminAreaList
//               Helper classes used on the Administrative Areas sub-tab of the
//               Geo Info tab of the details screen.
//
//               TBoundaryItem and TBoundaryList
//               Helper classes used on the Boundaries sub-tab of the Geo Info
//               tab of the details screen.
//
//               TDesignationItem and TDesignationList
//               Helper classes used on the Designation tab of the details screen.
//
//               TGridSquareList
//               Helper class working in conjunction with TGridSquareItem in the
//               GridSquareItem unit, and used on the Grid Squares sub-tab of
//               the Geo Info tab of the details screen.
//
//               TLandParcelItem and TLandParcelList
//               Helper classes used on the Land Parcel sub-tab of the Geo Info
//               tab of the details screen.
//
//               TLocationItem and TLocationList
//               Helper classes used on the General tab of the details screen.
//
//               TRelationItem and TRelationList
//               Helper classes used on the Relations sub-tab of the Other Info
//               tab of the details screen.
//
//               TTenureItem and TTenureList
//               Helper classes used on the Tenure sub-tab of the Other Info tab
//               of the details screen.
//
//               TUseItem and TUseList
//               Helper classes used on the Uses sub-tab of the Other Info tab
//               of the details screen.
//
//  Author:      Eric Salmon
//  Created:     3 June 1999
//
//  Changes:     Eric Salmon 07/02/2002
//               Datasets properties (DatabaseName, SessionName, Connection) set
//               via dmDatabase.
//
//  Last Revision Details:
//    $Revision: 74 $
//    $Date: 8/04/10 16:53 $
//    $Author: Andrewkemp $
//
//  Copyright © Dorset Software Services Ltd, 2001
//
//==============================================================================

unit LocationDetailsData;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, Db,
  BaseData, Grids, GridSquareItem, Constants, ExceptionForm, DataClasses, SpatialRefFuncs,
  MapServerLink, JNCCDatasets, ADODB, DatabaseAccessADO, GeneralFunctions;

resourcestring
  ResStr_CannotDeleteLocation = 'The selected location is referenced in a Survey Event or a Sample and cannot be deleted';

type
  ELocationError = class (TExceptionPath);

  TMapSheet = class(TObject)
  private
    FMapSheetKey: string;
    FBaseMapKey: string;
    FFileName: string;
    FIDField: string;
    FIsInternal: boolean;
  public
    constructor CreateInternal(fileName, mapSheetKey, baseMapKey: string);
    constructor CreateExternal(fileName, idField: string);
    constructor CopyFrom(original: TMapSheet); 
    property MapSheetKey: string read FMapSheetKey write FMapSheetKey;
    property BaseMapKey: string read FBaseMapKey;
    property FileName: string read FFileName;
    property IDField: string read FIDField;
    property IsInternal: boolean read FIsInternal;
  end;

  //============================================================================
  TdmLocationDetails = class(TBaseDataModule)
    qryLocation: TJNCCQuery;
    dsLocation: TDataSource;
    dsLocationType: TDataSource;
    qryLocNames: TJNCCQuery;
    qryDesignation: TJNCCQuery;
    qrySiteStatus: TJNCCQuery;
    dsSiteStatus: TDataSource;
    qryAdminAreas: TJNCCQuery;
    qryGridSquares: TJNCCQuery;
    qryLandParcels: TJNCCQuery;
    qryBoundaries: TJNCCQuery;
    qryRelations: TJNCCQuery;
    qryUses: TJNCCQuery;
    tblTenureType: TJNCCTable;
    dsTenureType: TDataSource;
    qryTenures: TJNCCQuery;
    qryCheckForeignKey: TJNCCQuery;
    qryLocationType: TJNCCQuery;
  private
    { Private declarations }
    function IsForeignKey(const TableName, FieldName: String; const Key: string): Boolean;
  public
    { Public declarations }
    constructor Create(AOwner:TComponent); override;
    function DeleteRecord(const ALocDetKey:string):boolean;
  end;  // TdmLocationDetails

  //============================================================================
  TLocationNameItem = class(TStringDataItem)
  private
    FLocationName: string;
    FPreferred: boolean;
    procedure SetLocationName(const Value: string);
    procedure SetPreferred(const Value: boolean);
  protected
    procedure InitFromRecord(iDataset: TDataset); override;
    procedure WriteToRecord(iDataset: TDataset); override;
  public
    property LocationName:string read FLocationName write SetLocationName;
    property Preferred:boolean read FPreferred write SetPreferred;
  end;  // TLocationNameItem

  //----------------------------------------------------------------------------
  TLocationNameList = class (TStringDataList)
  private
    FLocationKey: string;
  protected
    function ItemsDisplayName: String; override;
    function GetText(iDataItem : TDataItem) : string; override;
    procedure DoAdditions;
    procedure DoModifications; override;
    procedure ProcessUpdate; override;
  public
    constructor Create(iDataSet: TDataset; const iKeyFieldName: string;
      iStrings: TStrings; iItemClass: TItemClass); override;
    property LocationKey:string read FLocationKey write FLocationKey;
  end;  // TLocationNameList

  //============================================================================
  TDesignationItem = class(TGridDataItem)
  private
    FSiteStatusKey: string;
    FSiteStatus: string;
    FRefCode: string;
    FAuthorityKey: string;
    FAuthority: string;
    FDateFrom: string;
    FDateTo: string;
    FComment: TMemoryStream;
    procedure SetAuthority(const Value: string);
    procedure SetAuthorityKey(const Value: string);
    procedure SetDateFrom(const Value: string);
    procedure SetDateTo(const Value: string);
    procedure SetRefCode(const Value: string);
    procedure SetSiteStatus(const Value: string);
    procedure SetSiteStatusKey(const Value: string);
  protected
    procedure InitFromRecord(iDataset: TDataset); override;
    procedure WriteToRecord(iDataset: TDataset); override;
    function GetCell(const iX: integer): string; override;
  public
    constructor CreateNew(AOwner:TCacheDataList); override;
    destructor Destroy; override;
    property SiteStatusKey:string read FSiteStatusKey write SetSiteStatusKey;
    property SiteStatus:string read FSiteStatus write SetSiteStatus;
    property RefCode:string read FRefCode write SetRefCode;
    property AuthorityKey:string read FAuthorityKey write SetAuthorityKey;
    property Authority:string read FAuthority write SetAuthority;
    property DateFrom:string read FDateFrom write SetDateFrom;
    property DateTo:string read FDateTo write SetDateTo;
    property Comment:TMemoryStream read FComment;
  end;  // TDesignationItem

  //----------------------------------------------------------------------------
  TDesignationList = class(TGridDataList)
  private
    FLocationKey: string;
  protected
    function ItemsDisplayName: String; override;
    procedure DoAdditions;
    procedure ProcessUpdate; override;
  public
    property LocationKey:string read FLocationKey write FLocationKey;
  end;  // TDesignationList

  //============================================================================
  TAdminAreaItem = class(TGridDataItem)
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
  end;  // TAdminAreaItem

  //----------------------------------------------------------------------------
  TAdminAreaList = class(TGridDataList)
  private
    FLocationKey: string;
    procedure SetLocationKey(const Value: string);
  protected
    function ItemsDisplayName: String; override;
    procedure DoAdditions;
    procedure ProcessUpdate; override;
  public
    property LocationKey: string read FLocationKey write SetLocationKey;
  end;  // TAdminAreaList

  //============================================================================
  TGridSquareList = class(TGridDataList)
  private
    FLocationKey: string;
    procedure SetLocationKey(const Value: string);
  protected
    function ItemsDisplayName: String; override;
    procedure DoAdditions;
    procedure ProcessUpdate; override;
  public
    property LocationKey: string read FLocationKey write SetLocationKey;
  end;  // TGridSquareList

  //============================================================================
  TLandParcelItem = class(TGridDataItem)
  private
    FMapSystem: String;
    FNumber: String;
    procedure SetMapSystem(const Value: String);
    procedure SetNumber(const Value: String);
  protected
    procedure InitFromRecord(iDataset: TDataset); override;
    procedure WriteToRecord(iDataset: TDataset); override;
    function GetCell(const iX: integer): string; override;
  public
    property Number: String read FNumber write SetNumber;
    property MapSystem: String read FMapSystem write SetMapSystem;
  end;  // TLandParcelItem

  //----------------------------------------------------------------------------
  TLandParcelList = class(TGridDataList)
  private
    FLocationKey: string;
    procedure SetLocationKey(const Value: string);
  protected
    function ItemsDisplayName: String; override;
    procedure DoAdditions;
    procedure ProcessUpdate; override;
  public
    property LocationKey: string read FLocationKey write SetLocationKey;
  end;  // TLandParcelList

  //============================================================================
  TBoundaryItem = class(TGridDataItem)
  private
    FVersion: Integer;
    FOriginalMapSheetKey: String;
    FObjectID: Integer;
    FObjectKey: String;
    FOriginalObjectID: Integer;
    FFromDate: String;
    FToDate: String;
    FMapSheet: TMapSheet;
    procedure SetFromDate(const Value: String);
    procedure SetToDate(const Value: String);
    procedure SetMapSheetKey(const Value: String);
    procedure SetObjectID(const Value: Integer);
    procedure SetVersion(const Value: Integer);
    procedure SetMapSheet(const Value: TMapSheet);
    function GetMapSheetKey: String;
  protected
    procedure InitFromRecord(iDataset: TDataset); override;
    procedure WriteToRecord(iDataset: TDataset); override;
    function GetCell(const iX: integer): string; override;
  public
    property FromDate: String read FFromDate write SetFromDate;
    property ToDate: String read FToDate write SetToDate;
    property Version: Integer read FVersion write SetVersion;
    property MapSheetKey: String read GetMapSheetKey write SetMapSheetKey;
    property ObjectId: Integer read FObjectID write SetObjectID;
    property ObjectKey: string read FObjectKey write FObjectKey;
    property MapSheet: TMapSheet read FMapSheet write SetMapSheet;
  end;  // TBoundaryItem

  //----------------------------------------------------------------------------
  TBoundaryList = class(TGridDataList)
  private
    FLocationKey: string;
    procedure SetLocationKey(const Value: string);
  protected
    function ItemsDisplayName: String; override;
    procedure DoAdditions;
    procedure ProcessUpdate; override;
  public
    procedure LinkObjectToBoundary(const AMapSheetKey: string; AStaticID: Integer;
      const ALocationKey, ABoundaryKey: string);
    property LocationKey: string read FLocationKey write SetLocationKey;
  end;  // TBoundaryList

  //============================================================================
  TRelationItem = class(TGridDataItem)
  private
    FLocation2Name: String;
    FRelationship: String;
    FLocation2Key: string;
    procedure SetLocation2Key(const Value: string);
    procedure SetLocation2Name(const Value: String);
    procedure SetRelationship(const Value: String);
  protected
    procedure InitFromRecord(iDataset: TDataset); override;
    procedure WriteToRecord(iDataset: TDataset); override;
    function GetCell(const iX: integer): string; override;
  public
    property Location2Key: string read FLocation2Key write SetLocation2Key;
    property Location2Name: String read FLocation2Name write SetLocation2Name;
    property Relationship: String read FRelationship write SetRelationship;
  end;  // TRelationItem

  //----------------------------------------------------------------------------
  TRelationList = class(TGridDataList)
  private
    FLocationKey: string;
    procedure SetLocationKey(const Value: string);
  protected
    function ItemsDisplayName: String; override;
    procedure DoAdditions;
    procedure ProcessUpdate; override;
  public
    property LocationKey: string read FLocationKey write SetLocationKey;
  end;  // TRelationList

  //============================================================================
  TUseItem = class(TGridDataItem)
  private
    FComment: TMemoryStream;
    FPotential: TMemoryStream;
    FUse: String;
    FFromDate: String;
    FToDate: String;
    procedure SetUse(const Value: String);
    procedure SetFromDate(const Value: String);
    procedure SetToDate(const Value: String);
  protected
    procedure InitFromRecord(iDataset: TDataset); override;
    procedure WriteToRecord(iDataset: TDataset); override;
    function GetCell(const iX: integer): string; override;
  public
    constructor CreateNew(aOwner : TCacheDataList); override;
    property Use: String read FUse write SetUse;
    property Potential: TMemoryStream read FPotential;
    property Comment: TMemoryStream read FComment;
    property FromDate: String read FFromDate write SetFromDate;
    property ToDate: String read FToDate write SetToDate;
  end;

  //----------------------------------------------------------------------------
  TUseList = class(TGridDataList)
  private
    FLocationKey: string;
    procedure SetLocationKey(const Value: string);
  protected
    function ItemsDisplayName: String; override;
    procedure DoAdditions;
    procedure ProcessUpdate; override;
  public
    property LocationKey: string read FLocationKey write SetLocationKey;
  end;  // TUseList

  //============================================================================
  TTenureItem = class(TGridDataItem)
  private
    FOwnerName: String;
    FFromDate: String;
    FTypeName: String;
    FToDate: String;
    FTypeKey: string;
    FOwnerKey: string;
    procedure SetFromDate(const Value: String);
    procedure SetOwnerKey(const Value: string);
    procedure SetToDate(const Value: String);
    procedure SetTypeKey(const Value: string);
    procedure SetOwnerName(const Value: String);
    procedure SetTypeName(const Value: String);
  protected
    procedure InitFromRecord(iDataset: TDataset); override;
    procedure WriteToRecord(iDataset: TDataset); override;
    function GetCell(const iX: integer): string; override;
  public
    property FromDate: String read FFromDate write SetFromDate;
    property ToDate: String read FToDate write SetToDate;
    property OwnerKey: string read FOwnerKey write SetOwnerKey;
    property OwnerName: String read FOwnerName write SetOwnerName;
    property TypeKey: string read FTypeKey write SetTypeKey;
    property TypeName: String read FTypeName write SetTypeName;
  end;  // TTenureItem

  //----------------------------------------------------------------------------
  TTenureList = class(TGridDataList)
  private
    FLocationKey: string;
    procedure SetLocationKey(const Value: string);
  protected
    function ItemsDisplayName: String; override;
    procedure DoAdditions;
    procedure ProcessUpdate; override;
  public
    property LocationKey: string read FLocationKey write SetLocationKey;
  end;  // TTenureList

//==============================================================================
implementation

{$R *.DFM}

uses
  GeneralData, ApplicationSettings, Maintbar, FormActions;

resourcestring
  ResStr_Designation =  'Designation';
  ResStr_AdminArea =  'Administrative Area';
  ResStr_GridSquare = 'Grid Square';
  ResStr_LocRelation =  'Location Relation';

//==============================================================================
{ TMapSheet }
//------------------------------------------------------------------------------

{-------------------------------------------------------------------------------
  Creates a reference to an internal map sheet.
}
constructor TMapSheet.CreateInternal(fileName, mapSheetKey, baseMapKey: string);
begin
  FFileName := fileName;
  FMapSheetKey := mapSheetKey;
  FBaseMapKey := baseMapKey;
  FIsInternal := True;
end;

{-------------------------------------------------------------------------------
  Creates a reference to an external map sheet.
}
constructor TMapSheet.CreateExternal(fileName, idField: string);
begin
  FFileName := fileName;
  FIDField := idField;
  FIsInternal := False;
end;

{-------------------------------------------------------------------------------
  Creates a new map sheet as an exact copy of an existing map sheet.
}
constructor TMapSheet.CopyFrom(original: TMapSheet);
begin
  FMapSheetKey := original.MapSheetKey;
  FBaseMapKey := original.BaseMapKey;
  FFileName := original.FileName;
  FIDField := original.FIDField;
  FIsInternal := original.IsInternal;
end;

//==============================================================================
{ TdmLocationDetails }
//------------------------------------------------------------------------------
constructor TdmLocationDetails.Create(AOwner:TComponent);
begin
  inherited Create(AOwner);
  qryLocationType.Open;
end;  // Create

//------------------------------------------------------------------------------
function TdmLocationDetails.DeleteRecord(const ALocDetKey: string): Boolean;
begin
  //Check the record is not required for another part of the program
  if IsForeignKey('SURVEY_EVENT', 'LOCATION_KEY', ALocDetKey)
    or IsForeignKey('SAMPLE', 'LOCATION_KEY', ALocDetKey) then
  begin
    MessageDlg(ResStr_CannotDeleteLocation, mtInformation, [mbOK], 0);
    Result:= False;
  end else
    with dmGeneralData do begin
      ExecuteSQL('DELETE FROM Location_Relation WHERE Location_Key_1 = '''+ALocDetKey+'''',
                 ResStr_DelFail+' - LOCATION_RELATION table (Key1)');
      ExecuteSQL('DELETE FROM Location_Relation WHERE Location_Key_2 = '''+ALocDetKey+'''',
                 ResStr_DelFail+' - LOCATION_RELATION table (Key2)');
      // Delete related records
      ExecuteSQL('DELETE FROM Grid_Square WHERE Location_Key = '''+ALocDetKey+'''',
                 ResStr_DelFail+' - GRID_SQUARE table');
      ExecuteSQL('DELETE FROM Land_Parcel WHERE Location_Key = '''+ALocDetKey+'''',
                 ResStr_DelFail+' - LAND_PARCEL table');
      ExecuteSQL('DELETE FROM Location_Admin_Areas WHERE Location_Key = '''+ALocDetKey+'''',
                 ResStr_DelFail+' - LOCATION_ADMIN_AREAS table');
      ExecuteSQL('DELETE FROM Location_Boundary WHERE Location_Key = '''+ALocDetKey+'''',
                 ResStr_DelFail+' - LOCATION_BOUNDARY table');
      ExecuteSQL('DELETE FROM Location_Data WHERE Location_Key = '''+ALocDetKey+'''',
                 ResStr_DelFail+' - LOCATION_DATA table');
      ExecuteSQL('DELETE FROM Location_Designation WHERE Location_Key = '''+ALocDetKey+'''',
                 ResStr_DelFail+' - LOCATION_DESIGNATION table');
      ExecuteSQL('DELETE FROM Location_Name WHERE Location_Key = '''+ALocDetKey+'''',
                 ResStr_DelFail+' - LOCATION_NAME table');
      DelSources('Location_Sources','Location_Key',ALocDetKey);

      ExecuteSQL('DELETE FROM Location_Use WHERE Location_Key = '''+ALocDetKey+'''',
                 ResStr_DelFail+' - LOCATION_USE table');
      ExecuteSQL('DELETE FROM Tenure WHERE Location_Key = '''+ALocDetKey+'''',
                 ResStr_DelFail+' - TENURE table');
      // And finally the location
      ExecuteSQL('DELETE FROM Location WHERE Location_Key = '''+ALocDetKey+'''',
                 ResStr_DelFail+' - LOCATION table');
      Result:=true;
    end;
end;  // DeleteRecord

//==============================================================================
function TdmLocationDetails.IsForeignKey(const TableName, FieldName: String;
  const Key: string): Boolean;
begin
  with qryCheckForeignKey do //SQL[1] is table name, SQL[3] is field name
  begin
    SQL[1]:= TableName;
    SQL[3]:= FieldName;
    Parameters.ParamByName('Key').Value := Key;

    Open;
    Result:= (Fields[0].AsInteger <> 0);
    Close;
  end;
end;  // IsForeignKey

//==============================================================================
{ TLocationNameItem }
//------------------------------------------------------------------------------
procedure TLocationNameItem.InitFromRecord(iDataset: TDataset);
begin
  with iDataSet do
    try
      FItemKey     :=FieldByName('Location_Name_Key').AsString;
      FLocationName:=FieldByName('Item_Name').AsString;
      FPreferred   :=FieldByName('Preferred').AsBoolean;
    except on E:Exception do
      raise ELocationError.Create(ResStr_InitRecFail+' - LOCATION_NAME',E);
    end;
end;  // InitFromRecord

//------------------------------------------------------------------------------
procedure TLocationNameItem.WriteToRecord(iDataset: TDataset);
begin
  with iDataSet do
    try
      // cannot update location name unless custodian, but can change the preferred flag
      if Custodian=AppSettings.SiteID then
        FieldByName('Item_Name').AsString :=FLocationName;
      FieldByName('Preferred').AsBoolean:=FPreferred;
    except on E:Exception do
      raise ELocationError.Create(ResStr_WriteRecFail+' - LOCATION_NAME',E);
    end;
end;  // WriteToRecord

//------------------------------------------------------------------------------
procedure TLocationNameItem.SetLocationName(const Value: string);
begin
  FLocationName := Value;
  SetModified;
end;  // SelLocationName

//------------------------------------------------------------------------------
procedure TLocationNameItem.SetPreferred(const Value: boolean);
begin
  FPreferred := Value;
  SetModified;
end;  // SetPreferred

//==============================================================================
{ TLocationNameList }
function TLocationNameList.ItemsDisplayName: String;
begin
  Result := ResStr_LocationName;
end;

//------------------------------------------------------------------------------
function TLocationNameList.GetText(iDataItem: TDataItem): string;
begin
  Result:=TLocationNameItem(iDataItem).LocationName;
end;  // GetText

//------------------------------------------------------------------------------
procedure TLocationNameList.DoAdditions;
var lqryAddition: TJnccQuery;
    lDataItem   : TLocationNameItem;
    i           : integer;
begin
  if ItemsToAdd.Count = 0 then Exit;
  lqryAddition := TJnccQuery.Create(nil);
  try
    dmDatabase.SetDatabaseLocal([lqryAddition]);
    with lqryAddition do
      try
        SQL.Text := 'SELECT * FROM Location_Name WHERE Location_Name_Key = ''''';
        Open;
        for i := 0 to ItemsToAdd.Count - 1 do begin
          lDataItem := TLocationNameItem(ItemsToAdd[i]);
          Append;
          FieldByName('Location_Name_Key').AsString :=
              dmGeneralData.GetNextKey('Location_Name', 'Location_Name_Key');
          FieldByName('Location_Key').AsString      := LocationKey;
          FieldByName('Item_Name').AsString         := lDataItem.LocationName;
          FieldByName('Preferred').AsBoolean        := lDataItem.Preferred;
          FieldByName('Entered_By').AsString        := AppSettings.UserID;
          Post;
        end;
      except on E:Exception do
        raise ELocationError.Create(Format(ResStr_AddFail, ['LOCATION_NAME']), E);
      end;
  finally
    lqryAddition.Free;
  end;
end;  // DoAdditions

//------------------------------------------------------------------------------
{ Override this so we can perform additional work required to update the
     boundaries on the map }
procedure TLocationNameList.DoModifications;
begin
  inherited DoModifications;
end;

//------------------------------------------------------------------------------
procedure TLocationNameList.ProcessUpdate;
var qryDel:TJnccQuery;
begin
  DoAdditions;
  DoModifications;
  qryDel:=TJnccQuery.Create(nil);
  try
    dmDatabase.SetDatabaseLocal([qryDel]);
    DeleteFromTable(qryDel,'Location_Name','Location_Name_Key');
  finally
    qryDel.Free;
  end;
end;  // ProcessUpdate

constructor TLocationNameList.Create(iDataSet: TDataset; const iKeyFieldName: string;
      iStrings: TStrings; iItemClass: TItemClass);
begin
  inherited;
  // allow saves to the preferred flag only if not custodian
  FOverrideCustodyControl := True;
end;

//==============================================================================
{ TDesignationItem }
//------------------------------------------------------------------------------
constructor TDesignationItem.CreateNew(AOwner: TCacheDataList);
begin
  inherited CreateNew(AOwner);
  FComment:=TMemoryStream.Create;
end;  // CreateNew

//------------------------------------------------------------------------------
destructor TDesignationItem.Destroy;
begin
  FComment.Free;
  inherited Destroy;
end;  // Destroy

//------------------------------------------------------------------------------
function TDesignationItem.GetCell(const iX: integer): string;
begin
  case iX of
    0 : Result:=SiteStatus;
    1 : Result:=Authority;
    2 : Result:=DateFrom;
    3 : Result:=DateTo;
  end;
end;  // GetCell

//------------------------------------------------------------------------------
procedure TDesignationItem.InitFromRecord(iDataset: TDataset);
begin
  with iDataSet do
    try
      FItemKey      :=FieldByName('Designation_Key').AsString;
      FSiteStatusKey:=FieldByName('Site_Status_Key').AsString;
      FSiteStatus   :=FieldByName('Short_Name').AsString;
      FRefCode      :=FieldByName('Ref_Code').AsString;
      FAuthorityKey :=FieldByName('Authority').AsString;
      FAuthority    :=dmGeneralData.GetName(FAuthorityKey);
      FDateFrom     :=FieldByName('Date_From').AsString;
      FDateTo       :=FieldByName('Date_To').AsString;
      FComment      :=TMemoryStream.Create;
			TMemoField(FieldByName('Comment')).SaveToStream(FComment);
    except on E:Exception do
      raise ELocationError.Create(ResStr_InitRecFail+' - LOCATION_DESIGNATION',E);
    end;
end;  // InitFromRecord

//------------------------------------------------------------------------------
procedure TDesignationItem.WriteToRecord(iDataset: TDataset);
begin
  with iDataSet do
    try
      FieldByName('Site_Status_Key').AsString  :=FSiteStatusKey;
      FieldByName('Ref_Code').AsString         :=FRefCode;
      FieldByName('Authority').AsString        :=FAuthorityKey;
      FieldByName('Date_From').AsString        :=FDateFrom;
      FieldByName('Date_To').AsString          :=FDateTo;
      FieldByName('Changed_By').AsString       :=AppSettings.UserID;
      FieldByName('Changed_Date').AsDateTime   :=Now;
      FComment.Position:=0;
      TMemoField(FieldByName('Comment')).LoadFromStream(FComment);
    except on E:Exception do
      raise ELocationError.Create(ResStr_WriteRecFail+' - LOCATION_DESIGNATION',E);
    end;
end;  // WriteToRecord

//------------------------------------------------------------------------------
procedure TDesignationItem.SetAuthority(const Value: string);
begin
  FAuthority := Value;
  SetModified;
end;  // SetAuthority

//------------------------------------------------------------------------------
procedure TDesignationItem.SetAuthorityKey(const Value: string);
begin
  FAuthorityKey := Value;
  SetModified;
end;  // SetAuthorityKey

//------------------------------------------------------------------------------
procedure TDesignationItem.SetDateFrom(const Value: string);
begin
  FDateFrom := Value;
  SetModified;
end;  // SetDateFrom

//------------------------------------------------------------------------------
procedure TDesignationItem.SetDateTo(const Value: string);
begin
  FDateTo := Value;
  SetModified;
end;  // SetDateTo

//------------------------------------------------------------------------------
procedure TDesignationItem.SetRefCode(const Value: string);
begin
  FRefCode := Value;
  SetModified;
end;  // SetRefCode

//------------------------------------------------------------------------------
procedure TDesignationItem.SetSiteStatus(const Value: string);
begin
  FSiteStatus := Value;
  SetModified;
end;  /// SetSiteStatus

//------------------------------------------------------------------------------
procedure TDesignationItem.SetSiteStatusKey(const Value: string);
begin
  FSiteStatusKey := Value;
  SetModified;
end;  // SetSiteStatusKey

//==============================================================================
{ TDesignationList }
function TDesignationList.ItemsDisplayName: String;
begin
  Result := ResStr_Designation;
end;

//------------------------------------------------------------------------------
procedure TDesignationList.DoAdditions;
var lqryAddition: TJnccQuery;
    lDataItem   : TDesignationItem;
    i           : integer;
begin
  if ItemsToAdd.Count = 0 then Exit;
  lqryAddition := TJnccQuery.Create(nil);
  try
    dmDatabase.SetDatabaseLocal([lqryAddition]);
    with lqryAddition do
      try
        SQL.Text := 'SELECT * FROM Location_Designation WHERE Designation_Key = '''' ';
        Open;
        for i := 0 to ItemsToAdd.Count - 1 do begin
          lDataItem := TDesignationItem(ItemsToAdd[i]);
          Append;
          FieldByName('Designation_Key').AsString :=
              dmGeneralData.GetNextKey('Location_Designation','Designation_Key');
          FieldByName('Location_Key').AsString    := LocationKey;
          FieldByName('Site_Status_Key').AsString := lDataItem.SiteStatusKey;
          FieldByName('Ref_Code').AsString        := lDataItem.RefCode;
          FieldByName('Authority').AsString       := lDataItem.AuthorityKey;
          FieldByName('Date_From').AsString       := lDataItem.DateFrom;
          FieldByName('Date_To').AsString         := lDataItem.DateTo;
          FieldByName('Entered_By').AsString      := AppSettings.UserID;
          lDataItem.Comment.Position := 0;
          TMemoField(FieldByName('Comment')).LoadFromStream(lDataItem.Comment);
          Post;
        end;
        Close;
      except on E:Exception do
        raise ELocationError.Create(Format(ResStr_AddFail, ['LOCATION_DESIGNATION']), E);
      end;
  finally
    lqryAddition.Free;
  end;
end;  // DoAdditions

//------------------------------------------------------------------------------
procedure TDesignationList.ProcessUpdate;
var qryDel:TJnccQuery;
begin
  DoAdditions;
  DoModifications;
  qryDel:=TJnccQuery.Create(nil);
  try
    dmDatabase.SetDatabaseLocal([qryDel]);
    DeleteFromTable(qryDel,'Location_Designation','Designation_Key');
  finally
    qryDel.Free;
  end;
end;  // ProcessUpdate

//==============================================================================
{ TAdminAreaItem }
//------------------------------------------------------------------------------
function TAdminAreaItem.GetCell(const iX: integer): string;
begin
  case iX of
    0: Result:= FName;
    1: Result:= FAdminAreaType;
  end;
end;  // GetCell

//------------------------------------------------------------------------------
procedure TAdminAreaItem.InitFromRecord(iDataset: TDataset);
var lTempList: TStringList;
begin
  with iDataSet do
    try
      FAdminAreaKey:=FieldByName('Admin_Area_Key').AsString;
      FItemKey     :=Fieldbyname('Location_Admin_Areas_Key').asstring; //Set the key in two places as we need ItemKey to be able to do modifications

      //Get display fields from other tables
      lTempList:= TStringList.Create;
      try
        dmGeneralData.GetRecordStrings(lTempList, 'ADMIN_AREA', FAdminAreaKey);
        if lTempList.Values['SHORT_CODE'] = '' then
          FName:= lTempList.Values['ITEM_NAME']
        else
          FName:= lTempList.Values['SHORT_CODE'] + ', ' + lTempList.Values['ITEM_NAME'];
        dmGeneralData.GetRecordStrings(lTempList, 'ADMIN_TYPE', lTempList.Values['ADMIN_TYPE_KEY']);
        FAdminAreaType:= lTempList.Values['SHORT_NAME'];
      finally
        lTempList.Free;
      end;
    except on E:Exception do
      raise ELocationError.Create(ResStr_InitRecFail+' - LOCATION_ADMIN_AREAS',E);
    end;
end;  // InitFromRecord

//------------------------------------------------------------------------------
// Doesn't seemed to be called at all, and shouldn't ever be called really
procedure TAdminAreaItem.WriteToRecord(iDataset: TDataset);
begin
//  with iDataSet do
//    try
//      FieldByName('Admin_Area_Key').AsString:= FAdminAreaKey;
//    except on Exception do
//      raise ELocationError.Create(EST_WRITE_REC_FAIL);
//    end;
end;  // WriteToRecord

//------------------------------------------------------------------------------
procedure TAdminAreaItem.SetAdminAreaKey(const Value: string);
begin
  FAdminAreaKey := Value;
  SetModified;
end;  // SetAdminAreaKey

//------------------------------------------------------------------------------
procedure TAdminAreaItem.SetAdminAreaType(const Value: String);
begin
  FAdminAreaType := Value;
  SetModified;
end;  // SetAdminAreaType

//------------------------------------------------------------------------------
procedure TAdminAreaItem.SetName(const Value: String);
begin
  FName := Value;
  SetModified;
end;  // SetName

//==============================================================================
{ TAdminAreaList }
function TAdminAreaList.ItemsDisplayName: String;
begin
  Result := ResStr_AdminArea;
end;

//------------------------------------------------------------------------------
procedure TAdminAreaList.DoAdditions;
var lDataItem   : TAdminAreaItem;
    i           : integer;
    lqryAddition: TJnccQuery;
begin
  if ItemsToAdd.Count = 0 then Exit;
  lqryAddition := TJnccQuery.Create(nil);
  try
    dmDatabase.SetDatabaseLocal([lqryAddition]);
    with lqryAddition do
      try
        SQL.Text := 'SELECT * FROM Location_Admin_Areas WHERE Location_Admin_Areas_Key = ''''';
        Open;
        for i := 0 to ItemsToAdd.Count - 1 do begin
          lDataItem := TAdminAreaItem(ItemsToAdd[i]);
          Append;
          Fieldbyname('Location_Admin_Areas_Key').AsString :=
              dmGeneralData.GetNextKey('Location_Admin_Areas','Location_Admin_Areas_Key');
          FieldByName('Admin_Area_Key').AsString:= lDataItem.FAdminAreaKey;
          FieldByName('Location_Key').AsString  := FLocationKey;
          FieldByName('Entry_Date').AsDateTime  := Now;
          FieldByName('Entered_By').AsString    := AppSettings.UserID;
          Post;
        end;
        Close;
      except on E:Exception do
        raise ELocationError.Create(Format(ResStr_AddFail, ['LOCATION_ADMIN_AREAS']), E);
      end;
  finally
    lqryAddition.Free;
  end;
end;  // DoAdditions

//------------------------------------------------------------------------------
procedure TAdminAreaList.ProcessUpdate;
var qryDel: TJnccQuery;
    lCount: Integer;
    lKeys : String;
begin
  DoAdditions;
  DoModifications;
  qryDel:=TJnccQuery.Create(nil);
  try
    dmDatabase.SetDatabaseLocal([qryDel]);
    with qryDel do begin
      if ItemsToDelete.Count > 0 then begin
        lKeys := '';
        for lCount:= 0 to ItemsToDelete.Count-1 do
          lKeys :=lKeys + ',''' + TAdminAreaItem(ItemsToDelete[lCount]).AdminAreaKey + '''';
        lKeys[1] := ' '; // Remove first ','
        SQL.Text := Format('DELETE FROM Location_Admin_Areas WHERE Admin_Area_Key IN (%s) ' +
                           'AND Location_Key = ''%s''', [lKeys, FLocationKey]);
        ExecSQL;
      end; // if ItemsToDelete.Count>0
    end;
  finally
    qryDel.Free;
  end;
end;  // ProcessUpdate

//------------------------------------------------------------------------------
procedure TAdminAreaList.SetLocationKey(const Value: string);
begin
  FLocationKey := Value;
end;  // SetLocationKey

//==============================================================================
{ TGridSquareList }
function TGridSquareList.ItemsDisplayName: String;
begin
  Result := ResStr_GridSquare;
end;

//------------------------------------------------------------------------------
procedure TGridSquareList.DoAdditions;
var lDataItem   : TGridSquareItem;
    i           : integer;
    lqryAddition: TJnccQuery;
begin
  if ItemsToAdd.Count = 0 then Exit;
  lqryAddition := TJnccQuery.Create(nil);
  try
    dmDatabase.SetDatabaseLocal([lqryAddition]);
    with lqryAddition do
      try
        SQL.Text := 'SELECT * FROM Grid_Square WHERE Grid_Square_Key = ''''';
        Open;
        for i := 0 to ItemsToAdd.Count - 1 do begin
          lDataItem:=TGridSquareItem(ItemsToAdd[i]);
          Append;
          FieldByName('Grid_Square_Key').AsString   :=
              dmGeneralData.GetNextKey('Grid_Square','Grid_Square_Key');
          FieldByName('Spatial_Ref').AsString       := lDataItem.EnteredRef;
          FieldByName('Spatial_Ref_System').AsString:= lDataItem.EnteredRefSystem;
          FieldByName('Location_Key').AsString      := LocationKey;
          FieldByName('Size').AsInteger             := lDataItem.SpatialRefSize;
          FieldByName('Entered_By').AsString        := AppSettings.UserID;
          Post;
        end;
        Close;
      except on E:Exception do
        raise ELocationError.Create(Format(ResStr_AddFail, ['GRID_SQUARE']), E);
      end;
  finally
    lqryAddition.Free;
  end;
end;  // DoAdditions

//------------------------------------------------------------------------------
procedure TGridSquareList.ProcessUpdate;
var qryDel: TJnccQuery;
    lCount: Integer;
    lKeys : String;
begin
  DoAdditions;
  DoModifications;
  qryDel:=TJnccQuery.Create(nil);
  try
    dmDatabase.SetDatabaseLocal([qryDel]);
    with qryDel do begin
      if ItemsToDelete.Count > 0 then begin
        lKeys := '';
        for lCount:= 0 to ItemsToDelete.Count-1 do
          lKeys := lKeys + ',''' + TGridSquareItem(ItemsToDelete[lCount]).ItemKey+ '''';
        lKeys[1] := ' ';
        SQL.Text := Format('DELETE FROM Grid_Square WHERE Grid_Square_Key IN (%s) ' +
                           'AND LOCATION_KEY = ''%s''', [lKeys, FLocationKey]);
        ExecSQL;
      end;
    end;
  finally
    qryDel.Free;
  end;
end;  // ProcessUpdate

//------------------------------------------------------------------------------
procedure TGridSquareList.SetLocationKey(const Value: string);
begin
  FLocationKey := Value;
end;  // SetLocationKey

//==============================================================================
{ TLandParcelItem }
//------------------------------------------------------------------------------
function TLandParcelItem.GetCell(const iX: integer): string;
begin
  case iX of
    0: Result:= FNumber;
    1: Result:= FMapSystem;
  end;
end;  // GetCell

//------------------------------------------------------------------------------
procedure TLandParcelItem.InitFromRecord(iDataset: TDataset);
begin
  with iDataSet do
    try
      FNumber   :=FieldByName('Land_Parcel_Number').AsString;
      FItemKey  :=Fieldbyname('Land_Parcel_Key').asstring;
      FMapSystem:=FieldByName('Land_Parcel_Map_Sheet').AsString;
    except on E:Exception do
      raise ELocationError.Create(ResStr_InitRecFail+' - LAND_PARCEL',E);
    end;
end;  // InitFromRecord

//------------------------------------------------------------------------------
procedure TLandParcelItem.WriteToRecord(iDataset: TDataset);
begin
  with iDataSet do
    try
      FieldByName('Land_Parcel_Number').AsString   := FNumber;
      FieldByName('Land_Parcel_Map_Sheet').AsString:= FMapSystem;
      FieldByName('Changed_By').AsString           := AppSettings.UserID;
      FieldByName('Changed_Date').AsDateTime       := Now;
    except on E:Exception do
      raise ELocationError.Create(ResStr_WriteRecFail+' - LAND_PARCEL',E);
    end;
end;  // WriteToRecord

//------------------------------------------------------------------------------
procedure TLandParcelItem.SetMapSystem(const Value: String);
begin
  FMapSystem := Value;
  SetModified;
end;  // SetMapSystem

//------------------------------------------------------------------------------
procedure TLandParcelItem.SetNumber(const Value: String);
begin
  FNumber := Value;
  SetModified;
end;  // SetNumber

//==============================================================================
{ TLandParcelList }
function TLandParcelList.ItemsDisplayName: String;
begin
  Result := 'Land Parcel';
end;

//------------------------------------------------------------------------------
procedure TLandParcelList.DoAdditions;
var lDataItem   : TLandParcelItem;
    i           : integer;
    lqryAddition: TJnccQuery;
begin
  if ItemsToAdd.Count = 0 then Exit;
  lqryAddition := TJnccQuery.Create(nil);
  try
    dmDatabase.SetDatabaseLocal([lqryAddition]);
    with lqryAddition do
      try
        SQL.Text := 'SELECT * FROM Land_Parcel WHERE Land_Parcel_Key = ''''';
        Open;
        for i := 0 to ItemsToAdd.Count - 1 do
        begin
          lDataItem:=TLandParcelItem(ItemsToAdd[i]);
          Append;
          Fieldbyname('Land_Parcel_Key').AsString      :=
              dmGeneralData.GetNextKey('Land_Parcel','Land_Parcel_Key');
          FieldByName('Location_Key').AsString         := FLocationKey;
          FieldByName('Land_Parcel_Number').AsString   := lDataItem.Number;
          FieldByName('Land_Parcel_Map_Sheet').AsString:= lDataItem.MapSystem;
          FieldByName('Entered_By').AsString           := AppSettings.UserID;
          Post;
        end;
        Close;
      except on E:Exception do
        raise ELocationError.Create(Format(ResStr_AddFail, ['LAND_PARCEL']), E);
      end;
  finally
    lqryAddition.Free;
  end;
end;  // DoAdditions

//------------------------------------------------------------------------------
procedure TLandParcelList.ProcessUpdate;
var qryDel: TJnccQuery;
    lCount: Integer;
    lKeys : String;
begin
  DoAdditions;
  DoModifications;
  qryDel:=TJnccQuery.Create(nil);
  try
    dmDatabase.SetDatabaseLocal([qryDel]);
    with qryDel do begin
      if ItemsToDelete.Count > 0 then begin
        lKeys := '';
        for lCount:= 0 to ItemsToDelete.Count-1 do
          lKeys := lKeys + ',''' + TGridSquareItem(ItemsToDelete[lCount]).ItemKey+ '''';
        lKeys[1] := ' ';
        SQL.Text := Format('DELETE FROM Land_Parcel WHERE Land_Parcel_Key IN (%s) ' +
                           'AND LOCATION_KEY = ''%s''', [lKeys, FLocationKey]);
        ExecSQL;
      end; // if count
    end;
  finally
    qryDel.Free;
  end;
end;  // ProcessUpdate

//------------------------------------------------------------------------------
procedure TLandParcelList.SetLocationKey(const Value: string);
begin
  FLocationKey := Value;
end;  // SetLocationKey

//==============================================================================
{ TBoundaryItem }
//------------------------------------------------------------------------------
function TBoundaryItem.GetCell(const iX: integer): string;
begin
  case iX of
    0: Result:= IntToStr(FVersion);
    1: Result:= FFromDate;
    2: Result:= FToDate;
  end;
end;  // GetCell

//------------------------------------------------------------------------------
procedure TBoundaryItem.InitFromRecord(iDataset: TDataset);
begin
  with iDataSet do
    try
      FItemKey     := FieldByName('Location_Boundary_Key').AsString;
      FFromDate    := FieldByName('From_Vague_Date_Start').Text;
      FToDate      := FieldByName('To_Vague_Date_Start').Text;
      FVersion     := FieldByName('Version').AsInteger;
      if FieldByName('External_FileName').IsNull then begin
        FMapSheet  := TMapSheet.CreateInternal('', FieldByName('Map_Sheet_Key').AsString, '');
        FObjectID  := FieldByName('Object_ID').AsInteger;
      end else begin
        FMapSheet  := TMapSheet.CreateExternal(FieldByName('External_FileName').AsString,
                                               FieldByName('External_FileName_KeyField').AsString);
        FObjectKey := FieldByName('Object_ID').AsString;
      end;

      FOriginalMapSheetKey := MapSheetKey;
      FOriginalObjectID    := FObjectID;
    except on E:Exception do
      raise ELocationError.Create(ResStr_InitRecFail+' - LOCATION_BOUNDARY',E);
    end;
end;  // InitFromRecord

//------------------------------------------------------------------------------
procedure TBoundaryItem.WriteToRecord(iDataset: TDataset);
begin
  with iDataSet do
    try
      { Unlink old boundary }
      if FOriginalMapSheetKey <> '' then
        TBoundaryList(OwnerList).LinkObjectToBoundary(FOriginalMapSheetKey, FOriginalObjectID, '', '');
      FieldByName('From_Vague_Date_Start').Text:= FFromDate;
      FieldByName('To_Vague_Date_Start').Text  := FToDate;
      FieldByName('Version').AsInteger         := FVersion;
      if Assigned(FMapSheet) then
        if FMapSheet.IsInternal then begin
          FieldByName('Map_Sheet_Key').AsString := FMapSheet.MapSheetKey;
          FieldByName('External_Filename').Clear;
          FieldByName('External_Filename_KeyField').Clear;
          FieldByName('Object_ID').AsInteger := FObjectID;
        end else begin
          FieldByName('Map_Sheet_Key').Clear;
          FieldByName('External_Filename').AsString := FMapSheet.FileName;
          FieldByName('External_Filename_KeyField').AsString := FMapSheet.IDField;
          FieldByName('Object_ID').AsString := FObjectKey;
        end;
      FieldByName('Changed_By').AsString     := AppSettings.UserID;
      FieldByName('Changed_Date').AsDateTime := Now;
      { Link new boundary in, FMapSheetKey should have a value if we get here. }
      if MapSheetKey <> '' then
        TBoundaryList(OwnerList).LinkObjectToBoundary(MapSheetKey, FObjectID,
            FieldByName('Location_Key').AsString,
            FieldByName('Location_Boundary_Key').AsString);
    except on E:Exception do
      raise ELocationError.Create(ResStr_WriteRecFail+' - LOCATION_BOUNDARY',E);
    end;
end;  // WriteToRecord

//------------------------------------------------------------------------------
procedure TBoundaryItem.SetFromDate(const Value: String);
begin
  FFromDate := Value;
  SetModified;
end;  // SetFromDate

//------------------------------------------------------------------------------
procedure TBoundaryItem.SetToDate(const Value: String);
begin
  FToDate := Value;
  SetModified;
end;  // SetToDate

//------------------------------------------------------------------------------
procedure TBoundaryItem.SetMapSheetKey(const Value: String);
begin
  FMapSheet.MapSheetKey := Value;
  SetModified;
end;  // SetMapFile    

//------------------------------------------------------------------------------
function TBoundaryItem.GetMapSheetKey: String;
begin
  if Assigned(FMapSheet) then
    Result := FMapSheet.MapSheetKey
  else
    Result := '';
end;  // SetMapFile

//------------------------------------------------------------------------------
procedure TBoundaryItem.SetObjectID(const Value: Integer);
begin
  FObjectID := Value;
  SetModified;
end;  // SetObjectID

//------------------------------------------------------------------------------
procedure TBoundaryItem.SetVersion(const Value: Integer);
begin
  FVersion := Value;
  SetModified;
end;  // SetVersion

//------------------------------------------------------------------------------
procedure TBoundaryItem.SetMapSheet(const Value: TMapSheet);
begin
  FMapSheet := Value;
  SetModified;
end;  // SetVersion


//==============================================================================
{ TBoundaryList }
function TBoundaryList.ItemsDisplayName: String;
begin
  Result := 'Boundary';
end;

//------------------------------------------------------------------------------
procedure TBoundaryList.DoAdditions;
var lDataItem   : TBoundaryItem;
    i           : integer;
    lqryAddition: TJnccQuery;
    lBoundaryKey: string;
begin
  if ItemsToAdd.Count = 0 then Exit;
  lqryAddition := TJnccQuery.Create(nil);
  try
    dmDatabase.SetDatabaseLocal([lqryAddition]);
    with lqryAddition do
      try
        SQL.Text := 'SELECT * FROM Location_Boundary WHERE Location_Boundary_Key = ''''';
        Open;
        for i := 0 to ItemsToAdd.Count - 1 do begin
          lDataItem := TBoundaryItem(ItemsToAdd[i]);
          Append;
          lBoundaryKey := dmGeneralData.GetNextKey('LOCATION_BOUNDARY','Location_Boundary_Key');
          FieldByName('Location_Boundary_Key').AsString := lBoundaryKey;
          FieldByName('Location_Key').AsString          := FLocationKey;
          FieldByName('From_Vague_Date_Start').Text     := lDataItem.FromDate;
          FieldByName('To_Vague_Date_Start').Text       := lDataItem.ToDate;
          FieldByName('Version').AsInteger              := lDataItem.Version;
          if Assigned(lDataItem.MapSheet) then
            if lDataItem.MapSheet.IsInternal then begin
              FieldByName('Map_Sheet_Key').AsString    := lDataItem.MapSheet.MapSheetKey;
              FieldByName('External_Filename').Clear;
              FieldByName('External_Filename_KeyField').Clear;
              FieldByName('Object_ID').AsInteger       := lDataItem.ObjectID;
            end else begin
              FieldByName('Map_Sheet_Key').Clear;
              FieldByName('External_Filename').AsString          := lDataItem.MapSheet.FileName;
              FieldByName('External_Filename_KeyField').AsString := lDataItem.MapSheet.IDField;
              FieldByName('Object_ID').AsString                  := lDataItem.ObjectKey;
            end;
          FieldByName('Entered_By').AsString   := AppSettings.UserID;
          FieldByName('Entry_Date').AsDateTime := Now;
          Post;
          if lDataItem.MapSheetKey <> '' then
            LinkObjectToBoundary(lDataItem.MapSheetKey, lDataItem.ObjectID,
                                 FLocationKey, lBoundaryKey);
        end;
        Close;
      except on E:Exception do
        raise ELocationError.Create(Format(ResStr_AddFail, ['LOCATION_BOUNDARY']), E);
      end;
  finally
    lqryAddition.Free;
  end;
end;  // DoAdditions

//------------------------------------------------------------------------------
procedure TBoundaryList.LinkObjectToBoundary(const AMapSheetKey: string; AStaticID: Integer;
  const ALocationKey, ABoundaryKey: string);
var
  lMapServerLink: TMapServerLink;
begin
  // Get a MapServerLink object
  lMapServerLink := TMapServerLink.Create(nil);
  try
    // Get the BaseMapKey to know which Map dataset to update.
    with dmDatabase.ExecuteSQL('SELECT Base_Map_Key FROM Map_Sheet WHERE Map_Sheet_Key = ''' +
                               AMapSheetKey + '''', True) do
    begin
      if RecordCount>0 then begin // is there a linked map sheet?
        // Activate the correct Map dataset
        lMapServerLink.ActiveDataset := Fields['Base_Map_Key'].Value + '.gds';
        // And update the appropriate map sheet.
        lMapServerLink.LinkObjectToBoundary(AMapSheetKey, AStaticID, ALocationKey, ABoundaryKey);
        Close;
      end;
    end;
  finally
    lMapServerLink.Free;
  end;
end;

//------------------------------------------------------------------------------
procedure TBoundaryList.ProcessUpdate;
var qryDel:TJnccQuery;
    i : integer;
begin
  DoAdditions;
  DoModifications;
  qryDel:=TJnccQuery.Create(nil);
  try
    dmDatabase.SetDatabaseLocal([qryDel]);
    { Remove links from the boundaries on the map }
    if ItemsToDelete.Count>0 then
    begin
      with dmGeneralData.qryAllPurpose do
      begin
        SQL.Text := 'SELECT Map_Sheet_Key, Object_ID FROM Location_Boundary ' +
                    'WHERE Location_Boundary_Key = :Boundary_Key';
        Parameters.ParamByName('Boundary_Key').DataType := ftString;
        for i := 0 to ItemsToDelete.Count  -1 do
        begin
          Parameters.ParamByName('Boundary_Key').Value := TDataItem(ItemsToDelete[i]).ItemKey;
          Open;
          try
            {FMapServerLink.}LinkObjectToBoundary(FieldByName('Map_Sheet_Key').AsString,
                                                  FieldByName('Object_ID').AsInteger, '', '');
          finally
            Close;
          end;
        end; // for
      end; // with qryAllpurpose
      DeleteFromTable(qryDel, 'LOCATION_BOUNDARY', 'LOCATION_BOUNDARY_KEY');
    end;
  finally
    qryDel.Free;
  end;
end;  // ProcessUpdate

//------------------------------------------------------------------------------
procedure TBoundaryList.SetLocationKey(const Value: string);
begin
  FLocationKey := Value;
end;  // SetLocationKey

//==============================================================================
{ TRelationItem }
//------------------------------------------------------------------------------
function TRelationItem.GetCell(const iX: integer): string;
begin
  case iX of
    0: Result:= FLocation2Name;
    1: Result:= FRelationship;
  end;
end;  // GetCell

//------------------------------------------------------------------------------
procedure TRelationItem.InitFromRecord(iDataset: TDataset);
begin
  with iDataSet do
    try
      FItemKey      := FieldByName('Location_Relation_Key').AsString;
      FLocation2Key := FieldByName('Location_Key_2').AsString;
      FRelationship := FieldByName('Relationship').AsString;
      FLocation2Name:=dmGeneralData.GetLocationName(FLocation2Key);
    except on E:Exception do
      raise ELocationError.Create(ResStr_InitRecFail+' - LOCATION_RELATION',E);
    end;
end;  // InitFromRecord

//------------------------------------------------------------------------------
procedure TRelationItem.WriteToRecord(iDataset: TDataset);
begin
  with iDataSet do
    try
      FieldByName('Location_Key_2').AsString:= FLocation2Key;
      FieldByName('Relationship').AsString  := FRelationship;
      FieldByName('Changed_Date').AsDateTime:= Now;
      FieldByName('Changed_By').AsString    := AppSettings.UserId;
    except on E:Exception do
      raise ELocationError.Create(ResStr_WriteRecFail+' - LOCATION_RELATION',E);
    end;
end;  // WriteToRecord

//------------------------------------------------------------------------------
procedure TRelationItem.SetLocation2Key(const Value: string);
begin
  FLocation2Key := Value;
  SetModified;
end;  // SetLocation2Key

//------------------------------------------------------------------------------
procedure TRelationItem.SetLocation2Name(const Value: String);
begin
  FLocation2Name := Value;
  SetModified;
end;  // SetLocation2Name

//------------------------------------------------------------------------------
procedure TRelationItem.SetRelationship(const Value: String);
begin
  FRelationship := Value;
  SetModified;
end;  // SetRelationship

//==============================================================================
{ TRelationList }
function TRelationList.ItemsDisplayName: String;
begin
  Result := ResStr_LocRelation;
end;

//------------------------------------------------------------------------------
procedure TRelationList.DoAdditions;
var lDataItem   : TRelationItem;
    i           : integer;
    lqryAddition: TJnccQuery;
begin
  if ItemsToAdd.Count = 0 then Exit;
  lqryAddition := TJnccQuery.Create(nil);
  try
    dmDatabase.SetDatabaseLocal([lqryAddition]);
    with lqryAddition do
      try
        SQL.Text := 'SELECT * FROM Location_Relation WHERE Location_Relation_Key = ''''';
        Open;
        for i := 0 to ItemsToAdd.Count - 1 do begin
          lDataItem := TRelationItem(ItemsToAdd[i]);
          Append;
          FieldByName('Location_Relation_Key').AsString :=
              dmGeneralData.GetNextKey('LOCATION_RELATION','Location_Relation_Key');
          FieldByName('Location_Key_1').AsString        := FLocationKey;
          FieldByName('Location_Key_2').AsString        := lDataItem.Location2Key;
          FieldByName('Relationship').AsString          := lDataItem.Relationship;
          FieldByName('Entered_By').AsString            := AppSettings.UserID;
          Fieldbyname('System_Supplied_Data').AsBoolean := false;
          Post;
        end;
        Close;
      except on E:Exception do
        raise ELocationError.Create(Format(ResStr_AddFail, ['LOCATION_RELATION']), E);
      end;
  finally
    lqryAddition.Free;
  end;
end;  // DoAdditions

//------------------------------------------------------------------------------
procedure TRelationList.ProcessUpdate;
var qryDel:TJnccQuery;
begin
  DoAdditions;
  DoModifications;
  qryDel:=TJnccQuery.Create(nil);
  try
    dmDatabase.SetDatabaseLocal([qryDel]);
    DeleteFromTable(qryDel, 'LOCATION_RELATION', 'LOCATION_RELATION_KEY');
  finally
    qryDel.Free;
  end;
end;  // ProcessUpdate

//------------------------------------------------------------------------------
procedure TRelationList.SetLocationKey(const Value: string);
begin
  FLocationKey := Value;
end;  // SetLocationKey

//==============================================================================
{ TUseItem }
//------------------------------------------------------------------------------
constructor TUseItem.CreateNew(aOwner: TCacheDataList);
begin
  inherited CreateNew(aOwner);
  FComment:= TMemoryStream.Create;
  FPotential:= TMemoryStream.Create;
end;  // CreateNew

//------------------------------------------------------------------------------
function TUseItem.GetCell(const iX: integer): string;
begin
  case iX of
    0: Result:= FUse;
    1: Result:= FFromDate;
    2: Result:= FToDate;
  end;
end;  // GetCell

//------------------------------------------------------------------------------
procedure TUseItem.InitFromRecord(iDataset: TDataset);
begin
  with iDataSet do
    try
      FItemKey:=FieldByName('Location_Use_Key').AsString;
      FUse    :=FieldByName('Location_Use').AsString;

      //Potential - rich edit
      FPotential:= TMemoryStream.Create;
      TMemoField(FieldByName('Potential')).SaveToStream(FPotential);

      //Comment - rich edit
      FComment:= TMemoryStream.Create;
      TMemoField(FieldByName('Comment')).SaveToStream(FComment);

      FFromDate:=FieldByName('From_Vague_Date_Start').Text;
      FToDate  :=FieldByName('To_Vague_Date_Start').Text;
    except on E:Exception do
      raise ELocationError.Create(ResStr_InitRecFail+' - LOCATION_USE',E);
    end;
end;  // InitFromRecord

//------------------------------------------------------------------------------
procedure TUseItem.WriteToRecord(iDataset: TDataset);
begin
  with iDataSet do
    try
      FieldByName('Location_Use').AsString:= FUse;
      //Comment - rich edit
      FComment.Position:= 0;
      TMemoField(FieldByName('Comment')).LoadFromStream(FComment);
      //Potential - rich edit
      FPotential.Position:= 0;
      TMemoField(FieldByName('Potential')).LoadFromStream(FPotential);

      FieldByName('From_Vague_Date_Start').Text:= FFromDate;
      FieldByName('To_Vague_Date_Start').Text  := FToDate;
      FieldByName('Changed_Date').AsDateTime   := Now;
      FieldByName('Changed_By').AsString       := AppSettings.UserId;
    except on E:Exception do
      raise ELocationError.Create(ResStr_WriteRecFail+' - LOCATION_USE',E);
    end;
end;  // WriteToRecord

//------------------------------------------------------------------------------
procedure TUseItem.SetFromDate(const Value: String);
begin
  FFromDate := Value;
  SetModified;
end;  // SetFromDate

//------------------------------------------------------------------------------
procedure TUseItem.SetToDate(const Value: String);
begin
  FToDate := Value;
  SetModified;
end;  // SetToDate

//------------------------------------------------------------------------------
procedure TUseItem.SetUse(const Value: String);
begin
  FUse := Value;
  SetModified;
end;  // SetUse

//==============================================================================
{ TUseList }
function TUseList.ItemsDisplayName: String;
begin
  Result := ResStr_LocationUse;
end;

//------------------------------------------------------------------------------
procedure TUseList.DoAdditions;
var lDataItem   : TUseItem;
    i           : integer;
    lqryAddition: TJnccQuery;
begin
  if ItemsToAdd.Count = 0 then Exit;
  lqryAddition := TJnccQuery.Create(nil);
  try
    dmDatabase.SetDatabaseLocal([lqryAddition]);
    with lqryAddition do
      try
        SQL.Text := 'SELECT * FROM Location_Use WHERE Location_Use_Key = ''''';
        Open;
        for i := 0 to ItemsToAdd.Count - 1 do begin
          lDataItem := TUseItem(ItemsToAdd[i]);
          Append;
          FieldByName('Location_Use_Key').AsString  :=
              dmGeneralData.GetNextKey('LOCATION_USE','LOCATION_USE_KEY');
          FieldByName('Location_Key').AsString      := FLocationKey;
          FieldByName('Location_Use').AsString      := lDataItem.Use;
          FieldByName('From_Vague_Date_Start').Text := lDataItem.FromDate;
          FieldByName('To_Vague_Date_Start').Text   := lDataItem.ToDate;
          FieldByName('Entered_By').AsString        := AppSettings.UserId;
          //Comment - rich edit
          lDataItem.Comment.Position := 0;
          TMemoField(FieldByName('Comment')).LoadFromStream(lDataItem.Comment);
          //Potential - rich edit
          lDataItem.Potential.Position := 0;
          TMemoField(FieldByName('Potential')).LoadFromStream(lDataItem.Potential);
          Post;
        end;
        Close;
      except on E:Exception do
        raise ELocationError.Create(Format(ResStr_AddFail, ['LOCATION_USE']), E);
      end;
  finally
    lqryAddition.Free;
  end;
end;  // DoAdditions

//------------------------------------------------------------------------------
procedure TUseList.ProcessUpdate;
var qryDel:TJnccQuery;
begin
  DoAdditions;
  DoModifications;
  qryDel:=TJnccQuery.Create(nil);
  try
    dmDatabase.SetDatabaseLocal([qryDel]);
    DeleteFromTable(qryDel, 'LOCATION_USE', 'LOCATION_USE_KEY');
  finally
    qryDel.Free;
  end;
end;  // ProcessUpdate

//------------------------------------------------------------------------------
procedure TUseList.SetLocationKey(const Value: string);
begin
  FLocationKey := Value;
end;  // SetLocationKey

//==============================================================================
{ TTenureItem }
//------------------------------------------------------------------------------
function TTenureItem.GetCell(const iX: integer): string;
begin
  case iX of
    0: Result:= FOwnerName;
    1: Result:= FTypeName;
    2: Result:= FFromDate;
    3: Result:= FToDate;
  end;
end;  // GetCell

//------------------------------------------------------------------------------
procedure TTenureItem.InitFromRecord(iDataset: TDataset);
var lTempList: TStringList;
begin
  with iDataSet do
    try
      FItemKey  := FieldByName('Tenure_Key').AsString;
      FOwnerKey := FieldByName('Owned_By').AsString;
      FOwnerName:= dmGeneralData.GetName(FOwnerKey);
      FTypeKey  := FieldByName('Tenure_Type_Key').AsString;

      //Get display field
      lTempList:= TStringList.Create;
      try
        dmGeneralData.GetRecordStrings(lTempList, 'TENURE_TYPE', FTypeKey);
        FTypeName:= lTempList.Values['SHORT_NAME'];
      finally
        lTempList.Free;
      end;

      FFromDate:= FieldByName('From_Vague_Date_Start').Text;
      FToDate  := FieldByName('To_Vague_Date_Start').Text;
    except on E:Exception do
      raise ELocationError.Create(ResStr_InitRecFail+' - TENURE',E);
    end;
end;  // InitFromRecord

//------------------------------------------------------------------------------
procedure TTenureItem.WriteToRecord(iDataset: TDataset);
begin
  with iDataSet do
    try
      FieldByName('Owned_By').AsString         := FOwnerKey;
      FieldByName('Tenure_Type_Key').AsString  := FTypeKey;
      FieldByName('From_Vague_Date_Start').Text:= FFromDate;
      FieldByName('To_Vague_Date_Start').Text  := FToDate;
      FieldByName('Changed_By').AsString       := AppSettings.UserID;
      FieldByName('Changed_Date').AsDateTime   := Now;
    except on E:Exception do
      raise ELocationError.Create(ResStr_WriteRecFail+' - TENURE',E);
    end;
end;  // WriteToRecord

//------------------------------------------------------------------------------
procedure TTenureItem.SetFromDate(const Value: String);
begin
  FFromDate := Value;
  SetModified;
end;  // SetFromDate

//------------------------------------------------------------------------------
procedure TTenureItem.SetOwnerKey(const Value: string);
begin
  FOwnerKey := Value;
  SetModified;
end;  // SetOwnerKey

//------------------------------------------------------------------------------
procedure TTenureItem.SetOwnerName(const Value: String);
begin
  FOwnerName := Value;
  SetModified;
end;  // SetOwnerName

//------------------------------------------------------------------------------
procedure TTenureItem.SetToDate(const Value: String);
begin
  FToDate := Value;
  SetModified;
end;  // SetToDate

//------------------------------------------------------------------------------
procedure TTenureItem.SetTypeKey(const Value: string);
begin
  FTypeKey := Value;
  SetModified;
end;  // SetTypeKey

//------------------------------------------------------------------------------
procedure TTenureItem.SetTypeName(const Value: String);
begin
  FTypeName := Value;
  SetModified;
end;  // SetTypeName

//==============================================================================
{ TTenureList }
function TTenureList.ItemsDisplayName: String;
begin
  Result := ResStr_Tenure;
end;

//------------------------------------------------------------------------------
procedure TTenureList.DoAdditions;
var lDataItem   : TTenureItem;
    i           : integer;
    lqryAddition: TJnccQuery;
begin
  if ItemsToAdd.Count = 0 then Exit;
  lqryAddition := TJnccQuery.Create(nil);
  try
    dmDatabase.SetDatabaseLocal([lqryAddition]);
    with lqryAddition do
      try
        SQL.Text := 'SELECT * FROM Tenure WHERE Tenure_Key = ''''';
        Open;
        for i := 0 to ItemsToAdd.Count - 1 do begin
          lDataItem := TTenureItem(ItemsToAdd[i]);
          Append;
          FieldByName('Tenure_Key').AsString        :=
              dmGeneralData.GetNextKey('TENURE','Tenure_Key');
          FieldByName('Location_Key').AsString      := FLocationKey;
          FieldByName('Owned_By').AsString          := lDataItem.OwnerKey;
          FieldByName('Tenure_Type_Key').AsString   := lDataItem.TypeKey;
          FieldByName('From_Vague_Date_Start').Text := lDataItem.FromDate;
          FieldByName('To_Vague_Date_Start').Text   := lDataItem.ToDate;
          FieldByName('Entered_By').AsString        := AppSettings.UserID;
          Post;
        end;
        Close;
      except on E:Exception do
        raise ELocationError.Create(Format(ResStr_AddFail, ['TENURE']), E);
      end;
  finally
    lqryAddition.Free;
  end;
end;  // DoAdditions

//------------------------------------------------------------------------------
procedure TTenureList.ProcessUpdate;
var qryDel:TJnccQuery;
begin
  DoAdditions;
  DoModifications;
  qryDel:=TJnccQuery.Create(nil);
  try
    dmDatabase.SetDatabaseLocal([qryDel]);
    DeleteFromTable(qryDel, 'TENURE', 'TENURE_KEY');
  finally
    qryDel.Free;
  end;
end;  // ProcessUpdate

//------------------------------------------------------------------------------
procedure TTenureList.SetLocationKey(const Value: string);
begin
  FLocationKey := Value;
end;  // SetLocationKey

end.
   