unit GridSquareItem;

interface

uses
  DataClasses, Db, SysUtils, Constants;

type
  EGridSquareError = class(Exception);

  TGridSquareInfo = class
  private
    FSize: Integer;
    FSizeName: String;
    FSpatialRefSystem: String;
  public
    constructor Create(const ASpatialRefSystem: String; ASize: Integer; const ASizeName: String);
    property Size: Integer read FSize;
    property SizeName: String read FSizeName;
    property SpatialRefSystem: String read FSpatialRefSystem;
  end;

  TGridSquareItem = class(TGridDataItem)
  private
    FSpatialRefSize: Integer;
    FSpatialRefSizeName: string;
    FSpatialRef: String;
    FEnteredRef: String;
    FSpatialRefSystem: string;
    FEnteredRefSystem: string;
    FLocationKey : TKeyString;
    FUserID : TKeyString;

    procedure SetSpatialRef(const Value: String);
    procedure SetEnteredRef(const Value: String);
    procedure SetSpatialRefSize(Value: Integer);
    procedure SetSpatialRefSizeName(const Value: string);
    procedure SetSpatialRefSystem(const Value: String);
    procedure SetEnteredRefSystem(const Value: String);
    procedure SetLocationKey(const Value: TKeyString);
    procedure SetUserId(const Value: TKeyString);
  protected
    procedure WriteToRecord( iDataset : TDataset ); override;
    function GetCell( const iX : integer ): string; override;
  public
    procedure InitFromRecord( iDataset : TDataset ); override;
    property SpatialRef: String read FSpatialRef write SetSpatialRef;
    property EnteredRef: String read FEnteredRef write SetEnteredRef;
    property SpatialRefSize: Integer read FSpatialRefSize write SetSpatialRefSize;
    property SpatialRefSizeName: string read FSpatialRefSizeName write SetSpatialRefSizeName;
    property SpatialRefSystem: string read FSpatialRefSystem write SetSpatialRefSystem;
    property EnteredRefSystem: string read FEnteredRefSystem write SetEnteredRefSystem;
    property LocationKey: TKeyString read FLocationKey write SetLocationKey;
    property UserId: TKeyString read FUserId write SetUserId;
  end;  // TGridSquareItem


//==============================================================================
implementation

uses
  SpatialRefFuncs;

//==============================================================================
{ TGridSquareItem }
function TGridSquareItem.GetCell(const iX: integer): string;
begin
  case iX of
    0 : Result := LocaliseSpatialRef(FEnteredRef);
    1 : Result := FSpatialRefSizeName;
  end;
end;  // GetCell

//==============================================================================
procedure TGridSquareItem.InitFromRecord(iDataset: TDataset);
begin
  with iDataSet do
    try
      FEnteredRef       := FieldByName('Spatial_Ref').Value;
      FItemKey          := FieldByName('Grid_Square_Key').AsString;
      FSpatialRefSize   := FieldByName('Size').AsInteger;
      FSpatialRefSystem := FieldByName('Spatial_Ref_System').AsString;
      FLocationKey      := FieldByName('Location_Key').AsString;
      FEnteredRefSystem := FieldByName('Spatial_Ref_System').AsString;
      FUserId           := FieldByName('Entered_By').AsString;
    except on E:Exception do
      raise EGridSquareError.Create(ResStr_InitRecFail + ' - GRID SQUARE');
    end;
end;  // InitFromRecord

//==============================================================================
procedure TGridSquareItem.SetEnteredRef(const Value: String);
begin
  FEnteredRef := Value;
  SetModified;
end;

//==============================================================================
procedure TGridSquareItem.SetEnteredRefSystem(const Value: String);
begin
  FEnteredRefSystem := Value;
end;

//==============================================================================
procedure TGridSquareItem.SetLocationKey(const Value: TKeyString);
begin
  FLocationKey := Value;
end;  // SetLocationKey

//==============================================================================
procedure TGridSquareItem.SetSpatialRef(const Value: String);
begin
  FSpatialRef := Value;
  SetModified;
end;  // SetSpatialRef

//==============================================================================
procedure TGridSquareItem.SetSpatialRefSize(Value: Integer);
begin
  FSpatialRefSize := Value;
  SetModified;
end;  // SetSpatialRefSize

//==============================================================================
procedure TGridSquareItem.SetSpatialRefSizeName(const Value: string);
begin
  FSpatialRefSizeName := Value;
  SetModified;
end;  // SetSpatialRefSize

//==============================================================================
procedure TGridSquareItem.SetSpatialRefSystem(const Value: String);
begin
  FSpatialRefSystem := Value;
end;  // SetSpatialRefSystem

//==============================================================================
procedure TGridSquareItem.SetUserId(const Value: TKeyString);
begin
  FUserId := Value;
end;  // SetUserID

//==============================================================================
procedure TGridSquareItem.WriteToRecord(iDataset: TDataset);
begin
  with iDataSet do
    try
      FieldByName('Spatial_Ref').Value           := FEnteredRef;
      FieldByName('Spatial_Ref_System').AsString := FEnteredRefSystem;
      FieldByName('Size').AsInteger              := FSpatialRefSize;
      FieldByName('Changed_Date').AsDateTime     := Now;
      FieldByName('Changed_By').AsString         := FUserID;
    except on E:Exception do
      raise EGridSquareError.Create(ResStr_WriteRecFail +' - GRID SQUARE');
    end;
end;  // WriteToRecord

//==============================================================================
{ TGridSquareInfo }
constructor TGridSquareInfo.Create(
    const ASpatialRefSystem: String;
    ASize: Integer;
    const ASizeName: String);
begin
  FSpatialRefSystem := ASpatialRefSystem;
  FSize             := ASize;
  FSizeName         := ASizeName;
end;

end.
