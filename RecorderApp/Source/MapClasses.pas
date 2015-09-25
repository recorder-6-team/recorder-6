{===============================================================================
  Unit:        MapClasses

  Defines:     Classes:
                 TAvailableMap
                 TAvailableMaps
                 TMapDrawItem
                 TComAvailableMap

  Description: Various classes for map.

  Model:       <none>

  Created:     January 2004

  Last revision information:
    $Revision: 13 $
    $Date: 20/02/08 15:41 $
    $Author: Ericsalmon $

===============================================================================}

unit MapClasses;

interface

uses
  Windows, SysUtils, Graphics, Contnrs, DataClasses, SpatialRefFuncs, Forms,
  Recorder2000_TLB, ADOInt, ComObj;

type
  {-----------------------------------------------------------------------------
  }
  TAvailableMap = class
  private
    FBaseMapKey: TKeyString;
    FDisplayName: String;
    FIsDefault: Boolean;
    FSpatialSystem: String;
  public
    property BaseMapKey: TKeyString read FBaseMapKey;
    property DisplayName: String read FDisplayName;
    property IsDefault: Boolean read FIsDefault;
    property SpatialSystem: String read FSpatialSystem;
  end;

  {-----------------------------------------------------------------------------
  }
  TAvailableMaps = class
  private
    FItems: TObjectList;
    function GetCount: Integer;
    function GetItem(Index: Integer): TAvailableMap;
    function GetItemByKey(BaseMapKey: String): TAvailableMap;
    function GetDefaultMap: TAvailableMap;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Refresh;
    property Count: Integer read GetCount;
    property DefaultMap: TAvailableMap read GetDefaultMap;
    property Items[Index: Integer]: TAvailableMap read GetItem; default;
    property ItemsByKey[BaseMapKey: String]: TAvailableMap read GetItemByKey;
  end;

  {-----------------------------------------------------------------------------
  }
  TMapDrawItem = class (TObject)
  private
    FAccuracy: Integer;
    FDate: TDateTime;
    FDisplayed: Boolean;
    FKey: TKeyString;
    FLatLong: TLatLong;
    FSpatialRef: String;
    FSpatialSys: String;
    FGridSquare: TGridSquare;
    FHasGridSquare: Boolean;
    procedure SetGridSquare;
  public
    procedure InitFromInterface(AMapPoint: IMapPoint);
    procedure InitFromRecord(AFields: Fields; const AKeyField: String);
    procedure ResetGridSquare;
    property Accuracy: Integer read FAccuracy;
    property Date: TDateTime read FDate;
    property Displayed: Boolean read FDisplayed write FDisplayed;
    property Key: TKeyString read FKey;
    property LatLong: TLatLong read FLatLong;
    property SpatialRef: String read FSpatialRef;
    property SpatialSys: String read FSpatialSys;
    property GridSquare: TGridSquare read FGridSquare;
    property HasGridSquare: Boolean read FHasGridSquare;
  end;

  {-----------------------------------------------------------------------------
  }
  TCOMAvailableMap = class(TAutoObject, IAvailableMap)
  private
    FMap: TAvailableMap;
    // IAvailableMap
    function Get_IsDefault: WordBool; safecall;
    function Get_Key: WideString; safecall;
    function Get_SpatialSystem: WideString; safecall;
    function Get_Title: WideString; safecall;
    procedure Display; safecall;
    procedure DisplayDistributionPoints(const ADatasetTitle: WideString;
                                        const AMapPoints: IMapDropFormat); safecall;
  public
    constructor Create(AMap: TAvailableMap);
    property Title: WideString read Get_Title;
    property Key: WideString read Get_Key;
    property IsDefault: WordBool read Get_IsDefault;
    property SpatialSystem: WideString read Get_SpatialSystem;
  end;

//==============================================================================
implementation

uses
  DatabaseAccessADO, GeneralData, ApplicationSettings, Variants, ComServ, FormActions;

{-==============================================================================
    TMapDrawItem
===============================================================================}
{-------------------------------------------------------------------------------
}
procedure TMapDrawItem.InitFromInterface(AMapPoint: IMapPoint);
begin
  FKey := AMapPoint.RecordKey;
  dmGeneralData.ReadLatLong(AMapPoint.Lat, FLatLong.Lat);
  dmGeneralData.ReadLatLong(AMapPoint.Long, FLatLong.Long);
  FDate := AMapPoint.Date;
  FAccuracy := AMapPoint.Accuracy;
end;  // TMapDrawItem.InitFromInterface

{-------------------------------------------------------------------------------
  initialise a TempDrawItem from a record containing appropriate fields.
  Does not initialise the Displayed status.
  If Key field not specified then ignored
}
procedure TMapDrawItem.InitFromRecord(AFields: Fields; const AKeyField: String);
begin
  if AKeyField<>'' then
    FKey := AFields[AKeyField].Value
  else
    FKey := '';
  dmGeneralData.ReadLatLong(AFields['Lat'].Value, FLatLong.Lat);
  dmGeneralData.ReadLatLong(AFields['Long'].Value, FLatLong.Long);
  try
    if not VarIsNull(AFields['Vague_Date_Start'].Value) then
      FDate := VarToDateTime(AFields['Vague_Date_Start'].Value);
  except
    on EOleException do ; // ignore - date field not present
  end;
  FSpatialRef := VarToStr(AFields['Spatial_Ref'].Value);
  FSpatialSys := VarToStr(AFields['Spatial_Ref_System'].Value);
  FAccuracy := -1;

  // Associate a grid square now. Will help in drawing and find source data.
  SetGridSquare;
end;  // TMapDrawItem.InitFromRecord

{-------------------------------------------------------------------------------
  Works out the grid square equivalent in the item's Spatial Reference System.
  Sets the flag to indicate there is a grid square available.
}
procedure TMapDrawItem.SetGridSquare;
begin
  try
    FGridSquare := SpatialRefToGridSquare(FSpatialRef, FSpatialSys, -1);
    FHasGridSquare := True;
  except
    // Grid squares not supported. 
    on ESpatialRefError do
      ResetGridSquare;
  end;
end;

{-------------------------------------------------------------------------------
  Clears the content of GridSquare. Sets the flag to indicate there is no valid
  grid square info available.
}
procedure TMapDrawItem.ResetGridSquare;
begin
  FillChar(FGridSquare, SizeOf(TGridSquare), #0);
  FHasGridSquare := False;
end;

{-==============================================================================
    TAvailableMaps
===============================================================================}
{-------------------------------------------------------------------------------
}
constructor TAvailableMaps.Create;
begin
  FItems := TObjectList.Create;
end;

{-------------------------------------------------------------------------------
}
destructor TAvailableMaps.Destroy;
begin
  FItems.Free;
  inherited;
end;

{-------------------------------------------------------------------------------
}
function TAvailableMaps.GetCount: Integer;
begin
  Result := FItems.Count;
end;

{-------------------------------------------------------------------------------
}
function TAvailableMaps.GetDefaultMap: TAvailableMap;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if Items[i].IsDefault then begin
      Result := Items[i];
      Break;
    end;
end;

{-------------------------------------------------------------------------------
}
function TAvailableMaps.GetItem(Index: Integer): TAvailableMap;
begin
  // Leave TObjectList raise exception if invalid index.
  Result := TAvailableMap(FItems[Index])
end;

{-------------------------------------------------------------------------------
}
function TAvailableMaps.GetItemByKey(BaseMapKey: String): TAvailableMap;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if Items[i].BaseMapKey = BaseMapKey then begin
      Result := Items[i];
      Break;
    end;
end;

{-------------------------------------------------------------------------------
}
procedure TAvailableMaps.Refresh;
begin
  FItems.Clear;
  with dmDatabase.ExecuteSQL(
      'SELECT BM.Base_Map_Key, CM.Default_Map, BM.Spatial_System, ' +
      'ISNULL(MS.Dataset_Sheet_Name, BM.Display_Name) AS Display_Name ' +
      'FROM Base_Map BM INNER JOIN Computer_Map CM ON CM.Base_Map_Key = BM.Base_Map_Key ' +
      'AND CM.Computer_ID = Host_Name() LEFT JOIN Map_Sheet MS ' +
      'ON MS.Base_Map_Key = BM.Base_Map_Key AND MS.Computer_ID = Host_Name() ' +
      'AND MS.Sheet_Type = 0', True) do
  begin
    while not Eof do begin
      // Only add the base map if the file also physically exists, not just in database.
      if FileExists(AppSettings.MapFilePath + Fields['Base_Map_Key'].Value + '.gds') then
        with Items[FItems.Add(TAvailableMap.Create)] do begin
          FBaseMapKey    := Fields['Base_Map_Key'].Value;
          FIsDefault     := Fields['Default_Map'].Value;
          FDisplayName   := Fields['Display_Name'].Value;
          FSpatialSystem := Fields['Spatial_System'].Value;
        end;
      MoveNext;
    end;
    Close;
  end;
end;

{===============================================================================
  TComAvailableMap
}
{-------------------------------------------------------------------------------
}
constructor TCOMAvailableMap.Create(AMap: TAvailableMap);
begin
  FMap := AMap;
end;

{-------------------------------------------------------------------------------
}
function TComAvailableMap.Get_IsDefault: WordBool;
begin
  Result := FMap.IsDefault;
end;  // TComAvailableMap.Get_IsDefault

{-------------------------------------------------------------------------------
}
function TComAvailableMap.Get_Key: WideString;
begin
  Result := FMap.BaseMapKey;
end;  // TComAvailableMap.Get_Key

{-------------------------------------------------------------------------------
}
function TComAvailableMap.Get_SpatialSystem: WideString;
begin
  Result := FMap.SpatialSystem;
end;  // TComAvailableMap.Get_SpatialSystem

{-------------------------------------------------------------------------------
}
function TComAvailableMap.Get_Title: WideString;
begin
  Result := FMap.DisplayName;
end;  // TComAvailableMap.Get_Title

{-------------------------------------------------------------------------------
}
procedure TComAvailableMap.Display;
var
  lMapWindow: TForm;
begin
  lMapWindow := dmFormActions.MapWindow(Key, True);
  // Allow several Map windows, but each should have different dataset.
  if Assigned(lMapWindow) then begin
    lMapWindow.BringToFront;
    lMapWindow.SetFocus;
  end;
end;  // TComAvailableMap.Display

{-------------------------------------------------------------------------------
}
procedure TCOMAvailableMap.DisplayDistributionPoints(
  const ADatasetTitle: WideString; const AMapPoints: IMapDropFormat);
begin
  Display;
  dmFormActions.MapWindow(Key, True).AddCOMDataset(ADatasetTitle, AMapPoints);
end;

initialization
  TAutoObjectFactory.Create(ComServer, TCOMAvailableMap, Class_AvailableMap,
    ciMultiInstance, tmApartment);

end.
