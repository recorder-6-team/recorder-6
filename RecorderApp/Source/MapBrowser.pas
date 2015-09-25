{===============================================================================
  Unit:        MapBrowser

  Defines:     TfrmMapBrowser

  Description: Map browser window allowing users to go to any map and also
               centre/select/zoom on any polygons.

  Model:       Map.mpb

  Created:     January 2004

  Last revision information:
    $Revision: 13 $
    $Date: 20/05/08 8:56 $
    $Author: Johnvanbreda $

===============================================================================}

unit MapBrowser;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ImageListButton, ExtCtrls, FormActions, Contnrs, DataClasses,
  LayerLegend, MapServerLink, ComCtrls, MapClasses, OnlineHelp;

type
  TfrmMapBrowser = class (TForm)
    Bevel: TBevel;
    btnCentre: TImageListButton;
    btnGo: TImageListButton;
    btnSelect: TImageListButton;
    btnZoom: TImageListButton;
    cmbLayers: TComboBox;
    cmbMaps: TComboBox;
    lblLayer: TLabel;
    lblMap: TLabel;
    lbPolygons: TListBox;
    procedure btnCentreClick(Sender: TObject);
    procedure btnGoClick(Sender: TObject);
    procedure btnSelectClick(Sender: TObject);
    procedure btnZoomClick(Sender: TObject);
    procedure cmbLayersChange(Sender: TObject);
    procedure cmbLayersDrawItem(Control: TWinControl; Index: Integer; ARect: TRect; State: 
        TOwnerDrawState);
    procedure cmbMapsChange(Sender: TObject);
    procedure cmbMapsDrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State:
        TOwnerDrawState);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure lbPolygonsClick(Sender: TObject);
    procedure lbPolygonsDrawItem(Control: TWinControl; Index: Integer; ARect: TRect; State:
        TOwnerDrawState);
  private
    FLayerLegend: TLayerLegend;
    FMapKeys: TObjectList;
    FMapServerLink: TMapServerLink;
    FPolygonItems: TObjectList;
    FRequestingMap: Boolean;
    hlpMapBrowser  : TOnlineHelp;
    function GetBaseMapKey: TKeyString;
    function GetMapSheetKey: TKeyString;
    function GetObjectID: Integer;
    procedure LoadPosition;
    procedure SavePosition;
    procedure UpdateButtons;
    procedure UpdateLayers(const ABaseMapKey: TKeyString);
    procedure UpdatePolygons(const AMapSheetKey: TKeyString; const ASheetFileName: String);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure UpdateMapWindowSelector;
    property SelectedBaseMapKey: TKeyString read GetBaseMapKey;
    property SelectedMapSheetKey: TKeyString read GetMapSheetKey;
    property SelectedObjectID: Integer read GetObjectID;
  end;


{-------------------------------------------------------------------------------
 Function to use to access the MapBrowser window. It ensures only one
 instance exists.  ForceCreate is used if the window should be created if
 it doesn't already exist.
}
function MapBrowserWindow(ForceCreate: Boolean = False): TfrmMapBrowser;

//==============================================================================
implementation

{$R *.dfm}

uses
  ApplicationSettings, DatabaseAccessADO, GeneralData, Map, Registry, MS5, Constants,
  GeneralFunctions;

const
  ST_POLYGON   = 'Polygon';
  ST_LOCATION  = 'Location';
  ST_ADMINAREA = 'AdminArea';

//==============================================================================
function MapBrowserWindow(ForceCreate: Boolean = False): TfrmMapBrowser;
var
  i: Integer;
begin
  Result := nil;

  with Application do
    for i := 0 to ComponentCount - 1 do
      if Components[i] is TfrmMapBrowser then begin
        Result := TfrmMapBrowser(Components[i]);
        Exit;
      end;
  // Get here if form was not found.
  if ForceCreate then begin
    Result := TfrmMapBrowser.Create(Application);
    Result.UpdateMapWindowSelector;
  end;
end;

//==============================================================================
{-==============================================================================
    TfrmMapBrowser
===============================================================================}
{-------------------------------------------------------------------------------
}
constructor TfrmMapBrowser.Create(AOwner: TComponent);
begin
  inherited;

  FLayerLegend   := TLayerLegend.Create(nil, nil);
  FMapKeys       := TObjectList.Create;
  FMapServerLink := TMapServerLink.Create(nil);
  FPolygonItems  := TObjectList.Create;
  
  LoadPosition;

  //Help Setup
  hlpMapBrowser := TOnlineHelp.Create(Self.Handle);
  OnHelp := hlpMapBrowser.OnHelpReplacement;
  Self.HelpContext:=IDH_MAPNAVIGATOR;
end;  // TfrmMapBrowser.Create 

{-------------------------------------------------------------------------------
}
destructor TfrmMapBrowser.Destroy;
begin
  SavePosition;
  FLayerLegend.Free;
  FLayerLegend := nil;
  FMapKeys.Free;
  FMapKeys := nil;
  FMapServerLink.Free;
  FMapServerLink := nil;
  FPolygonItems.Free;
  FPolygonItems := nil;

  inherited;
end;  // TfrmMapBrowser.Destroy 

{-------------------------------------------------------------------------------
  Centres the selected map on the selected object. No zoom or selection.  Ensures the map is 
      displayed. 
}
procedure TfrmMapBrowser.btnCentreClick(Sender: TObject);
var
  lMapWindow: TfrmMap;
begin
  FRequestingMap := True;
  try
    lMapWindow := dmFormActions.MapWindow(SelectedBaseMapKey, True);
    if Assigned(lMapWindow) then
      lMapWindow.CentreOnObject(SelectedMapSheetKey, SelectedObjectID, '', False, False);
  finally
    FRequestingMap := False;
  end;
end;  // TfrmMapBrowser.btnCentreClick 

{-------------------------------------------------------------------------------
  Display the selected map. 
}
procedure TfrmMapBrowser.btnGoClick(Sender: TObject);
var
  lMapWindow: TfrmMap;
begin
  FRequestingMap := True;
  try
    lMapWindow := dmFormActions.MapWindow(SelectedBaseMapKey, True);
    if Assigned(lMapWindow) then
      lMapWindow.SetFocus;
    Application.MainForm.SetFocus;
  finally
    FRequestingMap := False;
  end;
end;  // TfrmMapBrowser.btnGoClick

{-------------------------------------------------------------------------------
  Centres and selects the selected object on the selected map. No zoom.  Ensures the map is
      displayed.
}
procedure TfrmMapBrowser.btnSelectClick(Sender: TObject);
var
  lMapWindow: TfrmMap;
begin
  FRequestingMap := True;
  try
    lMapWindow := dmFormActions.MapWindow(SelectedBaseMapKey, True);
    if Assigned(lMapWindow) then
      lMapWindow.CentreOnObject(SelectedMapSheetKey, SelectedObjectID, '', False);
  finally
    FRequestingMap := False;
  end;
end;  // TfrmMapBrowser.btnSelectClick

{-------------------------------------------------------------------------------
  Centres and zoom the selected map on the selected object. No selection.  Ensures the map is
      displayed.
}
procedure TfrmMapBrowser.btnZoomClick(Sender: TObject);
var
  lMapWindow: TfrmMap;
begin
  FRequestingMap := True;
  try
    lMapWindow := dmFormActions.MapWindow(SelectedBaseMapKey, True);
    if Assigned(lMapWindow) then
      lMapWindow.CentreOnObject(SelectedMapSheetKey, SelectedObjectID, '', True, False);
  finally
    FRequestingMap := False;
  end;
end;  // TfrmMapBrowser.btnZoomClick 

{-------------------------------------------------------------------------------
  Trigger an update of the Polygons listbox. 
}
procedure TfrmMapBrowser.cmbLayersChange(Sender: TObject);
begin
  with TLayerItem(cmbLayers.Items.Objects[cmbLayers.ItemIndex]) do
    UpdatePolygons(MapSheetKey, FileName);
end;  // TfrmMapBrowser.cmbLayersChange 

{-------------------------------------------------------------------------------
  Draws the polygon symbol with the its own style and (unselected) colour. 
}
procedure TfrmMapBrowser.cmbLayersDrawItem(Control: TWinControl; Index: Integer; ARect: TRect; 
    State: TOwnerDrawState);
begin
  cmbLayers.Canvas.FillRect(ARect);
  with TLayerItem(cmbLayers.Items.Objects[Index]) do begin
    // Symbol is drawn in centre of specified rectangle.
    DrawSymbol(cmbLayers.Canvas,
               Rect(ARect.Left + 2, ARect.Top, ARect.Left + 18, ARect.Bottom));
    cmbLayers.Canvas.TextOut(ARect.Left + 22, ARect.Top + 2, Title);
  end;
end;  // TfrmMapBrowser.cmbLayersDrawItem 

{-------------------------------------------------------------------------------
  Triggers a refresh of the Layers combobox. 
}
procedure TfrmMapBrowser.cmbMapsChange(Sender: TObject);
begin
  with cmbMaps do
    UpdateLayers(TKeyData(Items.Objects[ItemIndex]).ItemKey);
end;  // TfrmMapBrowser.cmbMapsChange 

{-------------------------------------------------------------------------------
  Draws the Base Map 'World' image for each item. 
}
procedure TfrmMapBrowser.cmbMapsDrawItem(Control: TWinControl; Index: Integer; Rect: TRect; 
    State: TOwnerDrawState);
begin
  with cmbMaps do begin
    Canvas.FillRect(Rect);
    dmFormActions.ilMenuOn.Draw(Canvas, Rect.Left + 2, Rect.Top, 21);
    Canvas.TextOut(Rect.Left + 22, Rect.Top + 2, Items[Index]);
  end;
end;  // TfrmMapBrowser.cmbMapsDrawItem 

{-------------------------------------------------------------------------------
}
procedure TfrmMapBrowser.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;  // TfrmMapBrowser.FormClose 

{-------------------------------------------------------------------------------
}
function TfrmMapBrowser.GetBaseMapKey: TKeyString;
begin
  Result := TKeyData(cmbMaps.Items.Objects[cmbMaps.ItemIndex]).ItemKey;
end;  // TfrmMapBrowser.GetBaseMapKey 

{-------------------------------------------------------------------------------
}
function TfrmMapBrowser.GetMapSheetKey: TKeyString;
begin
  Result := TLayerItem(cmbLayers.Items.Objects[cmbLayers.ItemIndex]).MapSheetKey;
end;  // TfrmMapBrowser.GetMapSheetKey 

{-------------------------------------------------------------------------------
}
function TfrmMapBrowser.GetObjectID: Integer;
begin
  Result := StrToInt(TKeyData(lbPolygons.Items.Objects[lbPolygons.ItemIndex]).ItemKey);
end;  // TfrmMapBrowser.GetObjectID 

{-------------------------------------------------------------------------------
}
procedure TfrmMapBrowser.lbPolygonsClick(Sender: TObject);
begin
  // Just in case.
  UpdateButtons;
end;  // TfrmMapBrowser.lbPolygonsClick 

{-------------------------------------------------------------------------------
  Draws the Location image for a polygon linked to a location, the Admin Area image for a 
      polygon linked to an admin area, and the layer symbol if the polygon is unlinked. 
}
procedure TfrmMapBrowser.lbPolygonsDrawItem(Control: TWinControl; Index: Integer; ARect: TRect;
    State: TOwnerDrawState);
begin
  with lbPolygons do begin
    Canvas.FillRect(ARect);
    if not Assigned(lbPolygons.Items.Objects[Index]) then Exit;
    if TKeyData(lbPolygons.Items.Objects[Index]).ItemAdditional = ST_LOCATION then
      dmFormActions.ilMenuOn.Draw(Canvas, ARect.Left + 2, ARect.Top, 13)
    else
    if TKeyData(lbPolygons.Items.Objects[Index]).ItemAdditional = ST_ADMINAREA then
      dmFormActions.ilMenuOn.Draw(Canvas, ARect.Left + 2, ARect.Top, 20)
    else
      TLayerItem(cmbLayers.Items.Objects[cmbLayers.ItemIndex]).DrawSymbol(Canvas,
                 Rect(ARect.Left + 2, ARect.Top, ARect.Left + 18, ARect.Bottom));
    Canvas.TextOut(ARect.Left + 22, ARect.Top + 2, Items[Index]);
  end;
end;  // TfrmMapBrowser.lbPolygonsDrawItem 

{-------------------------------------------------------------------------------
}
procedure TfrmMapBrowser.LoadPosition;
var
  lRect: TRect;
begin
  with TRegistry.Create do
    try
      if OpenKey(REG_KEY_MAP_BROWSER, False) then begin
        ReadBinaryData('Position', lRect, SizeOf(TRect));
        Width  := lRect.Right;
        Height := lRect.Bottom;
        Left   := lRect.Left;
        Top    := lRect.Top;
        // Ensure Right Anchoring is correct for ssytems with different scaling
        btnGo.Left := Width - btnGo.Width - 16;
        cmbMaps.Width := btnGo.Left - cmbMaps.Left;
        cmbLayers.Width := Width - 55;
        lbPolygons.Width := Width - 23;
      end;
    finally
      CloseKey;
      Free;
    end;
end;  // TfrmMapBrowser.LoadPosition 

{-------------------------------------------------------------------------------
}
procedure TfrmMapBrowser.SavePosition;
var
  lRect: TRect;
begin
  with TRegistry.Create do
    try
      if OpenKey(REG_KEY_MAP_BROWSER, True) then begin
        lRect := Rect(Left, Top, Width, Height);
        WriteBinaryData('Position', lRect, SizeOf(TRect));
        WriteBool('Opened', Application.Terminated);
      end;
    finally
      CloseKey;
      Free;
    end;
end;  // TfrmMapBrowser.SavePosition 

{-------------------------------------------------------------------------------
}
procedure TfrmMapBrowser.UpdateButtons;
begin
  btnCentre.Enabled := lbPolygons.ItemIndex <> -1;
  btnZoom.Enabled   := btnCentre.Enabled;
  btnSelect.Enabled := btnCentre.Enabled;
end;  // TfrmMapBrowser.UpdateButtons 

{-------------------------------------------------------------------------------
  Refresh the Layers combobox to reflect any changes made to the maps. Calls UpdatePolygons to 
      update the Polygons listbox. 
}
procedure TfrmMapBrowser.UpdateLayers(const ABaseMapKey: TKeyString);
var
  lSelected: TKeyString;
  lLayer: TLayerItem;
  i: Integer;
begin
  // Clear Layers combo, but remember which one was selected.
  with cmbLayers do begin
    if ItemIndex <> -1 then
      lSelected := TLayerItem(Items.Objects[ItemIndex]).MapSheetKey;
    FLayerLegend.BaseMapKey := ABaseMapKey;
    Clear;
  end;
  
  // Repopulate
  FLayerLegend.Refresh;
  with FLayerLegend do
    for i := 0 to Count - 1 do
      if LegendItems[i] is TPolygonLayerItem then
        cmbLayers.Items.AddObject(LegendItems[i].Title, LegendItems[i]);

  with cmbLayers do begin
    // Reselect previous one, or first one if previous was removed.
    // There is always at least 1 polygon layer on each map.
    lLayer := FLayerLegend.LayerByKey(lSelected);
    if Assigned(lLayer) then ItemIndex := Items.IndexOf(lLayer.Title);
    if ItemIndex = -1 then ItemIndex := 0;
    lLayer := TLayerItem(Items.Objects[ItemIndex]);

    // Set the Map Dataset to use for the polygons.
    FMapServerLink.ActiveDataset := ABaseMapKey + '.gds';
    // Now refresh the list.
    UpdatePolygons(lLayer.MapSheetKey, lLayer.FileName);
  end;
end;  // TfrmMapBrowser.UpdateLayers 

{-------------------------------------------------------------------------------
  Refresh the maps combobox to reflect any changes made to the maps. Calls UpdateLayers to 
      update the Layers combobox. 
}
procedure TfrmMapBrowser.UpdateMapWindowSelector;
var
  lSelected: TKeyString;
  lKey: TKeyData;
  lMap: TAvailableMap;
  i: Integer;
begin
  // MapBrowser requested the MapWindow, so ignore the request to update.
  if FRequestingMap then Exit;

  // Clear Base Maps combo, but remember which one was selected.
  with cmbMaps do begin
    if ItemIndex <> -1 then
      lSelected := TKeyData(Items.Objects[ItemIndex]).ItemKey;
    // Use of ObjectList for easy clearing of the objects.
    FMapKeys.Clear;
    Clear;
  end;
  
  with AppSettings.AvailableMaps do begin
    for i := 0 to Count - 1 do begin
      lKey := TKeyData.Create;
      lKey.ItemKey := Items[i].BaseMapKey;
      FMapKeys.Add(lKey);
      cmbMaps.Items.AddObject(Items[i].DisplayName, lKey);
    end;
  end;
  
  // There is at least 1 map available, or the form would already have been closed.
  with cmbMaps do begin
    lMap := AppSettings.AvailableMaps.ItemsByKey[lSelected];
    if Assigned(lMap) then ItemIndex := Items.IndexOf(lMap.DisplayName);
    if ItemIndex = -1 then ItemIndex := 0;

    UpdateLayers(TKeyData(Items.Objects[ItemIndex]).ItemKey);
  end;
end;  // TfrmMapBrowser.UpdateMapWindowSelector 

{-------------------------------------------------------------------------------
  Update the Polygons listbox. 
}
procedure TfrmMapBrowser.UpdatePolygons(const AMapSheetKey: TKeyString;
  const ASheetFileName: String);
var
  lSheetID: Integer;
  i: Integer;
  lStaticId: Integer;
  lLinked: Byte;
  lLocationKey: TKeyString;
  lLocationName: String;
  lKey: TKeyData;
  lCursor: TCursor;
begin
  lCursor := HourglassCursor;
  try
    lbPolygons.Clear;
    // Use of ObjectList for easy clearing of the objects.
    FPolygonItems.Clear;

    // Get the Sheet Id, from the filename in map dataset.
    lSheetID := FMapServerLink.SheetIDByFileName(ASheetFileName);

    // Might be that the Dataset hasn't been opened in Map window yet and not all
    // sheets are ready to be accessed. So be careful.
    if lSheetID <> -1 then
      // Get all objects in the sheet and find out if it is linked to
      // Location Boundary, Admin Area Boundary, or is just Unlinked
      for i := 0 to FMapServerLink.ObjectTotal[lSheetID] - 1 do begin
        lStaticId := FMapServerLink.StaticIDforObject(lSheetID, i);
        lKey := TKeyData.Create;
        lKey.ItemKey := IntToStr(lStaticID);
        // Assume unlinked
        lLinked := 0;
        lLocationName := 'Unlinked Polygon [ID: ' + IntToStr(lStaticId) + ']';
        lKey.ItemAdditional := ST_POLYGON;
        // Find out if polygon linked to location.
        with dmDatabase.GetRecordset('usp_Boundary_GetLinkedLocation',
            ['@Map_Sheet_Key', AMapSheetKey, '@Static_Object_ID', lStaticId]) do
          if not Eof then begin
            lLinked := 1;
            lKey.ItemAdditional := ST_LOCATION;
            lLocationKey  := Fields['Location_Key'].Value;
            lLocationName := Fields['Item_Name'].Value;
          end;
        // Check if polygon linked to Admin Area Boundary, if not already linked
        if lLinked = 0 then
          // Only need the Admin Area name
          with dmDatabase.ExecuteSQL(Format('SELECT AA.Item_Name FROM Admin_Boundary AB ' +
                     'INNER JOIN Admin_Area AA ON AA.Admin_Area_Key = AB.Admin_Area_Key ' +
                     'WHERE AB.Map_Sheet_Key = ''%s'' AND AB.Object_Id = %d',
                     [AMapSheetKey, lStaticId]), True) do
          begin
            if not Eof then begin
              lKey.ItemAdditional := ST_ADMINAREA;
              lLocationName := Fields['Item_Name'].Value;
            end;
            Close;
          end;

        FPolygonItems.Add(lKey);
        lbPolygons.Items.AddObject(lLocationName, lKey);
      end;
  finally
    DefaultCursor(lCursor);
  end;
  // update buttons too.
  UpdateButtons;
end;  // TfrmMapBrowser.UpdatePolygons

end.

