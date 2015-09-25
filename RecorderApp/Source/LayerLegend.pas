{===============================================================================
  Unit:        LayerLegend

  Defines:     TLayerLegend
               TLayerItem

  Description:

  Model:       Map.mpb

  Created:     December 2003

  Last revision information:
    $Revision: 20 $
    $Date: 19/02/09 15:33 $
    $Author: Pauldavies $

===============================================================================}

unit LayerLegend;

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls, Forms, Dialogs, Grids,
  BaseLegend, MapData, Db, DataClasses, Constants, RegisterMap, MapServerLink,
  SpatialRefFuncs, ExceptionForm, ADODB, ADOInt, Variants;

resourcestring
  ResStr_CannotDeleteBaseMap = 'You cannot delete the base map layer.';
  ResStr_CannotDeletePolygon = 'You do not have permission to delete polygon layers.';
  ResStr_LastPolygon =  'You cannot delete the last remaining polygon layer.';
  ResStr_WriteSheetError = 'An error occurred writing the sheet title to the database';
  ResStr_UpdateSheetError = 'An error occurred updating the visibility of the sheet in the database';
  ResStr_MapSheetDeleted =  'The selected map sheet has been deleted from the database by another user';


type
  ELayerError = class (TExceptionPath)
  end;
  
  ELayerMissing = class (TExceptionPath)
  end;
  
  TLayerItem = class;

  TMoveLayerEvent = procedure (ALayer: TLayerItem; AFromPos, AToPos: Integer) of object;
  TLayerLegend = class (TBaseLegend)
  private
    FBaseMapKey: TKeyString;
    FConfirmBeforeDelete: Boolean;
    FDragging: Boolean;
    FMouseDownGrid: TPoint;
    FMoving: Boolean;
    FOnMoveLayer: TMoveLayerEvent;
    function GetLegendItems(Index: Integer): TLayerItem;
    procedure GridDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure GridDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var 
        Accept: Boolean);
    procedure GridMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: 
        Integer);
    procedure GridMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: 
        Integer);
    procedure RefreshSheetsOrder;
    procedure SetBaseMapKey(Value: TKeyString);
  protected
    procedure GridClick(Sender: TObject); override;
    procedure GridMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer); override;
    procedure RefreshData; override;
    procedure SetGrid(Value: TStringGrid); override;
  public
    constructor Create(AOwner: TComponent; AGrid: TStringGrid); override;
    function AddItem(AItem: TBaseLegendItem): Boolean; override;
    procedure AddItemFromFields(AFields: Fields);
    procedure DeleteItem(const Index: Integer); override;
    function LayerByKey(AKey: TKeyString): TLayerItem;
    procedure MoveLayer(AFromPos, AToPos: Integer);
    property BaseMapKey: TKeyString read FBaseMapKey write SetBaseMapKey;
    property ConfirmBeforeDelete: Boolean read FConfirmBeforeDelete write FConfirmBeforeDelete;
    property LegendItems[Index: Integer]: TLayerItem read GetLegendItems; default;
    property OnMoveLayer: TMoveLayerEvent read FOnMoveLayer write FOnMoveLayer;
  end;
  
  TLayerItem = class (TBaseLegendItem)
  private
    FFileName: String;
    FMapSheetKey: TKeyString;
  protected
    procedure DoAssign(ASource: TBaseLegendItem); override;
    function DoEditProperties: Boolean; override;
    procedure DoSetTitle(const Value: String); override;
    procedure DoSetVisible(const Value: Boolean); override;
    procedure InitFromFields(AFields: Fields); virtual;
  public
    property FileName: String read FFileName write FFileName;
    property MapSheetKey: TKeyString read FMapSheetKey write FMapSheetKey;
  end;
  
  TPolygonLayerItem = class (TLayerItem)
  private
    FColoursChanged: Boolean;
    FLastNumObjects: Integer;
    FPattern: TBrushStyle;
    FPatternChanged: Boolean;
    FSelectedColour: TColor;
    FUnSelectedColour: TColor;
    function GetMSPattern: ShortInt;
    procedure SetProperties(APattern: TBrushStyle; ASelectedColour, AUnselectedColour: TColor);
  protected
    procedure DoAssign(ASource: TBaseLegendItem); override;
    function DoEditProperties: Boolean; override;
    function GetCanDelete: Boolean; override;
    procedure InitFromFields(AFields: Fields); override;
    procedure ResetFlags; override;
  public
    constructor CreateNew(ABaseMapKey: TKeyString);
    procedure DrawSymbol(ACanvas: TCanvas; ARect: TRect); override;
    property ColoursChanged: Boolean read FColoursChanged;
    property LastNumObjects: Integer read FLastNumObjects write FLastNumObjects;
    property MSPattern: ShortInt read GetMSPattern;
    property Pattern: TBrushStyle read FPattern;
    property PatternChanged: Boolean read FPatternChanged;
    property SelectedColour: TColor read FSelectedColour;
    property UnSelectedColour: TColor read FUnSelectedColour;
  end;
  
  TNonDrawingLayerItem = class (TLayerItem)
  private
    procedure LoadData(ADialog: TdlgRegisterMap);
    procedure SaveData(ADialog: TdlgRegisterMap);
  protected
    function DoEditProperties: Boolean; override;
  end;
  
  TBaseMapLayerItem = class (TNonDrawingLayerItem)
  protected
    function GetAlwaysVisible: Boolean; override;
    function GetCanDelete: Boolean; override;
    function GetCanRename: Boolean; override;
  public
    procedure DrawSymbol(ACanvas: TCanvas; ARect: TRect); override;
  end;
  
  TBackgroundLayerItem = class (TNonDrawingLayerItem)
  private
    FLastFilePath: String;
  public
    constructor CreateNew(ABaseMapKey: TKeyString);
    procedure DrawSymbol(ACanvas: TCanvas; ARect: TRect); override;
  end;
  
//==============================================================================
implementation

uses
  FormActions, GeneralData, ApplicationSettings, DatabaseAccessADO,
  PolygonLayerDetails, GeneralFunctions;

resourcestring
  ResStr_ErrLoadingMapLayer = 'Error loading map layers';

{-==============================================================================
    TLayerLegend
===============================================================================}
{-------------------------------------------------------------------------------
}
constructor TLayerLegend.Create(AOwner: TComponent; AGrid: TStringGrid);
begin
  inherited;
  FConfirmBeforeDelete := True;
end;  // TLayerLegend.Create

{-------------------------------------------------------------------------------
}
function TLayerLegend.AddItem(AItem: TBaseLegendItem): Boolean;
begin
  Result := inherited AddItem(AItem);
  if Result then RefreshSheetsOrder
end;  // TLayerLegend.AddItem 

{-------------------------------------------------------------------------------
  Add a layer to the list, initialised by the current record in the supplied dataset 
}
procedure TLayerLegend.AddItemFromFields(AFields: Fields);
var
  lLayer: TLayerItem;
begin
  case AFields['Sheet_Type'].Value of
    0   : // Base map sheet
          lLayer := TBaseMapLayerItem.Create;
    1, 2: // Vector (1) or raster (2) sheet - not drawing
          lLayer := TBackgroundLayerItem.Create;
  else
    // Otherwise, Polygon layer (3)
    lLayer := TPolygonLayerItem.Create;
  end;
  lLayer.Initializing := True;  // Don't trigger any OnNeedRefresh before it's ready.
  lLayer.InitFromFields(AFields);
  lLayer.Initializing := False;
  AddItem(lLayer);  // Calls Change, eventually.
end;  // TLayerLegend.AddItemFromFields

{-------------------------------------------------------------------------------
}
procedure TLayerLegend.DeleteItem(const Index: Integer);
var
  lProceed: Boolean;
begin
  if LegendItems[Index] is TBaseMapLayerItem then
     raise ELayerError.CreateNonCritical(ResStr_CannotDeleteBaseMap);
  
  // Must not delete the last remaining polygon layer
  if LegendItems[Index] is TPolygonLayerItem then
    if AppSettings.UserAccessLevel <= ualAddOnly then
      raise ELayerError.CreateNonCritical(ResStr_CannotDeletePolygon)
    else
      with dmDatabase.ExecuteSQL(
          'SELECT Count(*) As Total FROM Map_Sheet WHERE Base_Map_Key = ''' + BaseMapKey
          + ''' AND Sheet_Type = 3 AND Remove_Sheet = 0', True) do
        try
          if Fields['Total'].Value < 2 then
            raise ELayerError.CreateNonCritical(ResStr_LastPolygon);
        finally
          Close;
        end;
  lProceed := True;
  if ConfirmBeforeDelete then
    lProceed := ConfirmDeletionYesNo(LegendItems[Index].Title + ' layer') = mrYes;
  
  if lProceed then begin
    dmDatabase.ExecuteSQL('UPDATE Map_Sheet SET Remove_Sheet=1, Sheet_Displayed=0 ' +
                          'WHERE Map_Sheet_Key=''' + LegendItems[Index].MapSheetKey + '''');
    inherited DeleteItem(Index);
    RefreshSheetsOrder;
  end;
end;  // TLayerLegend.DeleteItem 

{-------------------------------------------------------------------------------
}
function TLayerLegend.GetLegendItems(Index: Integer): TLayerItem;
begin
  Result := TLayerItem(inherited LegendItems[Index]);
end;  // TLayerLegend.GetLegendItems 

{-------------------------------------------------------------------------------
  Ensure correct refreshing occurs when toggling sheet visibility 
}
procedure TLayerLegend.GridClick(Sender: TObject);
begin
  { Clicks are strangley invoked during a begin drag.  We don't want to treat
      this as a click, so we check that this is a genuine click event }
  if not FDragging and not FMoving then
    inherited;
end;  // TLayerLegend.GridClick 

{-------------------------------------------------------------------------------
}
procedure TLayerLegend.GridDragDrop(Sender, Source: TObject; X, Y: Integer);
var
  lCol, lRow: Integer;
begin
  inherited;
  Grid.MouseToCell(X, Y, lCol, lRow);
  MoveLayer(Grid.Row, lRow);
end;  // TLayerLegend.GridDragDrop 

{-------------------------------------------------------------------------------
}
procedure TLayerLegend.GridDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; 
    var Accept: Boolean);
begin
  inherited;
  Accept := Source = Grid;
end;  // TLayerLegend.GridDragOver 

{-------------------------------------------------------------------------------
}
procedure TLayerLegend.GridMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
    X, Y: Integer);
begin
  inherited;
  { Start dragging only when the mouse has moved a bit }
  if ssLeft in Shift then
    FMouseDownGrid := Point(X, Y);
end;  // TLayerLegend.GridMouseDown 

{-------------------------------------------------------------------------------
}
procedure TLayerLegend.GridMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  lCol, lRow: Integer;
begin
  Grid.MouseToCell(X, Y, lCol, lRow);
  if ssLeft in Shift then begin
    { If dragged more than 5 pixels and not renaming cell, begin dragging }
    if ((Abs(X - FMouseDownGrid.X) > 5) or (Abs(Y - FMouseDownGrid.Y) > 5)) and
       not (goEditing in Grid.Options) then
    begin
      FDragging := True;
      Grid.BeginDrag(True);
    end;
  end else
    inherited;
end;  // TLayerLegend.GridMouseMove 

{-------------------------------------------------------------------------------
  End Drag occurs when mouse up - store this information in a flag as it is not easy to check 
      if a drag operation is in progress. 
}
procedure TLayerLegend.GridMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; 
    X, Y: Integer);
begin
  FDragging := False;
end;  // TLayerLegend.GridMouseUp 

{-------------------------------------------------------------------------------
}
function TLayerLegend.LayerByKey(AKey: TKeyString): TLayerItem;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if LegendItems[i].MapSheetKey = AKey then begin
      Result := LegendItems[i];
      Break;
    end;
end;  // TLayerLegend.LayerByKey 

{-------------------------------------------------------------------------------
  Reorder the display sequence of the layers. 
}
procedure TLayerLegend.MoveLayer(AFromPos, AToPos: Integer);
var
  lToPos: Integer;
begin
  // This flag stops a click that switches the Visibility On/Off when not required.
  FMoving := True;
  // Find out where to move the item to
  if (AToPos <> -1) and (AToPos < Count) then lToPos := AToPos
                                         else lToPos := Count - 1;
  Items.Move(AFromPos, lToPos);
  // Make sure the grid has the correct object on each row
  SynchroniseGrid;
  if Assigned(Grid) then Grid.Row := lToPos;
  
  RefreshSheetsOrder;
  if Assigned(OnMoveLayer) then OnMoveLayer(LegendItems[lToPos], AFromPos, lToPos);
  FMoving := False;
end;  // TLayerLegend.MoveLayer 

{-------------------------------------------------------------------------------
}
procedure TLayerLegend.RefreshData;
begin
  inherited RefreshData;

  with dmDatabase.ExecuteSQL('SELECT * FROM Map_Sheet WHERE Base_Map_Key = ''' + FBaseMapKey +
                             ''' AND (Computer_ID =''' + AppSettings.ComputerID +
                             ''' OR Sheet_Type=3) AND Remove_Sheet = 0 ' +
                             'ORDER BY Dataset_Sheet_Order', True) do
    try
      while not Eof do begin
        AddItemFromFields(Fields);
        MoveNext;
      end;
      Close;
    except
      on E:EDatabaseError do
      begin
        Close;
        raise ELayerError.Create(ResStr_ErrLoadingMapLayer, E);
      end;
    end; // try.. except..
end;  // TLayerLegend.RefreshData 

{-------------------------------------------------------------------------------
}
procedure TLayerLegend.RefreshSheetsOrder;
var
  i: Integer;
begin
  // Makes a nice ordered sequence with no gaps
  for i := 0 to Count - 1 do begin
    { Only make change if necessary so that sheet modification process is not
      triggered in map unless necessary }
    dmDatabase.ExecuteSQL(Format('UPDATE Map_Sheet SET Dataset_Sheet_Order=%d ' +
                                 'WHERE Map_Sheet_Key=''%s'' AND Dataset_Sheet_Order <> %d',
                                 [i, LegendItems[i].MapSheetKey, i]));
  end;
end;  // TLayerLegend.RefreshSheetsOrder 

{-------------------------------------------------------------------------------
}
procedure TLayerLegend.SetBaseMapKey(Value: TKeyString);
begin
  if FBaseMapKey <> Value then begin
    // Now setup for new BaseMap
    FBaseMapKey := Value;
    Refresh;
  end;
end;  // TLayerLegend.SetBaseMapKey 

{-------------------------------------------------------------------------------
}
procedure TLayerLegend.SetGrid(Value: TStringGrid);
begin
  if Assigned(Grid) and (Value = nil) then begin
    Grid.OnDragDrop  := nil;
    Grid.OnDragOver  := nil;
    Grid.OnMouseDown := nil;
    Grid.OnMouseUp   := nil;
  end;
  
  inherited SetGrid(Value);
  
  // Extra event handlers for drag drop }
  if Assigned(Grid) and (Appsettings.UserAccessLevel > ualAddOnly) then begin
    Grid.OnDragDrop  := GridDragDrop;
    Grid.OnDragOver  := GridDragOver;
    Grid.OnMouseDown := GridMouseDown;
    Grid.OnMouseUp   := GridMouseUp;
  end;
end;  // TLayerLegend.SetGrid 

{-==============================================================================
    TLayerItem
===============================================================================}
{-------------------------------------------------------------------------------
}
procedure TLayerItem.DoAssign(ASource: TBaseLegendItem);
begin
  if ASource is TLayerItem then begin
    FileName    := TLayerItem(ASource).FileName;
    MapSheetKey := TLayerItem(ASource).MapSheetKey;
  end;
  
  inherited;
end;  // TLayerItem.DoAssign 

{-------------------------------------------------------------------------------
}
function TLayerItem.DoEditProperties: Boolean;
begin
  CancelDrag;
  Result := True;
end;  // TLayerItem.DoEditProperties 

{-------------------------------------------------------------------------------
  Rename is different to simply updating the title property because it also updates the db 
}
procedure TLayerItem.DoSetTitle(const Value: String);
begin
  if (Value <> Title) and not Initializing then
    dmGeneralData.ExecuteSQL(Format('UPDATE Map_Sheet SET Sheet_Name=%s, ' +
                                    'Dataset_Sheet_Name=%s, Modified_Data=1 ' +
                                    'WHERE Map_Sheet_Key=''%s''',
                                    [QuotedStr(Value), QuotedStr(Value), FMapSheetKey]),
                                    ResStr_WriteSheetError);
  inherited;
end;  // TLayerItem.DoSetTitle 

{-------------------------------------------------------------------------------
}
procedure TLayerItem.DoSetVisible(const Value: Boolean);
begin
  inherited;
  if not AlwaysVisible and not Initializing then
    dmGeneralData.ExecuteSQL(
        Format('UPDATE Map_Sheet SET Sheet_Displayed=%d, Modified_Data=1, ' +
               'Changed_Date=GetDate(), Changed_By=''%s'' WHERE Map_Sheet_Key=''%s''',
               [Ord(Value), AppSettings.UserID, MapSheetKey]),
              ResStr_UpdateSheetError);
end;  // TLayerItem.DoSetVisible 

{-------------------------------------------------------------------------------
}
procedure TLayerItem.InitFromFields(AFields: Fields);
begin
  FFileName    := DatasetSheetFileName(AFields['Sheet_Type'].Value,
                                       VarToStr(AFields['Dataset_Sheet_FileName'].Value));
  FMapSheetKey := AFields['Map_Sheet_Key'].Value;
  Title        := VarToStr(AFields['Sheet_Name'].Value);
  Visible      := AFields['Sheet_Displayed'].Value;
end;  // TLayerItem.InitFromFields

{-==============================================================================
    TPolygonLayerItem
===============================================================================}
{-------------------------------------------------------------------------------
}
constructor TPolygonLayerItem.CreateNew(ABaseMapKey: TKeyString);
begin
  with TdlgPolygonLayerDetails.Create(nil) do
    try
      if ShowModal = mrOK then begin
        inherited Create;
  
        Initializing      := True;
        FPattern          := TBrushStyle(cmbPattern.ItemIndex);
        FSelectedColour   := cbSelected.ActiveColor;
        FUnSelectedColour := cbUnSelected.ActiveColor;
        Title             := eName.Text;
        MapSheetKey       := dmGeneraldata.GetNextKey('MAP_SHEET','Map_Sheet_Key');
        FileName          := AppSettings.ObjectSheetFilePath + MapSheetKey + '.gsf';
        Initializing      := False;
  
        dmDatabase.ExecuteSQL(Format(
          'INSERT INTO Map_Sheet (Map_Sheet_Key, Sheet_Name, Sheet_Type, Sheet_Displayed, ' +
          'New_Data, Modified_Data, Dataset_Sheet_Order, Selected_Colour, Unselected_Colour, ' +
          'Dataset_Sheet_Name, Pattern_Index, File_Name, Base_Map_Key, Entered_By)' +
          'VALUES(''%s'', %s, 3, 1, 1, 1, 0, %d, %d, %s, %d, ''%s.gsf'', ''%s'', ''%s'')',
          [MapSheetKey, QuotedStr(Title), SelectedColour, UnselectedColour, QuotedStr(Title),
           Ord(Pattern), ABaseMapKey, ABaseMapKey, AppSettings.UserID]));
      end else
        Abort;
    finally
      Free;
    end;
end;  // TPolygonLayerItem.CreateNew 

{-------------------------------------------------------------------------------
}
procedure TPolygonLayerItem.DoAssign(ASource: TBaseLegendItem);
begin
  if ASource is TPolygonLayerItem then
    SetProperties(TPolygonLayerItem(ASource).Pattern,
                  TPolygonLayerItem(ASource).SelectedColour,
                  TPolygonLayerItem(ASource).UnSelectedColour);
  
  inherited;
end;  // TPolygonLayerItem.DoAssign 

{-------------------------------------------------------------------------------
  Display TdlgPolygonLayerDetails dialog allowing the current layer to be edited.
  Refreshes the unselected polygon colours if they have changed. 
}
function TPolygonLayerItem.DoEditProperties: Boolean;
begin
  with TdlgPolygonLayerDetails.Create(nil) do
    try
      // Put info into dialog.
      eName.Text               := Title;
      cbSelected.ActiveColor   := SelectedColour;
      cbUnselected.ActiveColor := UnselectedColour;
      cmbPattern.ItemIndex     := Integer(Pattern);
      Readonly                 := AppSettings.UserAccessLevel <= ualAddOnly;
  
      if ShowModal = mrOk then begin
        Title := eName.Text;
        SetProperties(TBrushStyle(cmbPattern.ItemIndex),
                      cbSelected.ActiveColor, cbUnselected.ActiveColor);
        Result := FColoursChanged or FPatternChanged or TitleChanged;
      end else
        Result := False;
    finally
      Free;
    end;
end;  // TPolygonLayerItem.DoEditProperties 

{-------------------------------------------------------------------------------
}
procedure TPolygonLayerItem.DrawSymbol(ACanvas: TCanvas; ARect: TRect);
var
  lPColor, lBColor: TColor;
  lBStyle: TBrushStyle;
  lLeft, lTop: Integer;
begin
  with ACanvas do begin
    // Save values.
    lPColor := Pen.Color;
    lBColor := Brush.Color;
    lBStyle := Brush.Style;
  
    // Draw little polygon using unselected colour and style.
    Brush.Color := UnselectedColour;
    Brush.Style := Pattern;
    Pen.Color   := UnselectedColour;
    // Center the symbol
    lLeft := ARect.Left + (ARect.Right - ARect.Left - 16) div 2;
    lTop  := ARect.Top  + (ARect.Bottom - ARect.Top - 16) div 2;
    Polygon([Point(lLeft +  1, lTop +  1), Point(lLeft + 15, lTop),
             Point(lLeft + 11, lTop +  8), Point(lLeft + 15, lTop + 15),
             Point(lLeft     , lTop + 14), Point(lLeft +  1, lTop +  1)]);
  
    // Restore values.
    Pen.Color   := lPColor;
    Brush.Color := lBColor;
    Brush.Style := lBStyle;
  end;
end;  // TPolygonLayerItem.DrawSymbol 

{-------------------------------------------------------------------------------
}
function TPolygonLayerItem.GetCanDelete: Boolean;
begin
  Result := not (AppSettings.UserAccessLevel in [ualReadOnly, ualRecorder, ualAddOnly]);
end;  // TPolygonLayerItem.GetCanDelete

{-------------------------------------------------------------------------------
  The TBrushgStyle type is not directly compatible with map servers fill style, so this
      provides a mapping
}
function TPolygonLayerItem.GetMSPattern: ShortInt;
begin
  Result := ConvertPatternToMSPattern(Ord(FPattern));
end;  // TPolygonLayerItem.GetMSPattern

{-------------------------------------------------------------------------------
}
procedure TPolygonLayerItem.InitFromFields(AFields: Fields);
begin
  inherited;

  FPattern          := TBrushStyle(AFields['Pattern_Index'].Value);
  FUnselectedColour := TColor(AFields['Unselected_Colour'].Value);
  FSelectedColour   := TColor(AFields['Selected_Colour'].Value);
end;  // TPolygonLayerItem.InitFromFields

{-------------------------------------------------------------------------------
}
procedure TPolygonLayerItem.ResetFlags;
begin
  inherited ResetFlags;
  
  FColoursChanged := False;
  FPatternChanged := False;
end;  // TPolygonLayerItem.ResetFlags 

{-------------------------------------------------------------------------------
}
procedure TPolygonLayerItem.SetProperties(APattern: TBrushStyle; ASelectedColour, 
    AUnselectedColour: TColor);
begin
  FColoursChanged := (ASelectedColour <> SelectedColour) or
                     (AUnselectedColour <> UnselectedColour);
  FPatternChanged := APattern <> Pattern;
  if FColoursChanged or FPatternChanged then begin
    // Update fields
    FPattern          := APattern;
    FSelectedColour   := ASelectedColour;
    FUnSelectedColour := AUnSelectedColour;
  
    // And database
    dmDatabase.ExecuteSQL(Format(
        'UPDATE Map_Sheet SET Selected_Colour=%d, Unselected_Colour=%d, ' +
        'Pattern_Index=%d, Modified_Data=%d WHERE Map_Sheet_Key=''%s''',
        [SelectedColour, UnselectedColour, Ord(Pattern),
         Ord(ColoursChanged or PatternChanged), MapSheetKey]));
  end;
end;  // TPolygonLayerItem.SetProperties 

{-==============================================================================
    TNonDrawingLayerItem
===============================================================================}
{-------------------------------------------------------------------------------
}
function TNonDrawingLayerItem.DoEditProperties: Boolean;
var
  lDialog: TdlgRegisterMap;
begin
  inherited DoEditProperties;
  
  lDialog := TdlgRegisterMap.Create(nil);
  try
    LoadData(lDialog);
    if lDialog.ShowModal = mrOk then begin
      // Display changes and write new data to database
      SaveData(lDialog);
      Result := True;
    end else
      Result := False;
  finally
    lDialog.Free;
  end;
end;  // TNonDrawingLayerItem.DoEditProperties

{-------------------------------------------------------------------------------
}
procedure TNonDrawingLayerItem.LoadData(ADialog: TdlgRegisterMap);
begin
  with ADialog do
    with dmGeneralData.qryAllPurpose do begin
      SQL.Text := 'SELECT * FROM Map_Sheet WHERE Map_Sheet_Key = ''' + MapSheetKey + '''';
      Open;
      if RecordCount = 0 then begin
        Close;
        // Clean up the layer instance as it has been deleted
        Visible := False;
        raise ELayerError.CreateNonCritical(ResStr_MapSheetDeleted);
      end;
      try
        // Populate the RegisterMap dialog
        eSheetName.ReadOnly  := not CanRename; //False;
        eFileName.ReadOnly   := True;
        bbFileName.Enabled   := False;
        lblLayerDisp.Caption := Title;
        eSheetName.Text      := Title;
        eFileName.Text       := MapFilename(dmGeneralData.qryAllPurpose);
        eSouthWest.Text      := LocaliseSpatialRef(FieldByName('SW_Spatial_Ref').AsString);
        eNorthEast.Text      := LocaliseSpatialRef(FieldByName('NE_Spatial_Ref').AsString);
        // Set thousand separator to current locale.
        cmbCutIn.Text        :=
            StringReplace(FieldByName('Cut_In_Scale').AsString, ',', ThousandSeparator, [rfReplaceAll]);
        cmbCutOut.Text       :=
            StringReplace(FieldByName('Cut_Out_Scale').AsString, ',', ThousandSeparator, [rfReplaceAll]);
        PickupDetails; // Tell dialog we have added the details.
      finally
        Close;
      end;
    end;
end;  // TNonDrawingLayerItem.LoadData 

{-------------------------------------------------------------------------------
}
procedure TNonDrawingLayerItem.SaveData(ADialog: TdlgRegisterMap);
begin
  with ADialog do
    { Note we edit a record rather than use an update query, so the spatial
      reference field handling is automatic }
    with dmGeneralData.qryAllPurpose do begin
      SQL.Text := 'SELECT File_Name, Sheet_Type, SW_Lat, SW_Long, SW_Spatial_Ref, ' +
                  'NE_Lat, NE_Long, NE_Spatial_Ref, Spatial_Ref_System, ' +
                  'Cut_In_Scale, Cut_Out_Scale, Modified_Data, Changed_By, Changed_Date ' +
                  'FROM Map_Sheet WHERE Map_Sheet_Key = ''' + MapSheetKey + '''';
      ParseSQL := False;
      try
        Open;
        try
          Edit;
          // Save file name
          if FieldByName('Sheet_Type').AsInteger in [0,3] then
            FieldByName('File_Name').AsString := ExtractFileName(eFileName.Text)
          else
            FieldByName('File_Name').AsString := eFileName.Text;
          // Work out SW Spatial Ref
          if CompareText(eSouthWest.Text, ResStr_NoneRequired) = 0 then
            FieldByName('SW_Spatial_Ref').Text := ''
          else
            FieldByName('SW_Spatial_Ref').Text := eSouthWest.Text;
          // Work out NE Spatial Ref
          if CompareText(eNorthEast.Text, ResStr_NoneRequired) = 0 then
            FieldByName('NE_Spatial_Ref').Text := ''
          else
            FieldByName('NE_Spatial_Ref').Text := eNorthEast.Text;
          // Other fields. Force english thousand separator.
          FieldByName('Cut_In_Scale').AsString   :=
              StringReplace(cmbCutIn.Text, ThousandSeparator, ',', [rfReplaceAll]);
          FieldByName('Cut_Out_Scale').AsString  :=
              StringReplace(cmbCutOut.Text, ThousandSeparator, ',', [rfReplaceAll]);
          FieldByName('Modified_Data').AsBoolean := True;
          FieldByName('Changed_By').AsString     := AppSettings.UserID;
          FieldByName('Changed_Date').AsDateTime := Now;
          Post;
        finally
          Close;
        end;
      finally
        ParseSQL := True;
      end;
      Title := eSheetName.Text;
    end;
end;  // TNonDrawingLayerItem.SaveData 

{-==============================================================================
    TBaseMapLayerItem
===============================================================================}
{-------------------------------------------------------------------------------
}
procedure TBaseMapLayerItem.DrawSymbol(ACanvas: TCanvas; ARect: TRect);
begin
  dmFormActions.ilMenuOn.Draw(ACanvas,
                              ARect.Left + (ARect.Right - ARect.Left - 16) div 2,
                              ARect.Top  + (ARect.Bottom - ARect.Top - 16) div 2,
                              21);
end;  // TBaseMapLayerItem.DrawSymbol 

{-------------------------------------------------------------------------------
}
function TBaseMapLayerItem.GetAlwaysVisible: Boolean;
begin
  Result := True;
end;  // TBaseMapLayerItem.GetAlwaysVisible 

{-------------------------------------------------------------------------------
}
function TBaseMapLayerItem.GetCanDelete: Boolean;
begin
  Result := False;
end;  // TBaseMapLayerItem.GetCanDelete

{-------------------------------------------------------------------------------
}
function TBaseMapLayerItem.GetCanRename: Boolean;
begin
  Result := False;
end;  // TBaseMapLayerItem.GetCanRename

{-==============================================================================
    TBackgroundLayerItem
===============================================================================}
{-------------------------------------------------------------------------------
}
constructor TBackgroundLayerItem.CreateNew(ABaseMapKey: TKeyString);
begin
  with TdlgRegisterMap.Create(nil) do
    try
      if FLastFilePath <> '' then dlgOpenFile.InitialDir := FLastFilePath
                             else dlgOpenFile.InitialDir := AppSettings.MapFilePath;
      lblLayerDisp.Caption := dlgOpenFile.FileName;
      if ShowModal = mrOk then begin
        inherited Create;
  
        Initializing := True;
        FileName     := eFileName.Text;
        Title        := eSheetName.text;
        MapSheetKey  := dmGeneraldata.GetNextKey('MAP_SHEET','Map_Sheet_Key');
        Initializing := False;
  
        FLastFilePath := ExtractFilePath(dlgOpenFile.FileName);
  
        // Write new record to the database
        with dmGeneralData.qryAllPurpose do begin
          SQL.Clear;
          // don't use insert query - we need automatic spatial ref handling
          // Only need fields for insert, not the whole table
          SQL.Add('SELECT TOP 0 * FROM Map_Sheet');
          Open;
          Insert;
          FieldByName('Map_Sheet_Key').AsString := MapSheetKey;
          FieldByName('Computer_ID').AsString   := AppSettings.ComputerID;
          FieldByName('Base_Map_Key').AsString  := ABaseMapKey;
          { Add the new sheet_name, file_name }
          FieldByName('Sheet_Name').AsString := eSheetName.Text;
          FieldByName('File_Name').AsString  := FileName;
          FieldByName('Sheet_Type').AsInteger := Ord(not VectorSheet) + 1;
          if CompareText(eSouthWest.Text, ResStr_NoneRequired) <> 0 then
            FieldByName('SW_Spatial_Ref').Text := eSouthWest.Text;
          if CompareText(eNorthEast.Text, ResStr_NoneRequired) <> 0 then
            FieldByName('NE_Spatial_Ref').Text := eNorthEast.Text;
          FieldByName('NE_Spatial_Ref_Qualifier').AsString := 'Direct entry';
          FieldByName('SW_Spatial_Ref_Qualifier').AsString := 'Direct entry';
          // Force English thousand separator.
          FieldByName('Cut_In_Scale').AsString             :=
              StringReplace(cmbCutIn.Text, ThousandSeparator, ',', [rfReplaceAll]);
          FieldByName('Cut_Out_Scale').AsString            :=
              StringReplace(cmbCutOut.Text, ThousandSeparator, ',', [rfReplaceAll]);
          FieldByName('Sheet_Displayed').AsBoolean         := True;
          FieldByName('Entered_By').AsString               := AppSettings.UserID;
          FieldByName('New_Data').AsBoolean                := True;
          FieldByName('Modified_Data').AsBoolean           := True;
          FieldByName('Dataset_Sheet_Order').AsInteger     := 0;
          Post;
          Close;
        end;
      end else
        Abort;
    finally
      Free;
    end;
end;  // TBackgroundLayerItem.CreateNew 

{-------------------------------------------------------------------------------
}
procedure TBackgroundLayerItem.DrawSymbol(ACanvas: TCanvas; ARect: TRect);
begin
  dmFormActions.ilMenuOn.Draw(ACanvas,
                              ARect.Left + (ARect.Right - ARect.Left - 16) div 2,
                              ARect.Top  + (ARect.Bottom - ARect.Top - 16) div 2,
                              64);
end;  // TBackgroundLayerItem.DrawSymbol

end.
