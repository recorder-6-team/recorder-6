//==============================================================================
//  Unit:        BoundaryLocationMatch
//
//  Implements:  TdlgBoundaryLocationMatch
//
//  Description: Allows the user to match imported polygons to locations in the database.
//
//  Author:      Paul Davies
//  Created:     24 February 2009
//
//  Last Revision Details:
//    $Revision: 21 $
//    $Date: 8/04/10 16:50 $
//    $Author: Andrewkemp $
//
//==============================================================================

unit BoundaryLocationMatch;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ImageListButton, Grids, MapServerLink, DataClasses,
  AddinCompositeComponent, AddinLinkedControls, BaseChildUnit, ExtCtrls,
  BaseFormUnit, Constants;

resourcestring
  ResStr_NoTempTable           = 'Could not create temporary table #Polygons.';
  ResStr_FailPopulateTempTable = 'Unable to populate temporary table #Polygons.';
  ResStr_AutomaticMatchFailed  = 'The automatic match procedure failed.';
  ResStr_MatchFailed           = 'Unable to perform the manual match.';

  ResStr_PolygonIndex          = 'Polygon ID';
  ResStr_LocationName          = 'Location Name';

  ResStr_BoundaryCount         = 'Boundary Count: %d';
  ResStr_MatchedLocations      = 'Matched Locations: %d';

  ResStr_FindLocation          = 'Select a location to link to';

const
  STR_DROP_POLYGONS = 'IF OBJECT_ID(''tempdb.dbo.#Polygons'') IS NOT NULL '
                       + 'DROP TABLE #Polygons';

type
  TdlgBoundaryLocationMatch = class(TBaseChild)
    plContainer: TPanel;
    btnCancel: TImageListButton;
    btnImport: TImageListButton;
    Bevel1: TBevel;
    lblBoundaryCount: TLabel;
    lblMatchedLocationsCount: TLabel;
    lblBoundaryAttribute: TLabel;
    cmbBoundaryAttribute: TComboBox;
    btnNewEntry: TImageListButton;
    btnNewEntries: TImageListButton;
    sgMatchedObjects: TStringGrid;
    eMatchValue: TAddinLinkedEdit;
    procedure cmbBoundaryAttributeChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure sgMatchedObjectsClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure sgMatchedObjectsKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure sgMatchedObjectsMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure sgMatchedObjectsDrawCell(Sender: TObject; ACol,
      ARow: Integer; Rect: TRect; State: TGridDrawState);
    procedure eMatchValueKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure eMatchValueFindData(Sender: TObject);
    procedure eMatchValueGetData(Sender: TObject);
    procedure btnNewEntryClick(Sender: TObject);
    procedure btnNewEntriesClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnImportClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
  private
    FAttributes: TStrings;
    FMapHandle: HWnd;
    FSourceSheetID: integer;
    FMapServerLink: TMapServerLink;
    FPrimaryAttribute: string;
    FMatchField: string;
    FPolygonCount: integer;
    FMatches: integer;
    FTargetSheetID: integer;
    FLocationKeys: TStringList;
    FPolygonIndices: Array of Integer;
    FSortColumn: Integer;
    FSortAscending: Boolean;
    FMatchX, FMatchY: Integer;
    FPolygonIndex: Integer;
    procedure PopulateMatchedObjects;
    procedure PopulateSortedGrid;
    procedure RepopulateMatchedObjects;
    procedure RefreshMatchedLocationsCountLabel;
    procedure SetupDialog(attributes: TStrings; mapHandle: HWnd; sourceSheetID,
        targetSheetID: Integer; mapServerLink: TMapServerLink; primaryAttribute,
        matchField: String; locationKeys: TStringList);
    procedure PositionMatch;
    procedure HideMatch;
    function GetSelectedAttribute: Integer;
    function MakeNewEntry(index: integer): Boolean;
    procedure SetNewEntryButtonsEnabled;
    function LocationExists(locationName: string): Boolean;
    function GetPolygonIndices(i: integer): Integer;
    function GetPolygonIndicesLength(): Integer;
    procedure Unmatch(gridY: Integer);
    procedure WMTransferDone(var Msg:TMessage); message WM_TRANSFER_DONE;
  public
    constructor Create(owner: TComponent; attributes: TStrings; mapHandle: HWnd;
        sourceSheetID, targetSheetID: integer; mapServerLink: TMapServerLink;
        primaryAttribute, matchField: string; locationKeys: TStringList); reintroduce; overload;
    procedure Cancel;
    procedure UpdateLocation(KeyList: TKeyList);
    property PolygonIndices[i: Integer]: Integer read GetPolygonIndices;
    property PolygonIndicesLength: Integer read GetPolygonIndicesLength;
    property PolygonIndex: Integer read FPolygonIndex;
    property SortColumn: Integer read FSortColumn;
    property SortAscending: Boolean read FSortAscending;
    property SelectedAttribute: Integer read GetSelectedAttribute;
    property SourceSheetId: Integer read FSourceSheetId;
    property TargetSheetId: Integer read FTargetSheetId;
  end;

implementation

uses
  DatabaseAccessADO, ms5, JNCCDatasets, DB, Find, ApplicationSettings, DateUtils, MapData,
  AddinSearchManager, FormActions, Maintbar, SpatialRefFuncs, GeneralFunctions, Map,
  Recorder2000_tlb, ComAddinUnit, GeneralData;

const
  ID_COLUMN      = 0;
  DYNAMIC_COLUMN = 1;
  NAME_COLUMN    = 2;
  SITE_KEY       = 'NBNSYS0000000005';  // Key value of the 'Site' Location_Type

resourcestring
  ResStr_SiteCentroid   = 'Site Centroid';
  ResStr_MakeNewEntries =
      'A new entry will be created for each record still unmatched.  This may take some time'#13#10
      + 'depending on the number of entries to create.'#13#10#13#10
      + 'Do you want to proceed?';
  ResStr_SomeNotAdded   = 'Some Locations were not created as there is already a ' +
                          'Location of that name.';


{$R *.dfm}

{-------------------------------------------------------------------------------
}
constructor TdlgBoundaryLocationMatch.Create(owner: TComponent; attributes:
    TStrings; mapHandle: HWnd; sourceSheetID, targetSheetID: integer;
    mapServerLink: TMapServerLink; primaryAttribute, matchField: string;
    locationKeys: TStringList);
var
  sql: string;
  attributeIndex, polygonIndex: integer;
  attributeValue: Array[0..100] of char;
begin
  inherited Create(owner);

  FMatchX := -1;
  FMatchY := -1;

  SetupDialog(attributes, mapHandle, sourceSheetID, targetSheetID, mapServerLink,
      primaryAttribute, matchField, locationKeys);

  cmbBoundaryAttribute.Items := FAttributes;
  if FPrimaryAttribute <> '' then
    cmbBoundaryAttribute.ItemIndex := cmbBoundaryAttribute.Items.IndexOf(FPrimaryAttribute)
  else
    cmbBoundaryAttribute.ItemIndex := 0;

  // Populates the temporary table with the polygon shapes and their attributes.
  MapCheck(MapGetNumObjects(FMapHandle, FSourceSheetID, FPolygonCount));
  for polygonIndex := 0 to FPolygonCount - 1 do begin
    sql := Format('INSERT INTO #Polygons VALUES (%d, NULL, ''''', [polygonIndex]);
    for attributeIndex := 0 to FAttributes.Count - 1 do begin
      MapCheck(MapDataGetAttribute(FMapHandle, FSourceSheetID, attributeIndex,
                                   polygonIndex, attributeValue, 100));
      sql := Format('%s, %s', [sql, QuotedStr(attributeValue)]);
    end;
    sql := sql + ')';
    dmDatabase.ExecuteSQL(sql, False, ResStr_FailPopulateTempTable);
  end;

  if FPrimaryAttribute <> '' then begin
    // Performs the automatic matching.
    sql := 'UPDATE     P '
        + 'SET        P.LocationKey   = L.Location_Key,'
        + '           P.LocationName  = LN.Item_Name '
        + 'FROM       Location        L '
        + 'INNER JOIN Location_Name   LN '
        + '        ON LN.Location_Key = L.Location_Key '
        + '       AND LN.Preferred    = 1 '
        + 'INNER JOIN #Polygons       P '
        + '        ON P.Attr_' + FPrimaryAttribute + '=' + FMatchField;
    dmDatabase.ExecuteSQL(sql, False, ResStr_AutomaticMatchFailed);
  end;

  lblBoundaryCount.Caption := Format(ResStr_BoundaryCount, [FPolygonCount]);

  FSortColumn    := 0;
  FSortAscending := True;

  PopulateMatchedObjects;
end;

{-------------------------------------------------------------------------------
  Sets up the dialog with the various parameters required to operate, and creates
  the temporary table it uses.
}
procedure TdlgBoundaryLocationMatch.SetupDialog(attributes: TStrings;
    mapHandle: HWnd; sourceSheetID, targetSheetID: Integer; mapServerLink:
    TMapServerLink; primaryAttribute, matchField: String; locationKeys:
    TStringList);
var
  sql: string;
  i: integer;
  attributeCount: integer;
  attributeSizes: Array of Integer;
begin
  // Populates the fields
  FAttributes       := attributes;
  FMapHandle        := mapHandle;
  FSourceSheetID    := sourceSheetID;
  FTargetSheetID    := targetsheetID;
  FMapServerLink    := mapServerLink;
  FPrimaryAttribute := primaryAttribute;
  FMatchField       := matchField;
  FLocationKeys     := locationKeys;


  MapCheck(MapDataGetNumAttributeFields(mapHandle, sourceSheetID, attributeCount));
  SetLength(attributeSizes, attributeCount);
  for i := 0 to attributeCount - 1 do begin
    MapCheck(MapDataGetMaxAttributeSize(FMapHandle, FSourceSheetID, i, attributeSizes[i]));
  end;

  dmDatabase.ExecuteSQL(STR_DROP_POLYGONS, False, ResStr_NoTempTable);

  sql := 'CREATE TABLE #Polygons ('
      + '    PolygonIndex INT,'
      + '    LocationKey  CHAR(16) COLLATE database_default,'
      + '    LocationName VARCHAR(100) COLLATE database_default';

  for i := 0 to FAttributes.Count - 1 do
    sql := Format(
        '%s, Attr_%s VARCHAR(%d) COLLATE database_default',
        [sql, FAttributes[i], attributeSizes[i]]);

  sql := sql + ')';
  dmDatabase.ExecuteSQL(sql, False, ResStr_NoTempTable);

end;

{-------------------------------------------------------------------------------
  Populates sgMatchedObjects based on the data in #Polygons, and the selected
  value in the combo box.
}
procedure TdlgBoundaryLocationMatch.PopulateMatchedObjects;
var
  attributeName: string;
begin
  sgMatchedObjects.RowCount := FPolygonCount + 1;
  attributeName := cmbBoundaryAttribute.Items[cmbBoundaryAttribute.ItemIndex];

  sgMatchedObjects.Cells[ID_COLUMN,0]      := ResStr_PolygonIndex;
  sgMatchedObjects.Cells[DYNAMIC_COLUMN,0] := attributeName;
  sgMatchedObjects.Cells[NAME_COLUMN,0]    := ResStr_LocationName;

  PopulateSortedGrid;

  RefreshMatchedLocationsCountLabel;

  //Set whether to disable new entry button
  SetNewEntryButtonsEnabled;
end;

{-------------------------------------------------------------------------------
  Populates the grid using the temporary table, sorted in th`e selected order.
}
procedure TdlgBoundaryLocationMatch.PopulateSortedGrid;
var
  sortString, attributeName, locationName: String;
  i: Integer;
begin
  attributeName := cmbBoundaryAttribute.Items[cmbBoundaryAttribute.ItemIndex];

  // Gets the name of the column to sort by
  case FSortColumn of
    ID_COLUMN:      sortString := 'PolygonIndex';
    DYNAMIC_COLUMN: sortString := 'Attr_' + attributeName;
    NAME_COLUMN:    sortString := 'LocationName';
  end;

  // Set whether to sort ascending or descending
  if FSortAscending then
    sortString := sortString + ' ASC'
  else
    sortString := sortString + ' DESC';

  // Populate the grid
  with dmDatabase.ExecuteSQL('SELECT PolygonIndex,'
                           + '       Attr_' + attributeName + ','
                           + '       LocationName '
                           + 'FROM   #Polygons '
                           + 'ORDER BY ' + sortString, True) do
    try
      i := 1;
      FMatches := 0;
      while not Eof do
      begin
        sgMatchedObjects.Cells[ID_COLUMN, i]      := Fields['PolygonIndex'].Value;
        sgMatchedObjects.Cells[DYNAMIC_COLUMN, i] := Fields['Attr_' + attributeName].Value;
        locationName                              := Fields['LocationName'].Value;
        sgMatchedObjects.Cells[NAME_COLUMN, i]    := locationName;
        if locationName <> '' then FMatches := FMatches + 1;
        i := i + 1;
        MoveNext;
      end;
    finally
      Close;
    end;

  // Needed for the arrows indicating the sort to redraw properly.
  sgMatchedObjects.Repaint;
end;

{-------------------------------------------------------------------------------
  Updates the Matched Locations Label (call this after adding/removing a match).
}
procedure TdlgBoundaryLocationMatch.RefreshMatchedLocationsCountLabel;
begin
  lblMatchedLocationsCount.Caption := Format(ResStr_MatchedLocations, [FMatches])
end;

{-------------------------------------------------------------------------------
  Repopulates the middle column of sgMatchedObjects based on the data in #Polygons,
  so that it displays the values stated in the combo box.
}
procedure TdlgBoundaryLocationMatch.RepopulateMatchedObjects;
var
  attributeName: string;
begin
  attributeName := cmbBoundaryAttribute.Items[cmbBoundaryAttribute.ItemIndex];
  sgMatchedObjects.Cells[DYNAMIC_COLUMN, 0] := attributeName;
  PopulateSortedGrid;
end;

{-------------------------------------------------------------------------------
  When the item selected in the combo box is changed, reload the string grid so
  that the selected column is displayed.
}
procedure TdlgBoundaryLocationMatch.cmbBoundaryAttributeChange(Sender: TObject);
begin
  HideMatch;
  RepopulateMatchedObjects;
  SetNewEntryButtonsEnabled;
end;

{-------------------------------------------------------------------------------
  When the form is closed, if the modal result is mrOK and there are some matches,
  save the matches to the database.
}
procedure TdlgBoundaryLocationMatch.FormClose(Sender: TObject; var Action: TCloseAction);
var
  locationKey: string;
  i: integer;
begin
  Action := caFree;
  try
    if (ModalResult = mrOK) and (FMatches > 0) then
    begin
      // Gets all the items with non-null location keys into the lists
      with dmDatabase.ExecuteSQL('SELECT PolygonIndex,'
                               + '       LocationKey '
                               + 'FROM   #Polygons '
                               + 'WHERE  LocationKey <> ''''', True) do
      begin
        try
          i := 0;
          while not Eof do begin
            locationKey := Fields['LocationKey'].Value;
            SetLength(FPolygonIndices, i + 1);
            FLocationKeys.Add(locationKey);
            FPolygonIndices[i] := Fields['PolygonIndex'].Value;
            i := i + 1;
            MoveNext;
          end;
        finally
          Close;
        end;
      end;
    end;

  finally
    (TfrmMap(Owner)).MatchedBoundaries(ModalResult = mrOK, ModalResult <> mrCancel);
  end;
end;

{-------------------------------------------------------------------------------
  When the user clicks on the string grid (location name column), opens the find
  window to allow them to select a location.
}
procedure TdlgBoundaryLocationMatch.sgMatchedObjectsClick(Sender: TObject);
var
  gridX, gridY: integer;
  cursorLocation: TPoint;
begin
  // Get the mouse coordinates on the grid before the user moves it
  GetCursorPos(cursorLocation);
  cursorLocation := sgMatchedObjects.ScreenToClient(cursorLocation);

  sgMatchedObjects.MouseToCell(cursorLocation.X, cursorLocation.Y, gridX, gridY);

  // If the square being clicked on is a location field...
  if (gridX = NAME_COLUMN) and (gridY > 0) and (gridY <= FPolygonCount) then begin
    FMatchX := gridX;
    FMatchY := gridY;
    eMatchValue.Text := sgMatchedObjects.Cells[gridX, gridY];
    PositionMatch;
    if eMatchValue.Visible then
    begin
      FocusControl(eMatchValue);
    end;
  end else
    HideMatch;
end;

{-------------------------------------------------------------------------------
  Drops the temporary table when the form is destroyed.
}
procedure TdlgBoundaryLocationMatch.FormDestroy(Sender: TObject);
begin
  dmDatabase.ExecuteSQL(STR_DROP_POLYGONS, False, ResStr_NoTempTable);
end;

{-------------------------------------------------------------------------------
  When the delete key is pressed, delete the match for the currently selected
  row.
}
procedure TdlgBoundaryLocationMatch.sgMatchedObjectsKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
var
  gridY: Integer;
begin
  gridY := sgMatchedObjects.Selection.Top;
  if (Key in [VK_BACK, VK_DELETE])
     and (gridY > 0)
     and (sgMatchedObjects.Cells[NAME_COLUMN, gridY] <> '') then
  begin
    Unmatch(gridY);
  end;
end;

{-------------------------------------------------------------------------------
  Clicking on the column headers causes the grid to be sorted. Not handled by the
  click event because the column headers are fixed and so don't cause it to fire.
}
procedure TdlgBoundaryLocationMatch.sgMatchedObjectsMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  gridX, gridY: integer;
begin
  sgMatchedObjects.MouseToCell(X, Y, gridX, gridY);

  //Set whether to disable btnNewEntry and btnNewEntries
  SetNewEntryButtonsEnabled;

  // Clicking on column headings sorts the grid
  if gridY = 0 then
  begin
    // If already sorting on this column, reverse the order, else sort the new column
    // in ascending order.
    if gridX = FSortColumn then
      FSortAscending := not FSortAscending
    else begin
      FSortAscending := True;
      FSortColumn    := gridX;
    end;

    HideMatch;
    PopulateSortedGrid;
  end;
end;

{-------------------------------------------------------------------------------
  Positions the eMatch control on top of the specified grid cell.
}
procedure TdlgBoundaryLocationMatch.PositionMatch;
var
  cellRect: TRect;
begin
  if FMatchX = NAME_COLUMN then begin
    cellRect := sgMatchedObjects.CellRect(FMatchX, FMatchY);
    eMatchValue.Parent  := sgMatchedObjects;
    // Fit control into available space.
    eMatchValue.SetBounds(
        cellRect.Left,
        cellRect.Top - 1,  // Align to cover top line of grid cell, looks better.
        cellRect.Right - cellRect.Left,
        eMatchValue.Height);
    eMatchValue.Visible := True;
  end else
    eMatchValue.Visible := False;
end;

{-------------------------------------------------------------------------------
  Draws a triangle indicating which column is currently being sorted on, and the
  direction of the sort.
}
procedure TdlgBoundaryLocationMatch.sgMatchedObjectsDrawCell(
  Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
begin
  with sgMatchedObjects do
    if (ARow = 0) and (ACol = FSortColumn) and (Rect.Right - 12 > Rect.Left + 4) then
    begin
      Canvas.Brush.Color := clWindowText;
      Canvas.Pen.Color   := clWindowText;
      if FSortAscending then
        Canvas.Polygon([Point(Rect.Right - 12, Rect.Top + 11),
                        Point(Rect.Right -  4, Rect.Top + 11),
                        Point(Rect.Right -  8, Rect.Top +  7)])
      else
        Canvas.Polygon([Point(Rect.Right - 12, Rect.Top +  7),
                        Point(Rect.Right -  4, Rect.Top +  7),
                        Point(Rect.Right -  8, Rect.Top + 11)]);
    end else
    // Display read-only cells greyed, while still handling highlights.
    if ARow > 0 then
      if ACol in [ID_COLUMN, DYNAMIC_COLUMN] then
      begin
        Canvas.Brush.Color := MergeColours(clWindow, clBtnFace, 50);
        if State * [gdSelected, gdFocused] <> [] then
          Canvas.Brush.Color := MergeColours(Canvas.Brush.Color, clHighlight, 50);
        Canvas.Font.Color :=
            GetContrastColour(MergeColours(Canvas.Brush.Color, Canvas.Brush.Color, 50));
        Canvas.FillRect(Rect);
        DrawChoppedText(Cells[ACol, ARow], Canvas,Rect, 2);
      end;
  PositionMatch;
end;

{-------------------------------------------------------------------------------
  Hides the match control.
}
procedure TdlgBoundaryLocationMatch.HideMatch;
begin
  FMatchX := -1;
  eMatchValue.Visible := False;
end;

{-------------------------------------------------------------------------------
}
procedure TdlgBoundaryLocationMatch.eMatchValueKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
var
  lMoveLeftOk, lMoveRightOk: Boolean;
begin
  inherited;
  
  with eMatchValue do begin
    // editable, caret at left limit of text, or all text selected
    lMoveLeftOk := (SelStart = 0) or (SelLength = Length(Text));
    // editable, caret at right limit of text, or all text selected
    lMoveRightOk := (SelStart = Length(Text)) or (SelLength = Length(Text));
  end;

  case Key of
    VK_BACK, VK_DELETE:
      begin
        sgMatchedObjects.Perform(WM_KEYDOWN, Key, 0);
      end;
    VK_RETURN:
      begin
        if (eMatchValue.Key <> '') then
          sgMatchedObjects.Perform(WM_KEYDOWN, Key, 0);
        Key := 0;
      end;
    VK_UP, VK_DOWN, VK_PRIOR, VK_NEXT:
      begin
        // Some text, but no key, see if can automatically find match.
        if (eMatchValue.Text <> '') and (eMatchValue.Key = '') then eMatchValueFindData(nil);
        sgMatchedObjects.Perform(WM_KEYDOWN, Key, 0);
        Key := 0;
      end;
    VK_LEFT, VK_HOME:
      if lMoveLeftOk then begin
        // Some text, but no key, see if can automatically find match.
        if (eMatchValue.Text <> '') and (eMatchValue.Key = '') then eMatchValueFindData(nil);
        sgMatchedObjects.Perform(WM_KEYDOWN, Key, 0);
      end;
    VK_RIGHT, VK_END:
      if lMoveRightOk then begin
        // Some text, but no key, see if can automatically find match.
        if (eMatchValue.Text <> '') and (eMatchValue.Key = '') then eMatchValueFindData(nil);
        sgMatchedObjects.Perform(WM_KEYDOWN, Key, 0);
      end;
    VK_ESCAPE:
      begin
        HideMatch;
      end;
  end;
end;  // TBaseMatch.eMatchValueKeyDown

{-------------------------------------------------------------------------------
  Uses the find dialog to set the location.
}
procedure TdlgBoundaryLocationMatch.eMatchValueFindData(Sender: TObject);
var
  sql: String;
begin
  if eMatchValue.Text = '' then
  begin
    if sgMatchedObjects.Cells[FMatchX, FMatchY] <> '' then
    begin
      Unmatch(FMatchY);

      //Set whether to disable btnNewEntry and btnNewEntries
      SetNewEntryButtonsEnabled;
    end;
    HideMatch;
  end else
    with TdlgFind.CreateDialog(Self, ResStr_FindLocation, ftLocation) do
      try
        // Attempts to find a unique match, if there isn't one opens the dialog to
        // allow the user to choose.
        if FindUnique(eMatchValue.Text) or (ShowModal = mrOk) then begin
          // If the field was previously blank, increment the match count
          if sgMatchedObjects.Cells[FMatchX, FMatchY] = '' then begin
            FMatches := FMatches + 1;
            RefreshMatchedLocationsCountLabel;
          end;

          // Set the cell to equal the returned text
          sgMatchedObjects.Cells[FMatchX, FMatchY] := ItemText;

          // Update the temporary table with the new information
          sql := Format('UPDATE P '
              + 'SET    LocationKey          = L.Location_Key,'
              + '       LocationName         = ''%s'' '
              + 'FROM   #Polygons            P '
              + 'JOIN   Location_Name        LN '
              + '    ON LN.Location_Name_Key = ''%s'' '
              + 'JOIN   Location             L '
              + '    ON L.Location_Key       = LN.Location_Key '
              + 'WHERE  PolygonIndex         = %s',
              [ItemText,
               ItemKey,
               sgMatchedObjects.Cells[ID_COLUMN, FMatchY]]);

          dmDatabase.ExecuteSQL(sql, False, ResStr_MatchFailed);
          HideMatch;
        end;
      finally;
        Free;
      end;
end;

{-------------------------------------------------------------------------------
  Uses the location screen to find a location to link to the boundary.
}
procedure TdlgBoundaryLocationMatch.eMatchValueGetData(Sender: TObject);
begin
  dmFormActions.actLocations.ActionComponent := Self;
  dmFormActions.actLocations.Execute;
  SetupLink(TBaseForm(frmMain.ActiveMDIChild), Self, UpdateLocation);
  dmFormActions.actLocations.ActionComponent := nil;
end;

{-------------------------------------------------------------------------------
  Gets the index of the selected attribute.
}
function TdlgBoundaryLocationMatch.GetSelectedAttribute;
begin
  Result := cmbBoundaryAttribute.ItemIndex;
end;

{-------------------------------------------------------------------------------
  Creates a new location and links it to the polygon at the specified index.
}
function TdlgBoundaryLocationMatch.MakeNewEntry(index: integer): Boolean;
var
  name: string;
  id: integer;
  center: MSCoord;
  latLong: TLatLong;
  spatialRefString: string;

  procedure SetSpatialReferences;
  var
    datasetSpatialSystem, latStr, longStr: string;
    comMapFormat: IBaseMapFormat;
    i: integer;
  begin
    datasetSpatialSystem := TfrmMap(dmFormActions.GetFormInstance(TfrmMap)).DatasetSpatialSystem;

    { Convert to LatLong.  Depends on the base map system what we have to do }
    if SameText(datasetSpatialSystem, OS_GB) or SameText(datasetSpatialSystem, OS_NI) then
      latLong := SpecificENToLatLong(TMapCoord(center), datasetSpatialSystem)
    else
    if SameText(datasetSpatialSystem, LAT_LONG) then begin
      latLong.Lat := center.Y;
      latLong.Long:= center.X;
    end else
      for i := 0 to AppSettings.ComAddins.SpatialSystemInterfaces.Count - 1 do
      begin
        if (AppSettings.ComAddins.SpatialSystemInterfaces.Items[i] as
            ISpatialReference).SpatialRefSystem = datasetSpatialSystem then
        begin
          comMapFormat := AppSettings.ComAddins.SpatialSystemInterfaces.Items[i]
                           as IBaseMapFormat;
          latStr  := comMapFormat.MapCoordToLat(center.X,center.Y);
          longStr := comMapFormat.MapCoordToLong(center.X,center.Y);
          if (latStr = ResStr_BeyondSystemLimits ) or (longStr = ResStr_BeyondSystemLimits) then
            raise EOutOfRangeError.Create('')
          else
          if (latStr = '') or (longStr = '') then
            raise EComAddinError.CreateNonCritical(
                (AppSettings.ComAddins.SpatialSystemInterfaces.Items[i] as ISpatialReference).GetLastError)
          else begin
            latLong.Lat := LatitudeToFloat(latStr);
            latLong.Long := LongitudeToFloat(longStr);
          end;
        end;//if
      end;//for

    { Convert back from LatLong to a spatial reference. }
    if SameText(AppSettings.SpatialRefSystem, LAT_LONG) then
      spatialRefString := LTLNToAccurateFormattedLTLN(latLong)
    else
      spatialRefString := ConvertfromLatLong(latLong, AppSettings.SpatialRefSystem);
  end;

begin
  Result := false;
  if (index > 0) and (index < sgMatchedObjects.RowCount) then begin
    name := Trim(sgMatchedObjects.Cells[DYNAMIC_COLUMN, index]);
    if not LocationExists(name) then begin
      id   := StrToInt(sgMatchedObjects.Cells[ID_COLUMN, index]);

      MapCheck(MapGetObjectCentroid(FMapHandle, FSourceSheetID, id, center));

      SetSpatialReferences;

      dmDatabase.RunStoredProc('usp_Location_InsertForPolygonImport',
          ['@LocationName',        name,
           '@SpatialRef',          spatialRefString,
           '@SpatialRefSystem',    AppSettings.SpatialRefSystem,
           '@Lat',                 latLong.Lat,
           '@Long',                latLong.Long,
           '@SpatialRefQualifier', ResStr_SiteCentroid,
           '@LocationType',        SITE_KEY,
           '@UserID',              AppSettings.UserID,
           '@PolygonIndex',        id]);

      if sgMatchedObjects.Cells[NAME_COLUMN, index] = '' then begin
        FMatches := FMatches + 1;
        RefreshMatchedLocationsCountLabel;
      end;

      sgMatchedObjects.Cells[NAME_COLUMN, index] := name;
      if (index = FMatchY) and (FMatchX > -1) then
        eMatchValue.Text := name;

      Result := true;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Creates a new entry for each currently selected row.
}
procedure TdlgBoundaryLocationMatch.btnNewEntryClick(Sender: TObject);
var
  i: Integer;
  allSuccess: boolean;
begin
  allSuccess := True;
  for i := sgMatchedObjects.Selection.Top to sgMatchedObjects.Selection.Bottom do
    allSuccess := MakeNewEntry(i) and allSuccess;

  if not allSuccess then
    ShowInformation(ResStr_SomeNotAdded);

  SetNewEntryButtonsEnabled;
end;

{-------------------------------------------------------------------------------
  Creates a new entry for every unmatched row in the grid.
}
procedure TdlgBoundaryLocationMatch.btnNewEntriesClick(Sender: TObject);
var
  i: Integer; 
  allSuccess: boolean;
begin
  if ConfirmYesNo(ResStr_MakeNewEntries) = mrYes then
  begin   
    allSuccess := True;
    for i := 1 to sgMatchedObjects.RowCount - 1 do
      if sgMatchedObjects.Cells[NAME_COLUMN, i] = '' then
        allSuccess := MakeNewEntry(i) and allSuccess;

    if not allSuccess then
      ShowInformation(ResStr_SomeNotAdded);

    SetNewEntryButtonsEnabled;
  end;
end;

{-------------------------------------------------------------------------------
  Set whether to disable btnNewEntry and btnNewEntries
}
procedure TdlgBoundaryLocationMatch.SetNewEntryButtonsEnabled;
var
  gridY: integer;
begin
  gridY := sgMatchedObjects.Selection.Top;

  if FMatches = 0 then
  begin
    btnNewEntry.Enabled := true;
    btnNewEntries.Enabled := true;
  end else begin
    //if all boundries are matched with a location then disable btnNewEntries
    btnNewEntries.Enabled := FMatches <> sgMatchedObjects.RowCount - 1;

    //if selected boundry is matched with a location then disable btnNewEntry
    btnNewEntry.Enabled := (gridY > 0) and (sgMatchedObjects.Cells[NAME_COLUMN, gridY] = '');
  end;    

  // Need to disable the new entry button if a location of this name already
  // exists- only bother if the button is currently enabled though.
  btnNewEntry.Enabled := (gridY > 0) and btnNewEntry.Enabled and
              not LocationExists(sgMatchedObjects.Cells[DYNAMIC_COLUMN, gridY]);
end;

{-------------------------------------------------------------------------------
  Function for checking whether or not a location of the given name already exists
  or not.
}
function TdlgBoundaryLocationMatch.LocationExists(locationName: string): Boolean;
var
  sql: string;
begin
  sql := Format('SELECT 1 ' +
                'FROM   Location_Name ' +
                'WHERE  Item_Name = %s',
                [QuotedStr(locationName)]);

  Result := dmDatabase.ExecuteSQL(sql, true).RecordCount <> 0;
end;

{-------------------------------------------------------------------------------
}
procedure TdlgBoundaryLocationMatch.FormShow(Sender: TObject);
begin
  inherited;
  sgMatchedObjects.Row := 1;
  sgMatchedObjects.Col := NAME_COLUMN;
  FMatchX := sgMatchedObjects.Col;
  FMatchY := sgMatchedObjects.Row;
  eMatchValue.Text := sgMatchedObjects.Cells[FMatchX, FMatchY];
  PositionMatch;

  // hack to size the controls to the screen
  plContainer.Anchors := [akLeft, akTop];
  plContainer.Width := ClientWidth;
  plContainer.Height := ClientHeight;
  plContainer.Anchors := [akLeft, akTop, akRight, akBottom];

  if eMatchValue.Visible then
  begin
    FocusControl(eMatchValue);
  end;
end;

{ The Location link call back method
}
procedure TdlgBoundaryLocationMatch.UpdateLocation(KeyList: TKeyList);
var
  sql: string;
begin
  try
    if KeyList.Capacity > 0 then begin
      sql := Format('UPDATE     P '
                  + 'SET        LocationKey     = LN.Location_Key,'
                  + '           LocationName    = LN.Item_Name '
                  + 'FROM       #Polygons       P '
                  + 'INNER JOIN Location_Name   LN '
                  + '        ON LN.Location_Key = ''%s'' '
                  + 'WHERE      PolygonIndex    = %s',
                    [KeyList.Items[0].KeyField1,
                     sgMatchedObjects.Cells[ID_COLUMN, FMatchY]]);
      dmDatabase.ExecuteSQL(sql, False, ResStr_UnableToMatchBoundary);

      if sgMatchedObjects.Cells[FMatchX, FMatchY] = '' then begin
        FMatches := FMatches + 1;
        RefreshMatchedLocationsCountLabel;
      end;

      sgMatchedObjects.Cells[FMatchX, FMatchY] :=
          dmGeneralData.GetLocationName(KeyList.Items[0].KeyField1);

      HideMatch();
    end;

  finally
    KeyList.Free;
  end;
end;

{-------------------------------------------------------------------------------
}
function TdlgBoundaryLocationMatch.GetPolygonIndices(i: integer): Integer;
begin
  Result := FPolygonIndices[i];
end;

{ exposes the Matched Polygon Indices
}
function TdlgBoundaryLocationMatch.GetPolygonIndicesLength: Integer;
begin
  Result := Length(FPolygonIndices);
end;

{ Shows the form after getting a location
}
procedure TdlgBoundaryLocationMatch.WMTransferDone(var Msg: TMessage);
begin
  Show;
end;

{ The Import button click}
procedure TdlgBoundaryLocationMatch.btnImportClick(Sender: TObject);
begin
  inherited;
  ModalResult := mrOk;
  Close();
end;

{ The cancel click}
procedure TdlgBoundaryLocationMatch.btnCancelClick(Sender: TObject);
begin
  inherited;
  Close();
end;

{ cancels the import by closing the form, and suppresses the map
screen from showing itself}
procedure TdlgBoundaryLocationMatch.Cancel;
begin
  ModalResult := mrCancel;
  Close();
end;

{ Deletes a match
}
procedure TdlgBoundaryLocationMatch.Unmatch(gridY: Integer);
var
  sql: String;
begin
  sgMatchedObjects.Cells[NAME_COLUMN, gridY] := '';
  eMatchValue.Text := '';
  FMatches := FMatches - 1;
  RefreshMatchedLocationsCountLabel;
  // Set the location key and name for this cell to null/blank
  sql := Format('UPDATE P '
      + 'SET    LocationKey  = NULL,'
      + '       LocationName = '''' '
      + 'FROM   #Polygons    P '
      + 'WHERE  PolygonIndex = %s',
      [sgMatchedObjects.Cells[ID_COLUMN, gridY]]);

  dmDatabase.ExecuteSQL(sql, False, ResStr_MatchFailed);
end;

end.
