{===============================================================================
  Unit:        MatchLocations

  Defines:     TfraMatchLocations

  Description: Match locations from the data file to Recorder locations.

  Model:       ImportWizard

  Last revision information:
    $Revision: 36 $
    $Date: 12/07/10 11:53 $
    $Author: Andrewkemp $

===============================================================================}
unit MatchLocations;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, DB,
  Dialogs, BaseMatchPage, StdCtrls, ImageListButton, ExtCtrls, IWBasePage, Menus,
  AddinCompositeComponent, AddinLinkedControls, Grids, HTMLView, Buttons, BaseFormUnit,
  ADODB, DBGrids, ImportWizardDBGrid, ComboListID, FormActions, DataClasses,
  IWSettings, Constants;

resourcestring
  ResStr_GridReference = 'Grid Reference';
  ResStr_Centroid      = 'Centroid';
  ResStr_SeveralRefFound =
      'The selected location has several associated grid references. Please select '#13
      + 'one for the new location using the Centroid column.';
  ResStr_NewEntriesSeveralRefFound =
      'There is at least one location with several associated grid references.'#13
      + 'Please select one for each location using the Centroid column or check the'#13
      + 'option to use the first associated grid reference.';
  ResStr_SiteCentroid = 'Site Centroid';

type
  {-----------------------------------------------------------------------------
    Wizard page displayed when matching data from the imported Location column type to
    Recorder's list of locations.
    This page is omitted from the wizard sequence if there is no column in the import file
    associated with the Location column type.
  }
  TfraMatchLocations = class(TBaseMatch)
    chkVagueLocations: TCheckBox;
    eCentroid: TAddinLinkedEdit;
    lblLocation: TLabel;
    pmCentroid: TPopupMenu;
    pnlLocation: TPanel;
    shpLocation: TShape;
    sgSurrogateMatch: TStringGrid;
    chkUseFirstCentroid: TCheckBox;
    procedure chkVagueLocationsClick(Sender: TObject);
    procedure eCentroidGetData(Sender: TObject);
    procedure eCentroidKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure sgSurrogateMatchDrawCell(Sender: TObject; ACol,
      ARow: Integer; Rect: TRect; State: TGridDrawState);
    procedure sgSurrogateMatchSelectCell(Sender: TObject; ACol,
      ARow: Integer; var CanSelect: Boolean);
    procedure sgSurrogateMatchExit(Sender: TObject);
    procedure sgSurrogateMatchClick(Sender: TObject);
    procedure sgSurrogateMatchTopLeftChanged(Sender: TObject);
    procedure sgSurrogateMatchMouseUp(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  private
    FMatchedRows: TList;
    FRememberedRows: TList;
    FDropRow: integer;
    FDropCol: integer;
    FSorting: boolean;
    procedure CentroidFromGridRefClick(Sender: TObject);
    procedure CentroidFromMapClick(Sender: TObject);
    function FieldForCol(ACol: integer): TField;
    function FormatSpatialReference(const reference: String): String;
    function GetCurrentCell: string;
    function GetRowMatched(Index: Integer): Boolean;
    function GetRowRemembered(Index: Integer): Boolean;
    procedure InitGridColumns;
    procedure InitGridRow;
    procedure InitSurrogateGrid;
    procedure PopulateGridRefCombo(ACol, ARow: integer);
    procedure SaveLocationCentroid(const importValue: String);
    procedure SetCurrentCell(const Value: string);
    procedure SetRowMatched(Index: Integer; Value: Boolean);
    procedure SetRowRemembered(Index: Integer; Value: Boolean);
    procedure sgSurrogateMatchDrop(const Sender: TObject; const iFormat : integer;
      const iSourceData: TKeyList; const iTextStrings : TstringList;
      const iIsPasteOperation: boolean; var ioHandled : boolean);
    procedure sgSurrogateMatchCheckDrop(APoint: TPoint; const ATable, AFieldKey: String;
      var Accept: boolean);
    procedure UpdateCentroid(KeyList: TKeyList);
  protected
    procedure DoEditableValuesLoad; override;
    procedure DoEditableValuesSave; override;
    procedure FindNextUnmatched; override;
    function GetHasNext: Boolean; override;
    function GetMatchGrid: TCustomGrid; override;
    function GetNext: TBasePageClass; override;
    function GetPrevious: TBasePageClass; override;
    function GetRowImportValue: string; override;
    procedure LoadContent; override;
    procedure MakeNewEntries; override;
    procedure MakeNewEntry; override;
    procedure RefreshRow; override;
    function RunSearch: Boolean; override;
    function SelectedField: TField; override;
    function SetCellColour(const AFieldName: String): Boolean; override;
    procedure SetInputControl; override;
    procedure SetupDisplay; override;
    property CurrentCell: string read GetCurrentCell write SetCurrentCell;
    property RowMatched[Index: Integer]: Boolean read GetRowMatched write SetRowMatched;
    property RowRemembered[Index: Integer]: Boolean read GetRowRemembered write SetRowRemembered;
  public
    constructor Create(AOwner: TComponent; ASettings: TdmIWSettings); override;
    destructor Destroy; override;
    procedure RefreshContent; override;
    procedure RegisterDragDropComponents; override;
    procedure SaveContent; override;
    procedure UpdateMapWindowSelector; override;
    procedure UnRegisterDragDropComponents; override;
  end;

//==============================================================================
implementation

{$R *.dfm}

uses
  MissingData, MatchGeneric, Import, IWConstants, ApplicationSettings,
  GeneralFunctions, SpatialRefFuncs, DatabaseAccessADO, GeneralData,
  OnlineHelp, DropTarget;

const
  CRLF = #13#10;
  GRID_REF_COL = 1;
  LOCATION_MATCH_COL = 2;
  CENTROID_COL = 3;
  NOTES_MATCH_COL = 4;
type
  {-----------------------------------------------------------------------------
    Accessor to get to MeasureItem in TMenuItem which is protected.
  }
  TMenuItemAccessor = class(TMenuItem)
  end;

{-==============================================================================
    TfraMatchLocations
===============================================================================}
{-------------------------------------------------------------------------------
  Component setup
}
constructor TfraMatchLocations.Create(AOwner: TComponent; ASettings: TdmIWSettings);
begin
  inherited;
  // don't need to track moving through the dataset as grid not data aware
  // on locations matching
  dsMatchTable.OnDataChange := nil;
  HelpContext := IDH_IWMATCHLOCATIONS;
end;

{-------------------------------------------------------------------------------
}
destructor TfraMatchLocations.Destroy;
begin
  FMatchedRows.Free;
  inherited;
end;

{-------------------------------------------------------------------------------
}
procedure TfraMatchLocations.MakeNewEntries;
var
  lSpatialRef: String;
  lShowWarning: Boolean;
  i: Integer;
begin
  lShowWarning := False;
  with tblMatch do
    try
      First;
      while not Eof do begin
        if FieldByName(FN_MATCH_KEY).IsNull then
        begin
          lSpatialRef := tblMatch.FieldByName(FN_IMPORT_GRIDREF).AsString;
          if length(lspatialref) > 1 then // only do if there is a  valid spatial ref
          begin
            // Check for multi-centroid and inform user at the end if necessary.
            if (Pos(CRLF, lSpatialRef) <> 0) and not chkUseFirstCentroid.Checked then
              lShowWarning := True
            else begin
              // Proceed if only one centroid, or user said to pick the first one.
              if Pos(CRLF, lSpatialRef) = 0 then
                SaveLocationCentroid(lSpatialRef)
              else
              if chkUseFirstCentroid.Checked then
                SaveLocationCentroid(Copy(lSpatialRef, 1, Pos(CRLF, lSpatialRef) - 1));

              MatchRule.MakeNewEntry(FieldByName(FN_IMPORT_VALUE).AsString);
            end;
           end;
         end;
        Next;
      end;
    finally
      Requery;
      // Find any left empty one.
      FindNextUnmatched;
    end;

  // Refresh grid to reflect new matches. Since the grid is reinitialised after each sort,
  // there is no issue with row indexes between various objects.
  with sgSurrogateMatch do
    for i := 1 to RowCount - 1 do begin
      tblMatch.Locate(FN_IMPORT_VALUE, Cells[0, i], []);
      if tblMatch.FieldByName(FN_MATCH_KEY).AsString <> '' then begin
        Cells[LOCATION_MATCH_COL, i] :=
            tblMatch.FieldByName(FN_MATCH_VALUE).AsString;
        Cells[CENTROID_COL, i] :=
            LocaliseSpatialRef(tblMatch.FieldByName(FN_SPATIAL_REF).AsString);
        Cells[NOTES_MATCH_COL, i] :=
            tblMatch.FieldByName(FN_MATCH_NOTES).AsString;
        RowMatched[i]   := True;
        eMatchValue.Key := tblMatch.FieldByName(FN_MATCH_KEY).AsString;
        eCentroid.Key   := LocaliseSpatialRef(tblMatch.FieldByName(FN_SPATIAL_REF).AsString);
      end;
    end;

  GridEnterCell;

  if lShowWarning then
    ShowInformation(ResStr_NewEntriesSeveralRefFound);
end;  // TfraMatchLocations.MakeNewEntries

{-------------------------------------------------------------------------------
}
procedure TfraMatchLocations.MakeNewEntry;
var
  lBookmark: TBookmark;
  lSpatialRef: String;
begin
  // If only one Grid Ref, it's ok to make new entry even if nothing selected for Centroid.
  if tblMatch.FieldByName(FN_SPATIAL_REF).AsString = '' then begin
    lSpatialRef := tblMatch.FieldByName(FN_IMPORT_GRIDREF).AsString;

    if (Pos(CRLF, lSpatialRef) <> 0)
       and (not chkUseFirstCentroid.Checked or Settings.UseOldImportWizard) then
      ShowInformation(ResStr_SeveralRefFound)
    else begin
      // Stops weird moving about after requery.
      lBookmark := tblMatch.GetBookmark;
      try
        if Pos(CRLF, lSpatialRef) = 0 then  // Will also work for "old" wizard mode.
          SaveLocationCentroid(lSpatialRef)
        else
        if chkUseFirstCentroid.Checked then
          SaveLocationCentroid(Copy(lSpatialRef, 1, Pos(CRLF, lSpatialRef) - 1));

        // Back to where it was.
        tblMatch.GotoBookmark(lBookmark);
      finally
        tblMatch.FreeBookmark(lBookmark);
      end;
      inherited;  // Finish with inherited
      GridEnterCell;
    end;
  end else begin
    inherited;  // Finish with inherited
    GridEnterCell;
  end;
end;  // TfraMatchLocations.MakeNewEntry

{-------------------------------------------------------------------------------
  Sets the location centroid ready for creating a new location in Recorder.
}
procedure TfraMatchLocations.SaveLocationCentroid(const importValue: String);
var
  lValidSR: TValidSpatialRef;
  lLatLong: TLatLong;
  lSpatialRef, lSystem: String;
  idx: Integer;
begin
  // Separate lSystem from reference: 'AA9999|OSGB' => 'AA9999' & 'OSGB'
  idx := Pos('|', importValue);
  if idx = 0 then begin
    lSpatialRef := importValue;
    // No system specified - default system implied.
    lSystem     := DetermineSpatialRefSystem(lSpatialRef);
  end else begin
    lSystem     := Trim(Copy(importValue, idx + 1, Length(importValue)));
    lSpatialRef := Copy(importValue, 1, idx - 1);
  end;

  // Validate and proceed.
  lValidSR := ValidSpecificSpatialRef (lSpatialRef, lSystem);
  ContainerForm.ValidateValue(lValidSR.Valid, lValidSR.Error);
  lLatLong := ConvertToLatLong(lValidSR.FormattedSR, lSystem);
  dmDatabase.RunStoredProc('usp_IWMatchSet_LocationCentroid',
      ['@ImportValue',         tblMatch.FieldByName(FN_IMPORT_VALUE).AsString,
       '@SpatialRef',          lValidSR.FormattedSR,
       '@SpatialRefSystem',    lSystem,
       '@Lat',                 lLatLong.Lat,
       '@Long',                lLatLong.Long,
       '@SpatialRefQualifier', ResStr_SiteCentroid]);
  eCentroid.Text := LocaliseSpatialRef(lSpatialRef);
end;  // TfraMatchLocations.SetNewEntryCentroid

{-------------------------------------------------------------------------------
}
procedure TfraMatchLocations.CentroidFromGridRefClick(Sender: TObject);
begin
  eCentroid.Text := StringReplace(TMenuItem(Sender).Caption, '&', '', [rfReplaceAll]);
end;  // TfraMatchLocations.CentroidFromGridRefClick

{-------------------------------------------------------------------------------
}
procedure TfraMatchLocations.CentroidFromMapClick(Sender: TObject);
begin
  dmFormActions.MapWindowMenuClick(Sender);
  ContainerForm.SetupLink(TBaseForm(Application.MainForm.ActiveMDIChild),
                          ContainerForm, UpdateCentroid);
end;  // TfraMatchLocations.CentroidFromMapClick

{-------------------------------------------------------------------------------
}
procedure TfraMatchLocations.chkVagueLocationsClick(Sender: TObject);
begin
  inherited;
  ChangedContent;
end;  // TfraMatchLocations.chkVagueLocationsClick

{-------------------------------------------------------------------------------
}
procedure TfraMatchLocations.DoEditableValuesLoad;
begin
  // scroll the underlying dataset
  tblMatch.Locate(FN_IMPORT_VALUE, sgSurrogateMatch.Cells[0, sgSurrogateMatch.Row], []);
  if SelectedField.FieldName = FN_MATCH_VALUE then
    with eMatchValue do begin
      Text := tblMatch.FieldByName(FN_MATCH_VALUE).AsString;
      Key  := tblMatch.FieldByName(FN_MATCH_KEY).AsString;
      EditBox.SelLength := Length(Text);
    end
  else
  if SelectedField.FieldName = FN_SPATIAL_REF then
    with eCentroid do begin
      Text := LocaliseSpatialRef(tblMatch.FieldByName(FN_SPATIAL_REF).AsString);
      EditBox.SelLength := Length(Text);
    end
  else
  if SelectedField.FieldName = FN_MATCH_NOTES then
    with eMatchValue do begin
      Text := tblMatch.FieldByName(FN_MATCH_VALUE).AsString;
      Key  := tblMatch.FieldByName(FN_IMPORT_VALUE).AsString;
      EditBox.SelLength := Length(Text);
    end;
  // Can't create new entry if match already set
  btnNew.Enabled := (tblMatch.FieldByName(FN_MATCH_KEY).AsString = '');
end;  // TfraMatchLocations.DoEditableValuesLoad

{-------------------------------------------------------------------------------
}
procedure TfraMatchLocations.DoEditableValuesSave;
var
  lBookmark: TBookmark;
  function NeedMatchValueSave: boolean;
  begin
    Result := (SelectedField.FieldName = FN_MATCH_VALUE) and
              (tblMatch.FieldByName(FN_MATCH_KEY).AsString <> eMatchValue.Key);
  end;

  function NeedCentroidSave: boolean;
  begin
    Result := (SelectedField.FieldName = FN_SPATIAL_REF) and
              (VarToStr(SelectedField.Value) <> eCentroid.Text);
  end;

begin
  if (NeedMatchValueSave or NeedCentroidSave) and not FSorting then begin
    // Stops weird moving about after requery.
    LockWindowUpdate(dbgMatch.Handle);
    lBookmark := tblMatch.GetBookmark;
    try
      if NeedMatchValueSave then
      begin
        MatchRule.SetMatch(GetRowImportValue, eMatchValue.Key);
        btnCommit.Enabled := True;
        CurrentCell       := eMatchValue.Text;
        RowMatched[sgSurrogateMatch.Row] := (eMatchValue.Key <> '');
        sgSurrogateMatch.Cells[CENTROID_COL, sgSurrogateMatch.Row] :=
            LocaliseSpatialRef(
              dmGeneralData.GetLocationSpatialRef(eMatchValue.Key).FormattedSR);
         sgSurrogateMatch.Cells[NOTES_MATCH_COL, sgSurrogateMatch.Row] := '';
         MatchGrid.Invalidate;
      end else
      if NeedCentroidSave then
      begin
        if not tblMatch.FieldByName(FN_MATCH_KEY).IsNull or (eCentroid.Text = '') then
          dmDatabase.RunStoredProc('usp_IWMatchSet_LocationCentroid',
              ['@ImportValue',         tblMatch.FieldByName(FN_IMPORT_VALUE).AsString,
               '@SpatialRef',          Null,
               '@SpatialRefSystem',    Null,
               '@Lat',                 Null,
               '@Long',                Null,
               '@SpatialRefQualifier', Null])
        else begin
          // Need to convert from 'AA9999 (OSGB)' to 'AA9999|OSGB' before calling method.
          SaveLocationCentroid(DelocaliseSpatialRef(
              StringReplace(
                  StringReplace(eCentroid.Text, ' (', '|', []),
                  ')', '', [])));
        end;
        CurrentCell := eCentroid.Text;
      end;
      tblMatch.Requery;
      // Back to where it was.
      tblMatch.GotoBookmark(lBookmark);
    finally
      tblMatch.FreeBookmark(lBookmark);
      LockWindowUpdate(0);
    end;
  end;
end;  // TfraMatchLocations.DoEditableValuesSave

{-------------------------------------------------------------------------------
}
procedure TfraMatchLocations.eCentroidGetData(Sender: TObject);
var
  lPos: TPoint;
  i, lTmpWidth, lTmpHeight, lWidth, lIndex: Integer;
  lCanvas: TControlCanvas;
  lDC: HDC;
begin
  inherited;
  // Work out wher to position the popup so that it's right-aligned with control.
  lCanvas := TControlCanvas.Create;
  with lCanvas do
    try
      lDC := GetWindowDC(Self.Handle);
      lIndex := SaveDC(lDC);
      try
        Handle := lDC;
        Font := Screen.MenuFont;
  
        lWidth := 0;
        for i := 0 to pmCentroid.Items.Count - 1 do begin
          TMenuItemAccessor(pmCentroid.Items[i]).MeasureItem(lCanvas, lTmpWidth, lTmpHeight);
          if lWidth < lTmpWidth then lWidth := lTmpWidth;
        end;
        // Don't know why, but it needs 18 more pixels to be the right size.
        Inc(lWidth, 18);
      finally
        Handle := 0;
        RestoreDC(lDC, lIndex);
      end;
    finally
      Free;
    end;
  
  // Now bring it up.
  with eCentroid do
    lPos := ClientToScreen(Point(Width - lWidth, Height));
  pmCentroid.Popup(lPos.X, lPos.Y);
end;  // TfraMatchLocations.eCentroidGetData 

{-------------------------------------------------------------------------------
}
procedure TfraMatchLocations.eCentroidKeyDown(Sender: TObject; var Key: Word; Shift:
    TShiftState);
var
  lMoveLeftOk, lMoveRightOk: Boolean;
begin
  inherited;
  with eCentroid do begin
    // editable, caret at left limit of text, or all text selected
    lMoveLeftOk := (SelStart = 0) or (SelLength = Length(Text));
    // editable, caret at right limit of text, or all text selected
    lMoveRightOk := (SelStart = Length(Text)) or (SelLength = Length(Text));
  end;
  
  case Key of
    VK_RETURN, VK_UP, VK_DOWN, VK_PRIOR, VK_NEXT:
      begin
        MatchGrid.Perform(WM_KEYDOWN, Key, 0);
        Key := 0;
      end;
    VK_LEFT, VK_HOME:
        if lMoveLeftOk then MatchGrid.Perform(WM_KEYDOWN, Key, 0);
    VK_RIGHT, VK_END:
        if lMoveRightOk then MatchGrid.Perform(WM_KEYDOWN, Key, 0);
    VK_ESCAPE:
        eCentroid.Text := SelectedField.AsString;
  end;
end;  // TfraMatchLocations.eCentroidKeyDown 

{-------------------------------------------------------------------------------
}
function TfraMatchLocations.GetHasNext: Boolean;
begin
  Result := chkVagueLocations.Checked or (inherited GetHasNext);
  // Need to clear any error messages if all ok.
  if chkVagueLocations.Checked then ChangedHtml(nil);
end;  // TfraMatchLocations.GetHasNext 

{-------------------------------------------------------------------------------
}
function TfraMatchLocations.GetNext: TBasePageClass;
begin
  if Settings.MatchRuleIndex = Settings.ImportFile.MatchRuleCount then
    Result := TfraImport
  else
    Result := TfraMatchGeneric;
end;  // TfraMatchLocations.GetNext 

{-------------------------------------------------------------------------------
}
function TfraMatchLocations.GetPrevious: TBasePageClass;
begin
  if Settings.MatchRuleIndex = -1 then
    Result := TfraMissingData
  else
    Result := TfraMatchGeneric;
end;  // TfraMatchLocations.GetPrevious 

{-------------------------------------------------------------------------------
}
procedure TfraMatchLocations.LoadContent;
begin
  // Need to get chkVagueLocations state before inherited gets called.
  chkVagueLocations.Checked   := Settings.UseVagueLocations;
  chkUseFirstCentroid.Checked := Settings.UseFirstCentroid;

  inherited;
  InitSurrogateGrid;
  UpdateMapWindowSelector;

  eCentroid.Visible := false;
  eCentroid.Parent := sgSurrogateMatch;
  eCentroid.ButtonWidth := 17;
end;  // TfraMatchLocations.LoadContent 

{-------------------------------------------------------------------------------
}
function TfraMatchLocations.RunSearch: Boolean;
begin
  Result := not chkVagueLocations.Checked;
end;  // TfraMatchLocations.RunSearch 

{-------------------------------------------------------------------------------
}
procedure TfraMatchLocations.SaveContent;
begin
  inherited;
  Settings.UseVagueLocations := chkVagueLocations.Checked;
  Settings.UseFirstCentroid  := chkUseFirstCentroid.Checked;
end;  // TfraMatchLocations.SaveContent 

{-------------------------------------------------------------------------------
}
function TfraMatchLocations.SetCellColour(const AFieldName: String): Boolean;
begin
  Result := inherited SetCellColour(AFieldName);
  if AFieldName = FN_SPATIAL_REF then
    if not tblMatch.FieldByName(FN_MATCH_KEY).IsNull then
      with dbgMatch do begin
        Canvas.Brush.Color := MergeColours(clWindow, clBtnFace, 50);
        if tblMatch.RecNo mod 5 = 0 then
          Canvas.Brush.Color := MergeColours(Canvas.Brush.Color, clHighlight, 95);
        Canvas.Font.Color  := GetContrastColour(Canvas.Brush.Color);
        Result := True;
      end;
end;  // TfraMatchLocations.SetCellColour

{-------------------------------------------------------------------------------
  Expects the format to be REF|SYS. Formats the string for display by removing the
  system if it is the current one, or by bracketting it if it isn't.
}
function TfraMatchLocations.FormatSpatialReference(const reference: String): String;
begin
  if Pos(AppSettings.SpatialRefSystem, reference) > 0 then
    Result := StringReplace(reference, '|' + AppSettings.SpatialRefSystem, '', [])
  else
    Result := StringReplace(reference, '|', ' (', []) + ')';
end;  // TfraMatchLocations.FormatSpatialReference

{-------------------------------------------------------------------------------
  Populates the grid ref combo when the appropriate cell is selected.
}
procedure TfraMatchLocations.PopulateGridRefCombo(ACol, ARow: integer);
var
  lRect: TRect;
  lRefs: TStringList;
  i: Integer;
begin
  if not Assigned(FieldForCol(ACol)) then Exit;
  if (FieldForCol(ACol).FieldName = FN_SPATIAL_REF) and
     (tblMatch.FieldByName(FN_MATCH_KEY).IsNull) then
  begin
    with pmCentroid do begin
      // Reset to have just the maps.
      UpdateMapWindowSelector;

      // Now add all Grid Refs.
      lRefs := TStringList.Create;
      try
        lRefs.Text := sgSurrogateMatch.Cells[GRID_REF_COL, ARow];
        for i := 0 to lRefs.Count - 1 do begin
          Items.Insert(i, TMenuItem.Create(Items));
          Items[i].Caption := FormatSpatialReference(lRefs[i]);
          Items[i].OnClick := CentroidFromGridRefClick;
        end;
      finally
        lRefs.Free;
      end;
      if Assigned(ContainerForm) then ContainerForm.RefreshXPMenu;
    end;

    lRect := sgSurrogateMatch.CellRect(ACol, ARow);

    with eCentroid do begin
      SetBounds(lRect.Left - 1, lRect.Top - 1,
                lRect.Right - lRect.Left + 2, lRect.Bottom - lRect.Top);
      Visible := True;
      if CanFocus then SetFocus;
    end;
  end else
    eCentroid.Visible := False;
end;  // TfraMatchLocations.PopulateGridRefCombo

{-------------------------------------------------------------------------------
}
procedure TfraMatchLocations.SetupDisplay;
begin
  inherited;
  with tblMatch do begin
    with FieldByName(FN_IMPORT_GRIDREF) do begin
      DisplayLabel := ResStr_GridReference;
      ReadOnly     := True;
      Visible      := True;
    end;

    with FieldByName(FN_SPATIAL_REF) do begin
      DisplayLabel := ResStr_Centroid;
      Visible      := True;
      ReadOnly     := False;
    end;
  end;

  // Hiden anything not applicable to the "old" wizard.
  if Settings.UseOldImportWizard then begin
    chkUseFirstCentroid.Visible := False;
    pnlLocation.Height          := pnlLocation.Height - 20;
  end;
end;  // TfraMatchLocations.SetupDisplay 

{-------------------------------------------------------------------------------
}
procedure TfraMatchLocations.UpdateCentroid(KeyList: TKeyList);
begin
  try
    if (KeyList <> nil)  and (KeyList.Header.ItemCount > 0) then
      if (CompareText(KeyList.Header.TableName, 'Spatial_Ref') = 0) then
        eCentroid.Text := KeyList.Items[0].KeyField2;
  finally
    KeyList.Free;
  end;
  ChangedContent;
end;  // TfraMatchLocations.UpdateCentroid 

{-------------------------------------------------------------------------------
}
procedure TfraMatchLocations.UpdateMapWindowSelector;
begin
  inherited;
  AppSettings.UpdateMapMenu(ContainerForm, pmCentroid.Items, False, CentroidFromMapClick);
  pmCentroid.Items.Insert(0, TMenuItem.Create(pmCentroid.Items));
  pmCentroid.Items[0].Caption := '-';
end;  // TfraMatchLocations.UpdateMapWindowSelector

{-------------------------------------------------------------------------------
  Initialise the string grid which replaces the dbgrid for this screen, because
      string grids can support variable row heights
}
procedure TfraMatchLocations.InitSurrogateGrid;
begin
  InitGridColumns;
  with tblMatch do begin
    DisableControls;
    try
      First;
      sgSurrogateMatch.RowCount := RecordCount + 1;
      while not EOF do begin
        InitGridRow;
        Next;
      end;
      First;
    finally
      EnableControls;
    end;
  end; // with
  eMatchValue.Parent := sgSurrogateMatch;
end;

{-------------------------------------------------------------------------------
  Setup the columns in the surrogate string grid
}
procedure TfraMatchLocations.InitGridColumns;
var
  i: integer;
begin
  with sgSurrogateMatch do begin
    DefaultRowHeight := TStringGrid(dbgMatch).DefaultRowHeight;
    ColCount         := dbgMatch.Columns.Count;
    for i := 0 to dbgMatch.Columns.Count - 1 do begin
      ColWidths[i] := Max(dbgMatch.Columns[i].DefaultWidth, 30);
      Cells[i, 0]  := dbgMatch.Columns[i].Title.Caption;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Copy data from the match table into a row of the surrogate string grid
}
procedure TfraMatchLocations.InitGridRow;
var
  i,j: integer;
  lRowLineCount: integer;
  lValue: string;
begin
  for i := 0 to dbgMatch.Columns.Count - 1 do begin
    lValue := dbgMatch.Columns[i].Field.AsString;
    if (i = GRID_REF_COL) or (i = CENTROID_COL) then
      lValue := LocaliseSpatialRef(lValue);
    sgSurrogateMatch.Cells[i, tblMatch.RecNo] := lValue;
    // For grid refs column, multi values occupy several lines so resize the row
    if i = GRID_REF_COL then begin
      lRowLineCount := 1;
      for j := 1 to Length(dbgMatch.Columns[i].Field.AsString) do
        if dbgMatch.Columns[i].Field.AsString[j] = #13 then
          Inc(lRowLineCount);
      sgSurrogateMatch.RowHeights[tblMatch.RecNo] := sgSurrogateMatch.DefaultRowHeight * lRowLineCount;
    end;
    RowMatched[tblMatch.RecNo]    := not (tblMatch.FieldByName(FN_MATCH_KEY).IsNull);
    RowRemembered[tblMatch.RecNo] := tblMatch.FieldByName(FN_REMEMBERED).AsBoolean;
  end;
end;

{-------------------------------------------------------------------------------
  Paint a cell in the surrogate string grid
}
procedure TfraMatchLocations.sgSurrogateMatchDrawCell(Sender: TObject;
  ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);

  procedure DrawMultilineCell(const AText: String);
  var
    lRect: TRect;
    lLines: TStringList;
    i: Integer;
  begin
    lLines := TStringList.Create;
    with sgSurrogateMatch do
      try
        lLines.Text := AText;
        lRect := Rect;
        for i := 0 to lLines.Count - 1 do begin
          lRect.Bottom := lRect.Top + DefaultRowHeight;
          DrawChoppedText(FormatSpatialReference(lLines[i]), Canvas, lRect, 2);
          lRect.Top := lRect.Top + DefaultRowHeight;
        end; // for
      finally
        lLines.Free;
      end;
  end;

  procedure DrawSortDirection;
  begin
    if Pos(FieldForCol(ACol).FieldName, tblMatch.Sort) > 0 then
      with sgSurrogateMatch.Canvas do begin
        Brush.Color := clWindowText;
        Pen.Color   := clWindowText;
        if Pos(' DESC', tblMatch.Sort) > 0 then
          Polygon([Point(Rect.Right - 12, Rect.Top +  7),
                   Point(Rect.Right -  4, Rect.Top +  7),
                   Point(Rect.Right -  8, Rect.Top + 11)])
        else
          Polygon([Point(Rect.Right - 12, Rect.Top + 11),
                   Point(Rect.Right -  4, Rect.Top + 11),
                   Point(Rect.Right -  8, Rect.Top +  7)]);
      end;
  end;

begin
  with sgSurrogateMatch.Canvas do begin
    if (ARow = 0) then
      Brush.Color := clBtnFace
    else
    if (dbgMatch.Columns[ACol].Readonly)
      or ((ACol = CENTROID_COL) and RowMatched[ARow]) then
    begin
      Brush.Color := MergeColours(clWindow, clBtnFace, 50);
      if State * [gdSelected, gdFocused] <> [] then
        Brush.Color := MergeColours(Brush.Color, clHighlight, 50);
    end else
      Brush.Color := clWindow;

    if (ARow mod 5 = 0) and (ARow > 0) then
      Brush.Color := MergeColours(Brush.Color, clHighlight, 95);

    if (ARow > 0) and (ACol = LOCATION_MATCH_COL) and RowRemembered[ARow] then
      Brush.Color := MergeColours(Brush.Color, clGreen, 90);

    FillRect(Rect);
    // merge colours in next line converts from a windows color to RGB
    Font.Color := GetContrastColour(MergeColours(Brush.Color, Brush.Color, 50));

    if (ACol = GRID_REF_COL) and (ARow > 0) then
      DrawMultilineCell(sgSurrogateMatch.Cells[ACol, ARow])
    else
      TextOut(Rect.Left + 2, Rect.Top + 2, sgSurrogateMatch.Cells[ACol, ARow]);

    if (gdSelected in State) and not (csDesigning in ComponentState) then
      Windows.DrawFocusRect(Handle, Rect);
  end;

  if ARow = 0 then
    DrawSortDirection;
end;

{-------------------------------------------------------------------------------
  Select a cell in the grid - display the appropriate input control
}
procedure TfraMatchLocations.sgSurrogateMatchSelectCell(Sender: TObject;
  ACol, ARow: Integer; var CanSelect: Boolean);
begin
  GridLeaveCell;  
end;

{-------------------------------------------------------------------------------
  Retrieve the db field associated with a column
}
function TfraMatchLocations.FieldForCol(ACol: integer): TField;
begin
  Result := dbgMatch.Columns[ACol].Field;
end;

{-------------------------------------------------------------------------------
  Override this to retrieve the row import value from the grid
}
function TfraMatchLocations.GetRowImportValue: string;
begin
  Result := sgSurrogateMatch.Cells[0, sgSurrogateMatch.Row];
end;

{-------------------------------------------------------------------------------
  Accessor for the current cell's text
}
function TfraMatchLocations.GetCurrentCell: string;
begin
  with sgSurrogateMatch do
    Result := Cells[Col, Row];
end;

{-------------------------------------------------------------------------------
  Accessor for the current cell's text
}
procedure TfraMatchLocations.SetCurrentCell(const Value: string);
begin
  with sgSurrogateMatch do
    Cells[Col, Row] := Value;
end;

{-------------------------------------------------------------------------------
  }
procedure TfraMatchLocations.sgSurrogateMatchExit(Sender: TObject);
begin
  GridLeaveCell;
end;

{-------------------------------------------------------------------------------
}
procedure TfraMatchLocations.sgSurrogateMatchClick(Sender: TObject);
begin
  GridEnterCell;
end;

{-------------------------------------------------------------------------------
  Position the appropriate input control onto the surrogate grid
}
procedure TfraMatchLocations.SetInputControl;
var
  lRect: TRect;
begin
  if not (Refreshing or ScanningDataset) then
  begin
    eMatchValue.Visible := False;
    if SelectedField <> nil then begin
      if (SelectedField.FieldName = FN_MATCH_VALUE) OR
         (SelectedField.FieldName = FN_MATCH_NOTES)  then
      begin
        lRect := sgSurrogateMatch.CellRect(sgSurrogateMatch.Col, sgSurrogateMatch.Row);
        if Assigned(eMatchValue) then
          with eMatchValue do begin
            SetBounds(lRect.Left - 1, lRect.Top - 1,
                      lRect.Right - lRect.Left + 2, lRect.Bottom - lRect.Top);
            Visible := True;
            if CanFocus then SetFocus;
          end;
      end else
      if MatchGrid.CanFocus then
        MatchGrid.SetFocus;
      PopulateGridRefCombo(sgSurrogateMatch.Col, sgSurrogateMatch.Row);
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Ensure controls move properly when scrolled
}
procedure TfraMatchLocations.sgSurrogateMatchTopLeftChanged(Sender: TObject);
begin
  SetInputControl;
end;

{-------------------------------------------------------------------------------
  Update the match and centroid when a row has been updated in the data table.
}
procedure TfraMatchLocations.RefreshRow;
begin
  with sgSurrogateMatch do begin
    Cells[LOCATION_MATCH_COL, Row] :=
        tblMatch.FieldByName(FN_MATCH_VALUE).AsString;
    Cells[CENTROID_COL, Row] :=
        LocaliseSpatialRef(tblMatch.FieldByName(FN_SPATIAL_REF).AsString);
    Cells[NOTES_MATCH_COL, Row] :=
       tblMatch.FieldByName(FN_MATCH_NOTES).AsString;
    RowMatched[Row] := tblMatch.FieldByName(FN_MATCH_KEY).AsString <> '';
    eMatchValue.Key := tblMatch.FieldByName(FN_MATCH_KEY).AsString;
    eCentroid.Key   := tblMatch.FieldByName(FN_SPATIAL_REF).AsString;
  end;
end;

{-------------------------------------------------------------------------------
  Override the base class behaviour to nominate the string grid for matching
}
function TfraMatchLocations.GetMatchGrid: TCustomGrid;
begin
  Result := sgSurrogateMatch;
end;

{-------------------------------------------------------------------------------
  Retrieve the field for the selected column in the string grid
}
function TfraMatchLocations.SelectedField: TField;
begin
  Result := dbgMatch.Fields[sgSurrogateMatch.Col];
end;

{-------------------------------------------------------------------------------
  Accessor
}
function TfraMatchLocations.GetRowMatched(Index: Integer): Boolean;
begin
  if not Assigned(FMatchedRows) then
    FMatchedRows := TList.Create;
  Result := FMatchedRows.IndexOf(Ptr(Index)) > -1;
end;

{-------------------------------------------------------------------------------
  Accessor
}
procedure TfraMatchLocations.SetRowMatched(Index: Integer; Value: Boolean);
begin
  if not Assigned(FMatchedRows) then
    FMatchedRows := TList.Create;
  if Value then begin
    if FMatchedRows.IndexOf(Ptr(Index)) = -1 then
      FMatchedRows.Add(Ptr(Index));
  end else
    FMatchedRows.Remove(Ptr(Index));
end;

{-------------------------------------------------------------------------------
  Find next unmatched row
}
procedure TfraMatchLocations.FindNextUnmatched;
var
  lRow: integer;

  procedure SeekUnmatchedRow;
  begin
    while lRow < sgSurrogateMatch.RowCount do begin
      if not RowMatched[lRow] then
        Break;
      Inc(lRow);
    end;
  end;

begin
  lRow := sgSurrogateMatch.Row;
  SeekUnmatchedRow;
  if lRow < sgSurrogateMatch.RowCount then
    sgSurrogateMatch.Row := lRow
  else begin
    // reached bottom, so check from top of grid
    lRow := 1;
    SeekUnmatchedRow;
    if lRow < sgSurrogateMatch.RowCount then
      sgSurrogateMatch.Row := lRow
  end;
end;

{-------------------------------------------------------------------------------
  Accessor
}
function TfraMatchLocations.GetRowRemembered(Index: Integer): Boolean;
begin
  if not Assigned(FRememberedRows) then
    FRememberedRows := TList.Create;
  Result := FRememberedRows.IndexOf(Ptr(Index)) > -1;
end;

{-------------------------------------------------------------------------------
  Accessor
}
procedure TfraMatchLocations.SetRowRemembered(Index: Integer; Value: Boolean);
begin
  if not Assigned(FRememberedRows) then
    FRememberedRows := TList.Create;
  if Value then begin
    if FRememberedRows.IndexOf(Ptr(Index)) = -1 then
      FRememberedRows.Add(Ptr(Index));
  end else
    FRememberedRows.Remove(Ptr(Index));
end;

{-------------------------------------------------------------------------------
  Clicks on the column titles for sorting the grid
}
procedure TfraMatchLocations.sgSurrogateMatchMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  lCellX, lCellY: Integer;
begin
  // trap for clicks on the title row to alter grid sort
  sgSurrogateMatch.MouseToCell(X, Y, lCellX, lCellY);
  if lCellY = 0 then begin
    FSorting := true;
    GridLeaveCell;
    if (Pos(FieldForCol(lCellX).FieldName, tblMatch.Sort) > 0) and
       (Pos(' DESC', tblMatch.Sort) = 0) then
    begin
      // already sorted ASC on this col, so toggle direction to DESC
      tblMatch.Sort := '[' + FieldForCol(lCellX).FieldName + '] DESC';
    end else
      tblMatch.Sort := '[' + FieldForCol(lCellX).FieldName + ']';

    // Refresh the string grid
    sgSurrogateMatch.RowCount          := 2;
    sgSurrogateMatch.Rows[1].CommaText := '';
    eMatchValue.Text                   := '';
    eMatchValue.Key                    := '';
    eCentroid.Text                     := '';
    InitSurrogateGrid;
    // return to top of grid
    sgSurrogateMatch.Row := 1;
    sgSurrogateMatch.Col := 0;
    Fsorting := false;
    GridEnterCell;
  end;
end;

{-------------------------------------------------------------------------------
  Register the grid to receive dropped locations
}
procedure TfraMatchLocations.RegisterDragDropComponents;
begin
  inherited;
  ContainerForm.RegisterDropComponentAdvanced(
      sgSurrogateMatch,
      sgSurrogateMatchDrop,
      [TN_LOCATION],
      [CF_JNCCDATA, CF_TEXT],
      sgSurrogateMatchCheckDrop);
end;

{-------------------------------------------------------------------------------
  Unregister the grid so it no longer receives notification of dropped locations
}
procedure TfraMatchLocations.UnRegisterDragDropComponents;
begin
  inherited;
  ContainerForm.UnRegisterDragDropComponent(sgSurrogateMatch);
end;

{-------------------------------------------------------------------------------
  Check whether a data item can be dropped on the grid
}
procedure TfraMatchLocations.sgSurrogateMatchCheckDrop(APoint: TPoint;
  const ATable, AFieldKey: String; var Accept: boolean);
var
  GridPoint: TPoint;
begin
  GridPoint := sgSurrogateMatch.ScreenToClient(APoint);
  sgSurrogateMatch.MouseToCell(GridPoint.X, GridPoint.Y, FDropCol, FDropRow);
  Accept := (FDropRow > 0)
          and (AFieldKey <> '')
          and SameText(ATable, GetDataSourceTableName);
end;

{-------------------------------------------------------------------------------
  Location dropped on the grid
}
procedure TfraMatchLocations.sgSurrogateMatchDrop(const Sender: TObject;
  const iFormat: integer; const iSourceData: TKeyList;
  const iTextStrings: TstringList; const iIsPasteOperation: boolean;
  var ioHandled: boolean);
var
  lCurrentCol: integer;
begin
  if (iFormat = CF_JNCCDATA)
     and (iSourceData.Header.ItemCount > 0)
     and SameText(iSourceData.ItemTable[0], GetDataSourceTableName) then
  begin
    // Select correct cell to receive new data item, save it, then reset column
    lCurrentCol          := sgSurrogateMatch.Col;
    sgSurrogateMatch.Col := LOCATION_MATCH_COL;
    sgSurrogateMatch.Row := FDropRow;
    eMatchValue.Key      := iSourceData.Items[0].KeyField1;
    eMatchValue.Text     := dmGeneralData.GetLocationName(eMatchValue.Key);
    DoEditableValuesSave;
    sgSurrogateMatch.Col := lCurrentCol;
    ioHandled            := True;
  end;  // if (iFormat = CF_JNCCDATA) and (iSourceData.Header.ItemCount > 0)
end;

{-------------------------------------------------------------------------------
}
procedure TfraMatchLocations.RefreshContent;
begin
  PopulateGridRefCombo(sgSurrogateMatch.Col, sgSurrogateMatch.Row);
  sgSurrogateMatch.Refresh;
end;  // TfraMatchLocations.WMRefreshSpatialRefSystem




end.
