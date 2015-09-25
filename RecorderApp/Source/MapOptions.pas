//==============================================================================
//  Unit:        MapOptions
//
//  Implements:  TdlgMapOptions
//
//  Description:
//
//  Author:      John van Breda
//  Created:     6 March 2001
//
//  Last Revision Details:
//    $Revision: 105 $
//    $Date: 6/07/09 15:19 $
//    $Author: Ericsalmon $
//
//==============================================================================

unit MapOptions;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, Buttons, Grids, CheckLst, MapData, ExceptionForm, MS5,
  MS5User, ExtCtrls, OnlineHelp, ColorGrd, DataClasses, Db, ColorBtn, Map,
  MapServerLink, Menus, GeneralFunctions, ImageListButton, BaseFormUnit, XPMenu,
  Constants, BaseLegend, DatasetLegend, LayerLegend, Variants;

type
  EOptionsError = class (TExceptionPath)
  end;
  
  EConfigureToolbar = class (EOptionsError)
  end;
  
  EDatasetSetupError = class (TExceptionPath)
  end;
  
  EMapConfigError = class (TExceptionPath)
  end;
  
  EMapOptionsError = class (TExceptionPath)
  end;

  EFindBaseMapsException = class (TExceptionPath)
  end;

  TBaseMap = class (TObject)
  private
    FDescription: String;
    FFileName: String;
    FSpatialSystem: String;
  public
    constructor Create(const AFileName, ASpatialSystem, ADescription: String);
    property Description: String read FDescription;
    property FileName: String read FFileName;
    property SpatialSystem: String read FSpatialSystem;
  end;

  TSavedMap = class (TObject)
  private
    FBaseMapKey: TKeyString;
    FBaseResetIndex: Integer;
    FComputerID: String;
    FComputerMapKey: TKeyString;
    FComputerResetIndex: Integer;
    FDescription: String;
    FDisplayName: String;
    FIsDefault: Boolean;
    FMapServerLink: TMapServerLink;
    FOriginalFileName: String;
    FOriginalFileNameBeforeReset: String;
    FSpatialSystem: String;
    FSpatialSystemBeforeReset: String;
    function GetNeedReset: Boolean;
    procedure InsertData;
    procedure SetDisplayName(const Value: String);
    procedure SetIsDefault(const Value: Boolean);
    procedure UpdateData;
  public
    constructor Create;
    destructor Destroy; override;
    procedure ResetData(const AResetAll: Boolean);
    procedure SaveData;
    property BaseMapKey: TKeyString read FBaseMapKey;
    property BaseResetIndex: Integer read FBaseResetIndex;
    property ComputerID: String read FComputerID;
    property ComputerMapKey: TKeyString read FComputerMapKey;
    property ComputerResetIndex: Integer read FComputerResetIndex;
    property Description: String read FDescription;
    property DisplayName: String read FDisplayName write SetDisplayName;
    property IsDefault: Boolean read FIsDefault write SetIsDefault;
    property MapServerLink: TMapServerLink read FMapServerLink;
    property NeedReset: Boolean read GetNeedReset;
    property OriginalFileName: String read FOriginalFileName;
    property OriginalFileNameBeforeReset: String read FOriginalFileNameBeforeReset;
    property SpatialSystem: String read FSpatialSystem;
    property SpatialSystemBeforeReset: String read FSpatialSystemBeforeReset;
  end;

  {-----------------------------------------------------------------------------
    Simple class so we can store the 4 char string on a list
  }
  TdlgMapOptions = class (TForm)
    Bevel1: TBevel;
    Bevel2: TBevel;
    btnAddMap: TImageListButton;
    btnClose: TButton;
    btnDeleteDataset: TImageListButton;
    btnDeleteLayer: TImageListButton;
    btnLayerProperties: TImageListButton;
    btnMoveDown: TImageListButton;
    btnMoveUp: TImageListButton;
    btnRemoveMap: TImageListButton;
    btnReset: TImageListButton;
    bvlBaseMapFrame: TBevel;
    chkUniqueDataset: TCheckBox;
    cmbBaseMaps: TComboBox;
    cmbMaps: TComboBox;
    eCutOffYear: TEdit;
    lblCutoff: TLabel;
    lblMap: TLabel;
    pcMapOptions: TPageControl;
    sgDatasets: TStringGrid;
    sgLayers: TStringGrid;
    sgMaps: TStringGrid;
    tsBaseMap: TTabSheet;
    tsDistributionSymbols: TTabSheet;
    tsMapLayers: TTabSheet;
    procedure btnAddMapClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure btnDeleteDatasetClick(Sender: TObject);
    procedure btnDeleteLayerClick(Sender: TObject);
    procedure btnLayerPropertiesClick(Sender: TObject);
    procedure btnMoveDownClick(Sender: TObject);
    procedure btnMoveUpClick(Sender: TObject);
    procedure btnRemoveMapClick(Sender: TObject);
    procedure btnResetClick(Sender: TObject);
    procedure chkUniqueDatasetClick(Sender: TObject);
    procedure cmbBaseMapsChange(Sender: TObject);
    procedure cmbBaseMapsExit(Sender: TObject);
    procedure cmbMapsChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure pcMapOptionsChange(Sender: TObject);
    procedure pcMapOptionsChanging(Sender: TObject; var AllowChange: Boolean);
    procedure sgDatasetsSelectCell(Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
    procedure sgLayersSelectCell(Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
    procedure sgMapsDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect;
      State: TGridDrawState);
    procedure sgMapsKeyPress(Sender: TObject; var Key: Char);
    procedure sgMapsSelectCell(Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
    procedure sgMapsTopLeftChanged(Sender: TObject);
    procedure sgMapsMouseWheel(Sender: TObject; Shift: TShiftState; MousePos: TPoint;
      var Handled: Boolean);
    procedure sgMapsClick(Sender: TObject);
  private
    FAddBackgroundLayer: TMenuItem;
    FAddPolygonLayer: TMenuItem;
    FDatasetLegend: TDatasetLegend;
    FdmMap: TdmMap;
    FLayerLegend: TLayerLegend;
    FSpatialSystemsList: TStrings;
    FXPMenu: TXPMenu;
    hlpMapDataset: TOnlineHelp;
    FNavigating: Boolean;
    procedure AddBackgroundLayerClick(Sender: TObject);
    procedure AddBaseSheet(AItem: TSavedMap);
    procedure AddPolygonLayerClick(Sender: TObject);
    procedure CreateDatasetFile(AItem: TSavedMap; AResetPolygons: Boolean);
    procedure CreateDefaultDrawingLayer(const ABaseMapKey: TKeyString);
    procedure CutOffYearChange;
    procedure FindBaseMaps(const AExtension: String);
    procedure GetAvailableBaseMaps;
    procedure GetInitializedBaseMaps;
    procedure GetSpatialRefSystems;
    function ImportFileName(AItem: TSavedMap; const AFileName: String): Integer;
    procedure MapDatasetDeleteItem(Sender: TObject);
    procedure MapDatasetNeedRefresh(Sender: TObject);
    procedure MapLayerDeleteItem(Sender: TObject);
    procedure MapLayerMove(ALayer: TLayerItem; AFromPos, AToPos: Integer);
    procedure MapLayerNeedRefresh(Sender: TObject);
    function PolygonLayersPresent(const ABaseMapKey: TKeyString): Boolean;
    procedure ResetDataset(const ABaseMapKey: String; AResetPolygons: Boolean);
    procedure SetBaseMapDisplayName(AItem: TSavedMap; const ADisplayName: String);
    procedure SetupUserAccess;
    procedure UpdateEnabledMapsTabButtons;
    procedure UpdateMapLayerTabButtons(ARow: Integer);
    procedure ValidateCutOffYear;
    procedure WMUpdateMenuIcons(var Msg:TMessage); message WM_UPDATE_MENU_ICONS;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;
  
//==============================================================================
implementation

{$R *.DFM}

uses
  ApplicationSettings, SpatialRefFuncs, Recorder2000_TLB, GeneralData, Registry,
  Maintbar, FormActions, RegisterMap, APIUtils, Contnrs, DatabaseAccessADO,
  ADOInt, StrUtils;

const
  MIN_YEAR = 1900;
  MAX_YEAR = 2100;

  COL_OBJECT       = 0;
  COL_DEFAULT      = 0;
  COL_INITIALIZED  = 1;
  COL_DISPLAY_NAME = 2;
  COL_BASE_MAP     = 3;

resourcestring
    //Map DataSetSetup
  ResStr_Error  = 'An error has occured while creating a new datset.';
  ResStr_Cancel = 'Are you sure you want to exit?'#13 +
               'You will not be able to run the map until you have created a new dataset.';
  ResStr_NoBaseMap   = 'No valid base maps can be located on the Base Map file path';
  ResStr_DatasetFail = 'There has been a general error setting up the new Dataset.';
  ResStr_AddPolygonlayer = 'Add Polygon Layer';
  ResStr_AddBackgroundLayer = 'Add Background Layer';
  ResStr_CannotImportBaseMap = 'The base map file selected could not be imported';
  ResStr_RemoveMap = 'Are you sure you want to remove the ''%s'' map?';
  ResStr_ResetMapDS = 'Are you sure you wish to reset the ''%s'' map dataset?';

  ResStr_ResetMapDSYesNo = 'This will reset the ''%s'' map dataset.'#13#13 +
                           'Select Yes to reset the base map and background layers, ' +
                           'select All to reset '#13'everything including shared polygon layers,' +
                           ' or select No to cancel.';

  ResStr_MaynotSetBaseMap = 'You may not set the base map to this map because its system ' +
                            'is different to the current system.';

  ResStr_ResetMapDSRemovePolygons = 'This will reset the ''%s'' map dataset.'#13#13 +
                                    'If you proceed this will remove all your polygon layers and any links ' +
                                    'to locations or admin '#13'areas in the database.  This will affect all users.';

  ResStr_RemoveAllDataset = 'This action will remove all datasets except %s.'#13#10 +
                            'Do you want to continue?';

  ResStr_NeedOneMapReset = 'You need at least one map reset to be able to access the other options.';

  //Map Configure
  ResStr_DBFail         = 'Unable to read or write Map Configuration details to the database';
  ResStr_ValidDate      = 'The cut off year entered is not a valid year';
  ResStr_DateOutOfRange = 'The cut off year is not within the acceptable range';
  ResStr_DateEmpty      = 'A cut off year must be entered';

{-==============================================================================
    TdlgMapOptions
===============================================================================}
{-------------------------------------------------------------------------------
}
constructor TdlgMapOptions.Create(AOwner: TComponent);
var
  lFormatSettings: TFormatSettings;
begin
  inherited Create(AOwner);
  
  FdmMap := TdmMap.Create(nil);
  
  pcMapOptions.ActivePageIndex := 0;
  SetGridColumnTitles(
      sgMaps,
      [ResStr_Default, ResStr_Initialised, ResStr_DisplayName, ResStr_BaseMap]);

  FLayerLegend := TLayerLegend.Create(Self, sgLayers);
  with FLayerLegend do begin
    OnDeleteItem := MapLayerDeleteItem;
    OnMoveLayer  := MapLayerMove;
    // Add items in reverse order, so that one can be skipped without changing the end result.
    if AppSettings.UserAccessLevel >= ualAddOnly then begin
      AddPopupMenuItem('-', nil, 0);
      FAddPolygonLayer    := AddPopupMenuItem(ResStr_AddPolygonlayer, AddPolygonLayerClick, 0);
      FAddBackgroundLayer := AddPopupMenuItem(ResStr_AddBackgroundLayer,
                                              AddBackgroundLayerClick, 0);
    end;
  end;
  FDatasetLegend := TDatasetLegend.Create(Self, sgDatasets);
  FDatasetLegend.OnDeleteItem := MapDatasetDeleteItem;

  SetupUserAccess;
  GetSpatialRefSystems;
  GetAvailableBaseMaps;
  GetInitializedBaseMaps;

  //Help Setup
  hlpMapDataset := TOnlineHelp.Create(Self.Handle);
  OnHelp := hlpMapDataset.OnHelpReplacement;
  Self.HelpContext                  := IDH_MAPDATASET;
  tsDistributionSymbols.HelpContext := IDH_MAPOPTIONSDIST;
  tsMapLayers.HelpContext           := IDH_MAPOPTIONSPOLY;
  tsBaseMap.HelpContext             := IDH_MAPOPTIONSBASE;

  lFormatSettings.ShortDateFormat := 'yyyy';
  eCutOffYear.Text := DateTimeToStr(AppSettings.CutOffDate, lFormatSettings);
  chkUniqueDataset.Checked := AppSettings.MapSingleDataset;

  UpdateEnabledMapsTabButtons;
  // Initialise XP Appearance for menus
  FXPMenu := TXPMenu.Create(Self);
  with FXPMenu do begin
    XPControls := [xcMainMenu, xcPopupMenu];
    Active     := AppSettings.ShowMenuIcons;
    Gradient   := AppSettings.GraduatedMenus;
  end;
end;  // TdlgMapOptions.Create

{-------------------------------------------------------------------------------
}
destructor TdlgMapOptions.Destroy;
var
  i: Integer;
begin
  // Clear the TBaseMap objects
  with cmbBaseMaps.Items do
    for i := 0 to Count - 1 do Objects[i].Free;
  cmbBaseMaps.Clear;

  // Clear the TSavedMap objects.
  with sgMaps do
    for i := 1 to RowCount - 1 do
      if Assigned(Objects[COL_OBJECT, i]) then Objects[COL_OBJECT, i].Free;

  FreeAndNil(FSpatialSystemsList);
  hlpMapDataset.Free;
  FdmMap.Free;

  FreeAndNil(FDatasetLegend);
  FreeAndNil(FLayerLegend);

  // Refresh Map sub-menu
  AppSettings.UpdateMapWindowSelectors;

  inherited Destroy;
end;  // TdlgMapOptions.Destroy

{-------------------------------------------------------------------------------
}
procedure TdlgMapOptions.AddBackgroundLayerClick(Sender: TObject);
var
  lMapWindow: TfrmMap;
  lItem, lMapItem: TBackgroundLayerItem;
begin
  inherited;

  lItem := TBackgroundLayerItem.CreateNew(FLayerLegend.BaseMapKey);
  if Assigned(lItem) then
  begin
    FLayerLegend.AddItem(lItem);
    // Now see if the Map window, if any, need to be updated too.
    lMapWindow := dmFormActions.MapWindow(FLayerLegend.BaseMapKey);
    if Assigned(lMapWindow) then begin
      lMapItem := TBackgroundLayerItem.Create;
      lMapItem.Assign(lItem);
      // AddItem causes OnChange event, which gets all event on items to be relinked.
      lMapWindow.LayerLegend.AddItem(lMapItem);
    end;
  end;
end;  // TdlgMapOptions.AddBackgroundLayerClick

{-------------------------------------------------------------------------------
  Attaches the selected Base sheet to the dataset and writes the Base Map details to the
      MAP_SHEET Table so that it can be removed if necessary.
}
procedure TdlgMapOptions.AddBaseSheet(AItem: TSavedMap);
var
  lKey: TKeyString;
  lQuery:string;
begin
  { And add the Base Sheet - we have already checked for its existance }
  MapCheck(ImportFileName(AItem, AppSettings.BaseMapPath + AItem.OriginalFileName),
           ResStr_CannotImportBaseMap);

  try
    lKey := dmGeneralData.GetNextKey('MAP_SHEET', 'Map_Sheet_Key');
    lQuery:=Format(
                'INSERT INTO Map_Sheet (Map_Sheet_Key, Sheet_Name, Sheet_Type, File_Name, ' +
                'Cut_In_Scale, Cut_Out_Scale, Sheet_Displayed, New_Data, Modified_Data, ' +
                'Dataset_Sheet_Name, Dataset_Sheet_FileName, Dataset_Sheet_Order, ' +
                'Computer_ID, Base_Map_Key, Entered_By)' +
                'VALUES(''%s'', %s, 0, %s, ''1:100000000'', ''1:1'', 1, 1, 0, %s, ' +
                '%s, 0, Host_Name(), ''%s'', ''%s'')',
                [lKey,QuoteEscapeStr(AItem.DisplayName), QuotedStr(AItem.OriginalFileName),
                QuoteEscapeStr(AItem.DisplayName), QuotedStr(AItem.OriginalFileName),
                AItem.BaseMapKey, AppSettings.UserID]);
    dmDatabase.ExecuteSQL(lQuery);
  except
    on EDatabaseError do begin
      Raise EDatasetSetupError.CreateNonCritical(ResStr_DBWriteFail);
    end;
  end;
end;  // TdlgMapOptions.AddBaseSheet

{-------------------------------------------------------------------------------
}
procedure TdlgMapOptions.AddPolygonLayerClick(Sender: TObject);
var
  lMapWindow: TfrmMap;
  lItem, lMapItem: TPolygonLayerItem;
begin
  inherited;

  lItem := TPolygonLayerItem.CreateNew(FLayerLegend.BaseMapKey);
  if Assigned(lItem) then
  begin
    FLayerLegend.AddItem(lItem);
    // Now see if the Map window, if any, need to be updated too.
    lMapWindow := dmFormActions.MapWindow(FLayerLegend.BaseMapKey);
    if Assigned(lMapWindow) then
    begin
      lMapItem := TPolygonLayerItem.Create;
      lMapItem.Assign(lItem);
      // AddItem causes OnChange event, which gets all event on items to be relinked.
      lMapWindow.LayerLegend.AddItem(lMapItem);
    end;
    // Move layer to top. If main map displayed, it will be done automatically.
    FLayerLegend.MoveLayer(FLayerLegend.Count - 1, 0);
  end;
end;  // TdlgMapOptions.AddPolygonLayerClick 

{-------------------------------------------------------------------------------
}
procedure TdlgMapOptions.btnAddMapClick(Sender: TObject);
var
  lItem: TSavedMap;
  lBase: TBaseMap;
begin
  with sgMaps do begin
    // Save current, if editing, before moving on to new row.
    SetBaseMapDisplayName(TSavedMap(Objects[COL_OBJECT, Row]), Cells[COL_DISPLAY_NAME, Row]);

    if Assigned(Objects[COL_OBJECT, RowCount - 1]) then
      RowCount := RowCount + 1;
  
    // Defaults the filename to first item in base maps combobox.
    lItem := TSavedMap.Create;
    lBase := TBaseMap(cmbBaseMaps.Items.Objects[0]);
    with lItem do begin
      FOriginalFileName := lBase.FileName;
      FSpatialSystem    := lBase.SpatialSystem;
      FDescription      := lBase.Description;
      SaveData;
    end;
    Objects[COL_OBJECT, RowCount - 1] := lItem;
  
    // Keep grid in sync.
    Cells[COL_DISPLAY_NAME, RowCount - 1] := ResStr_BaseMapSheet;
    Cells[COL_BASE_MAP,     RowCount - 1] := lItem.OriginalFileName;
    Row := RowCount - 1;
    Col := COL_DISPLAY_NAME;

    // Automatically create files for new map.
    btnResetClick(nil);

    Options    := Options + [goEditing];
    EditorMode := True;
    // Select the whole text.
    if TGridAccessor(sgMaps).InPlaceEditor <> nil then
      with TGridAccessor(sgMaps).InPlaceEditor do begin
        SelStart  := 0;
        SelLength := Length(Text);
      end;
  end;
  UpdateEnabledMapsTabButtons;
end;  // TdlgMapOptions.btnAddMapClick

{-------------------------------------------------------------------------------
}
procedure TdlgMapOptions.btnCloseClick(Sender: TObject);
begin
  if pcMapOptions.ActivePage = tsBaseMap then
    with sgMaps do
      // Save current, if editing, before closing form.
      SetBaseMapDisplayName(TSavedMap(Objects[COL_OBJECT, Row]), Cells[COL_DISPLAY_NAME, Row]);
end;  // TdlgMapOptions.btnCloseClick

{-------------------------------------------------------------------------------
}
procedure TdlgMapOptions.btnDeleteDatasetClick(Sender: TObject);
begin
  FDatasetLegend.DeleteItem(sgDatasets.Row);
end;  // TdlgMapOptions.btnDeleteDatasetClick 

{-------------------------------------------------------------------------------
}
procedure TdlgMapOptions.btnDeleteLayerClick(Sender: TObject);
begin
  FLayerLegend.DeleteItem(sgLayers.Row);
end;  // TdlgMapOptions.btnDeleteLayerClick 

{-------------------------------------------------------------------------------
  Invoke the properties dialog for the selected layer
}
procedure TdlgMapOptions.btnLayerPropertiesClick(Sender: TObject);
begin
  FLayerLegend.EditItemProperties(sgLayers.Row);
end;  // TdlgMapOptions.btnLayerPropertiesClick 

{-------------------------------------------------------------------------------
}
procedure TdlgMapOptions.btnMoveDownClick(Sender: TObject);
begin
  FLayerLegend.MoveLayer(sgLayers.Row, sgLayers.Row + 1);
end;  // TdlgMapOptions.btnMoveDownClick 

{-------------------------------------------------------------------------------
}
procedure TdlgMapOptions.btnMoveUpClick(Sender: TObject);
begin
  FLayerLegend.MoveLayer(sgLayers.Row, sgLayers.Row - 1);
end;  // TdlgMapOptions.btnMoveUpClick 

{-------------------------------------------------------------------------------
}
procedure TdlgMapOptions.btnRemoveMapClick(Sender: TObject);
var
  lProceed: Boolean;
  lBaseMapKey: TKeyString;
  lRow: Integer;
  i: Integer;
  lItem: TSavedMap;
  lMapWindow: TfrmMap;
  lWasDefault: Boolean;
begin
  with sgMaps do begin
    lRow        := Row;
    lItem       := TSavedMap(Objects[COL_OBJECT, lRow]);
    lBaseMapKey := lItem.BaseMapKey;
    SetBaseMapDisplayName(lItem, Cells[COL_DISPLAY_NAME, Row]);
    Application.ProcessMessages;
  end;

  lProceed := True;
  if lItem.ComputerMapKey <> '' then
    lProceed := MessageDlg(Format(ResStr_RemoveMap, [lItem.DisplayName]), mtWarning, [mbYes, mbNo], 0) = mrYes;
  if lProceed then begin
    lMapWindow := dmFormActions.MapWindow(lBaseMapKey);
    if Assigned(lMapWindow) then begin
      lMapWindow.Close;
      lMapWindow.Free;
    end;
    lWasDefault := lItem.IsDefault;

    // Delete all files linked to base map being deleted.
    with dmDatabase.ExecuteSQL('SELECT Dataset_Sheet_FileName, Sheet_Type FROM Map_Sheet ' +
                               'WHERE Base_Map_Key=''' + lBaseMapKey + ''' AND Sheet_Type<>0',
                               True) do
    begin
      while not Eof do begin
        FdmMap.DeleteMapSheetFiles(VarToStr(Fields['Dataset_Sheet_FileName'].Value),
                                   TSheetType(Fields['Sheet_Type'].Value));
        MoveNext;
      end;
      Close;
    end;
    // Delete in tables.
    dmDatabase.ExecuteSQL('DELETE Map_Sheet WHERE Base_Map_Key=''' + lBaseMapKey + '''');
    dmDatabase.ExecuteSQL('DELETE Computer_Map WHERE Computer_Map_Key=''' +
                          lItem.ComputerMapKey + '''');
    dmDatabase.ExecuteSQL('DELETE Base_Map WHERE Base_Map_Key=''' + lBaseMapKey + '''');
  
    // Destroy object.
    FreeAndNil(lItem);

    // Hide combo, or it might be trailing behind where there is no row...
    if cmbBaseMaps.Visible then cmbBaseMaps.Visible := False;

    // Now shrink the grid.
    with sgMaps do begin
      Objects[COL_OBJECT, lRow] := nil;
      if (RowCount > 0) and (RowCount <= FixedRows + 1) and (lRow = FixedRows) then
        // First row
        Rows[lRow].Clear  // Just clear row content
      else begin
        // Further down the grid
        for i := lRow + 1 to RowCount - 1 do  // Copying while overwrite current row i
          Rows[i - 1] := Rows[i];
        RowCount := RowCount - 1;             // Only need to decrease RowCount now
      end;
    end;
    // Delete Map dataset file itself, now that nothing refers to it (presumably...)
    DeleteFile(AppSettings.MapFilePath + lBaseMapKey + '.gds');

    // Enable Remove Map button only if at least one map in grid, i.e. object in row 1.
    UpdateEnabledMapsTabButtons;

    // If just removed default map, but still some available, select a new default.
    if lWasDefault and btnRemoveMap.Enabled then
      TSavedMap(sgMaps.Objects[COL_OBJECT, sgMaps.FixedRows]).IsDefault := True;
  end;

  sgMaps.Invalidate;
  AppSettings.UpdateMapWindowSelectors;
end;  // TdlgMapOptions.btnRemoveMapClick

{-------------------------------------------------------------------------------
}
procedure TdlgMapOptions.btnResetClick(Sender: TObject);
var
  lCursor: TCursor;
  lMapShowing: Boolean;
  lProceed: Integer;
  lClearPolygonLayers: Boolean;
  lItem: TSavedMap;
  lMapWindow: TfrmMap;
begin
  with sgMaps do begin
    lItem := TSavedMap(Objects[COL_OBJECT, Row]);
    SetBaseMapDisplayName(lItem, Cells[COL_DISPLAY_NAME, Row]);
  end;

  // Check there is something to process.
  if (lItem.BaseResetIndex <> lItem.ComputerResetIndex) or
     ((lItem.OriginalFileName <> '') and (lItem.SpatialSystem <> '')) then
  begin
    lProceed := mrNo;
    // Base map and computer map out of synch. Behave as if map wasn't on machine and reset.
    if lItem.BaseResetIndex <> lItem.ComputerResetIndex then
      lProceed := mrYes
    else
    // Check the base map already exists as a map dataset. If not, it's easy.
    // Otherwise, check a few things first.
    if FileExists(AppSettings.MapFilePath + lItem.BaseMapKey + '.gds') then
      if CompareText(lItem.SpatialSystem, lItem.SpatialSystemBeforeReset) = 0 then begin
        // Same Spatial System as before, can choose to keep polygon layers.
        if AppSettings.UserAccessLevel in [ualReadOnly, ualRecorder, ualAddOnly] then
          lProceed := MessageDlg(Format(ResStr_ResetMapDS, [lItem.DisplayName]), mtConfirmation, [mbYes, mbNo], 0)
        else
          lProceed := MessageDlg(Format(ResStr_ResetMapDSYesNo, [lItem.DisplayName]), mtConfirmation, [mbYes, mbAll, mbNo], 0)
      end else begin
        // Change of Spatial System, can't keep polygon layers.
        if AppSettings.UserAccessLevel in [ualReadOnly, ualRecorder, ualAddOnly] then begin
          MessageDlg(ResStr_MaynotSetBaseMap, mtInformation, [mbOK], 0);
          lProceed := mrNo;
        end else begin
          lProceed := MessageDlg(Format(ResStr_ResetMapDSRemovePolygons, [lItem.DisplayName]),
              mtConfirmation, [mbOk, mbCancel], 0);
          // So that both messages have consistent results.
          if lProceed = mrOk then lProceed := mrAll
                             else lProceed := mrNo;
        end;
      end;
  
    // Proceed if user said so, or if dataset doesn't already exist.
    if (lProceed <> mrNo) or
       not FileExists(AppSettings.MapFilePath + lItem.BaseMapKey + '.gds') then
    begin
      try
        lCursor := HourglassCursor;
        lMapShowing := False;
        try
          // Locate Map window for BaseMapKey, and close it if found.
          lMapWindow := dmFormActions.MapWindow(lItem.BaseMapKey);
          if Assigned(lMapWindow) then begin
            lMapShowing := True;
            lMapWindow.Close;
            Application.ProcessMessages;
          end;
          try
            lClearPolygonLayers := (lProceed = mrAll) or
                                    not PolygonLayersPresent(lItem.BaseMapKey);
            // Remove files and records
            ResetDataset(lItem.BaseMapKey, lClearPolygonLayers);
            // Refresh data in Base_Map and Computer_Map
            lItem.ResetData(lClearPolygonLayers);
            // Create new Dataset file
            CreateDatasetFile(lItem, lClearPolygonLayers);
            // Refresh the other tabs too.
            FLayerLegend.Refresh;
            FDatasetLegend.Refresh;
          except
            on EDatasetSetupError do
              Exit;
          end;
        finally
          DefaultCursor(lCursor);
        end;
        if lMapShowing then { Reshow the map }
          TfrmMap.Create(frmMain, lItem.BaseMapKey);
      finally
      end;
    end;
    sgMaps.Invalidate;
  end;
end;  // TdlgMapOptions.btnResetClick

{-------------------------------------------------------------------------------
}
procedure TdlgMapOptions.chkUniqueDatasetClick(Sender: TObject);
var
  lMapWindow: TfrmMap;
begin
  AppSettings.MapSingleDataset := chkUniqueDataset.Checked;
  
  // Grid is empty unless a map window with some datasets is open.
  lMapWindow := dmFormActions.MapWindow(FLayerLegend.BaseMapKey);
  if Assigned(lMapWindow) then
    if chkUniqueDataset.Checked then
      if MessageDlg(Format(ResStr_RemoveAllDataset, [FDatasetLegend[0].Title]), mtConfirmation, [mbYes, mbNo], 0) = mrNo then
      begin
        chkUniqueDataset.Checked := False //don't do anything
      end else begin
        FDatasetLegend.SetUnique;
        lMapWindow.DatasetLegend.SetUnique;  // Will trigger Map refresh.
      end;
end;  // TdlgMapOptions.chkUniqueDatasetClick 

{-------------------------------------------------------------------------------
}
procedure TdlgMapOptions.cmbBaseMapsChange(Sender: TObject);
var
  lBaseMap: TBaseMap;
begin
  lBaseMap := TBaseMap(cmbBaseMaps.Items.Objects[cmbBaseMaps.ItemIndex]);
  with TSavedMap(sgMaps.Objects[COL_OBJECT, sgMaps.Row]) do begin
    FSpatialSystem    := lBaseMap.SpatialSystem;
    FOriginalFileName := lBaseMap.FileName;
    FDescription      := lBaseMap.Description;
    SaveData;
  end;
  sgMaps.Invalidate;
end;  // TdlgMapOptions.cmbBaseMapsChange 

{-------------------------------------------------------------------------------
}
procedure TdlgMapOptions.cmbBaseMapsExit(Sender: TObject);
begin
  cmbBaseMaps.Visible := False;
end;  // TdlgMapOptions.cmbBaseMapsExit 

{-------------------------------------------------------------------------------
}
procedure TdlgMapOptions.cmbMapsChange(Sender: TObject);
var
  lMapWindow: TfrmMap;
  lItem: TDatasetItem;
  i: Integer;
  lColour: TColor;
begin
  with cmbMaps do
    if ItemIndex > -1 then
    begin
      // Changing BaseMapKey will trigger a refresh.
      FLayerLegend.BaseMapKey := TSavedMap(Items.Objects[ItemIndex]).BaseMapKey;
      // Still need to link event on all items.
      for i := 0 to FLayerLegend.Count - 1 do
        FLayerLegend[i].OnNeedRefresh := MapLayerNeedRefresh;

      // Need to clear before repopulating.
      FDatasetLegend.Refresh;
      lMapWindow := dmFormActions.MapWindow(FLayerLegend.BaseMapKey);
      if Assigned(lMapWindow) then
        with lMapWindow.DatasetLegend do
        begin
          for i := 0 to Count - 1 do begin
            lItem := TDatasetItem.Create;
            // Just need those 3 properties. The others are irrelevant in this context.
            lItem.Title   := LegendItems[i].Title;
            lItem.Visible := LegendItems[i].Visible;
            // There is a default colour selected automatically, so remember
            // the right one to use first.
            lColour := LegendItems[i].Colour;
            FDatasetLegend.AddItem(lItem);
            // Set the right colour now.
            lItem.Colour := lColour;
            // And only now link to event.
            lItem.OnNeedRefresh := MapDatasetNeedRefresh;
          end;
      end;
    end;
end;  // TdlgMapOptions.cmbMapsChange 

{-------------------------------------------------------------------------------
  Sends the call to clean out the MAP_SHEET table, then creates and opens a dataset.
  Attaches the selected base sheet Creates a blank Object sheet, including the fields required 
      for storing Static IDs and KeyValues in the internal database. 
}
procedure TdlgMapOptions.CreateDatasetFile(AItem: TSavedMap; AResetPolygons: Boolean);
var
  lExtent: MSExtent;
begin
  // Create a new dataset.
  MapCheck(MapNewDataset(AItem.MapServerLink.MapHandle,
                         PChar(AppSettings.MapFilePath + AItem.BaseMapKey + '.gds'),
                         PChar(AItem.DisplayName),
                         PChar(AItem.Description)),
           AppSettings.MapFilePath + AItem.OriginalFileName);
  { Select it and Set the Background color to white }
  AItem.MapServerLink.ActiveDataset := AItem.BaseMapKey + '.gds';
  MapCheck(MapSetBackColor(AItem.MapServerLink.MapHandle, COLOR_BACKGROUND),
           AppSettings.MapFilePath + AItem.OriginalFileName);

  // Add the Base sheet, or import file, and add to Map Dataset and database.
  AddBaseSheet(AItem);
  
  // Map window is closed, the default layer will be added to Map Dataset when form
  // is re-opened.
  if AResetPolygons then
    CreateDefaultDrawingLayer(AItem.BaseMapKey);

  // Size the map appropriately
  MapCheck(MapGetDatasetExtent(AItem.MapServerLink.MapHandle, lExtent));
  MapCheck(MapZoomToExtents(AItem.MapServerLink.MapHandle, lExtent));
end;  // TdlgMapOptions.CreateDatasetFile 

{-------------------------------------------------------------------------------
  Creates the default polygon layer - the one the user always gets after they reset the map  
}
procedure TdlgMapOptions.CreateDefaultDrawingLayer(const ABaseMapKey: TKeyString);
var
  lKey: String;
begin
  lKey := dmGeneraldata.GetNextKey('MAP_SHEET', 'Map_Sheet_Key');
  dmDatabase.ExecuteSQL(Format(
    'INSERT INTO Map_Sheet (Map_Sheet_Key, Sheet_Name, Sheet_Type, Sheet_Displayed, ' +
    'New_Data, Modified_Data, Dataset_Sheet_Order, Dataset_Sheet_Name, Selected_Colour, ' +
    'Unselected_Colour, Pattern_Index, File_Name, Base_Map_Key, Entered_By) VALUES (' +
    '''%s'', ''Polygon layer'', 3, 1, 1, 1, 1, ''Polygon layer'', %d, %d, %d, ' +
    '''%s.gsf'', ''%s'', ''%s'')',
    [lKey, clRed, clBlue, Ord(bsFDiagonal), lKey, ABaseMapKey, AppSettings.UserID]));
end;  // TdlgMapOptions.CreateDefaultDrawingLayer 

{-------------------------------------------------------------------------------
}
procedure TdlgMapOptions.CutOffYearChange;
begin
  ValidateCutOffYear;
  AppSettings.CutOffDate := EncodeDate(StrToInt(eCutOffYear.text), 1, 1)
end;  // TdlgMapOptions.CutOffYearChange

{-------------------------------------------------------------------------------
}
procedure TdlgMapOptions.FindBaseMaps(const AExtension: String);
var
  lSRec: TSearchRec;
  lBaseMapObject: TBaseMap;
  
  {This function returns an object containing details about the
   base map. If any of the details are missing, returns nil.}
  function GetBaseMapObjectFromFile(const AFileName: String): TBaseMap;
  var
    lRootName: String;
    lSpatialRef: String;
    lDescription: String;
    lStrings: TStringList;
  begin
    lRootName := ExtractWithoutExt(AFileName);
    // Read the .ini file which contains extra information about the map
    if FileExists(AppSettings.BaseMapPath + lRootName + '.ini') then
    begin
      lStrings := TStringList.Create;
      try
        lStrings.LoadFromFile(AppSettings.BaseMapPath + lRootName + '.ini');
        try
           lSpatialRef  := lStrings.Values['system'];
           lDescription := lStrings.Values['Description'];
           {Return nil if either of the values are not present or the spatial reference
            system isn't recognised}
           if (FSpatialSystemsList.IndexOf(lSpatialRef) >= 0) and (lDescription <> '') then
             Result := TBaseMap.Create(AFileName, lSpatialRef, lDescription)
           else
             Result := nil;
        except
          on EStringListError do
            Result := nil;  // Return nil if the stringlist can't read the ini file
        end;
      finally
        lStrings.Free;
      end;
    end else
      Result := nil; // Return nil if the ini file isnt' there
  end;  // GetBaseMapObjectFromFile
  
begin
  try
    // Look for map files with this extension
    if FindFirst(AppSettings.BaseMapPath + AExtension, faAnyFile, lSRec) = 0 then begin
      lBaseMapObject := GetBaseMapObjectFromFile(lSRec.Name);
      // When one is found, create an object with its properties and add it to list
      if Assigned(lBaseMapObject) then
        cmbBaseMaps.Items.AddObject(lBaseMapObject.Description + ' (' +
                                    lBaseMapObject.SpatialSystem + ')',
                                    lBaseMapObject);
  
      while FindNext(lSRec) = 0 do begin
        lBaseMapObject:= GetBaseMapObjectFromFile(lSRec.Name);
        if Assigned(lBaseMapObject) then
          cmbBaseMaps.Items.AddObject(lBaseMapObject.Description + ' (' +
                                      lBaseMapObject.SpatialSystem + ')',
                                      lBaseMapObject);
      end;
    end;
  finally
    FindClose(lSRec);
  end;
end;  // TdlgMapOptions.FindBaseMaps 

{-------------------------------------------------------------------------------
}
procedure TdlgMapOptions.FormClose(Sender: TObject; var Action: TCloseAction);
var
  i: Integer;
begin
  //MapConfig
  CutOffYearChange;
  //dmFormActions.actMapWindow.Enabled:= true;
  Action:=caFree;
  
  // Loop through all opened Map windows, if any, and refresh the sheets.
  for i := 0 to frmMain.MDIChildCount - 1 do
    if frmMain.MDIChildren[i] is TfrmMap then
      TfrmMap(frmMain.MDIChildren[i]).RefreshMapSheets;
  Application.ProcessMessages;
end;  // TdlgMapOptions.FormClose 

{-------------------------------------------------------------------------------
  Populates lbFileNames with filenames of .gsf files in the "Base Map Files" folder.  In order 
      to be a base map the file MUST be of a MapServer sheet format (ie. .gsf file plus the 
      other 3 files of which a MapServer file is made up of).
  For other types of vector sheet to become BaseMapFiles they would need importing into 
      MapServer format, or the filter in the following method would need changing and an 
      'Import' method added. 
}
procedure TdlgMapOptions.GetAvailableBaseMaps;
begin
  FindBaseMaps('*.gsf');
  FindBaseMaps('*.shp');
  if cmbBaseMaps.Items.Count = 0 then
    raise EDatasetSetupError.CreateNonCritical(ResStr_NoBaseMap);
end;  // TdlgMapOptions.GetAvailableBaseMaps 

{-------------------------------------------------------------------------------
}
procedure TdlgMapOptions.GetInitializedBaseMaps;
var
  lItem: TSavedMap;
begin
  with dmDatabase.ExecuteSQL(
        'SELECT BM.Base_Map_Key, BM.Original_Filename, BM.Display_Name, BM.Spatial_System, ' +
        'BM.Reset_Index AS Base_Reset_Index, BM.Original_FileName_Before_Reset, ' +
        'BM.Spatial_System_Before_Reset, CM.Computer_Map_Key, CM.Computer_ID, ' +
        'CM.Reset_Index AS Computer_Reset_Index, CM.Default_Map ' +
        'FROM	Base_Map BM ' +
        'LEFT JOIN Computer_Map CM ON CM.Base_Map_Key = BM.Base_Map_Key ' +
        'AND CM.Computer_ID = Host_Name()', True) do
  begin
    while not Eof do begin
      lItem := TSavedMap.Create;
      with lItem do begin
        // Use the object's fields when initialising, and not the properties.
        if not VarIsNull(Fields['Computer_Map_Key'].Value) then begin
          FComputerMapKey     := Fields['Computer_Map_Key'].Value;
          FIsDefault          := Fields['Default_Map'].Value;
          FComputerResetIndex := Fields['Computer_Reset_Index'].Value;
          FComputerID         := Fields['Computer_ID'].Value;
        end;
        FBaseMapKey       := Fields['Base_Map_Key'].Value;
        FBaseResetIndex   := Fields['Base_Reset_Index'].Value;
        FDisplayName      := Fields['Display_Name'].Value;
        FOriginalFileName := Fields['Original_FileName'].Value;
        FSpatialSystem    := Fields['Spatial_System'].Value;
        FOriginalFileNameBeforeReset := Fields['Original_FileName_Before_Reset'].Value;
        FSpatialSystemBeforeReset    := Fields['Spatial_System_Before_Reset'].Value;
  
        MapServerLink.ActiveDataset := FBaseMapKey + '.gds';
      end;
      // Add rows only if last one has been filled
      if Assigned(sgMaps.Objects[COL_OBJECT, 1]) then
        sgMaps.RowCount := sgMaps.RowCount + 1;
      sgMaps.Objects[COL_OBJECT, sgMaps.RowCount - 1] := lItem;
      MoveNext;
    end;
    Close;
  end;
  // Enable Remove Map button only if at least one map in grid, i.e. assigned object in row 1.
  UpdateEnabledMapsTabButtons;
end;  // TdlgMapOptions.GetInitializedBaseMaps

{-------------------------------------------------------------------------------
  Populates FSpatialSystemsList with strings to represent the systems available. 
}
procedure TdlgMapOptions.GetSpatialRefSystems;
var
  lSystemName: String;
  lSpatialReferenceSystem: IUnknown;
  i: Integer;
begin
  if Assigned(FSpatialSystemsList) then FreeAndNil(FSpatialSystemsList);
  FSpatialSystemsList := TStringList.Create;
  
  { Add all standard systems to the check list box
    By using AddObject a title (OS_GB_TITLE) is associated with a system name }
  with FSpatialSystemsList do begin
    Add(OS_GB);
    Add(OS_NI);
    Add(LAT_LONG);
    Add(UTM);
  end;
  
  // Now add any COM spatial systems
  with AppSettings.ComAddins.SpatialSystemInterfaces do
    for i := 0 to Count - 1 do begin
      lSystemName := (Items[i] as ISpatialReference).SpatialRefSystem;
      // Only add base map systems
      try
        lSpatialReferenceSystem := Items[i] as IBaseMapFormat;
        FSpatialSystemsList.Add(lSystemName);
      except
        on EIntfCastError do;
      end;
    end;
end;  // TdlgMapOptions.GetSpatialRefSystems 

{-------------------------------------------------------------------------------
  Adds a sheet to the map dataset. If this is a MapServer.gsf file then by calling 
      MapAttachSheet or if another format then by calling MapImport.
  Dependencies: FMapHandle, FdmMap
  Side Effects: Returns the return value from the import function so that subsequent error 
      handling can take account of the message 
}
function TdlgMapOptions.ImportFileName(AItem: TSavedMap; const AFileName: String): Integer;
var
  lRet: Integer;
  lExtension: String;
begin
  lExtension := ExtractFileExt(AFileName);
  { If it is a MapServer file it only has to be attached }
  if CompareText(lExtension, '.gsf') = 0 then
    lRet := MapAttachSheet(AItem.MapServerLink.MapHandle, PChar(AFileName))
  else begin
    { Import the file }
    lRet := MapImport(AItem.MapServerLink.MapHandle,
                      PChar(AFileName), PChar(AppSettings.MapFilePath), 0, True);
  end;
  if lRet <> MS_SUCCESS then begin
    Result := lRet;
    Exit;
  end;
  
  { Set the query status of the new sheet as the base map is not clickable}
  MapCheck(MapSetQuerySheet(AItem.MapServerLink.MapHandle,
                            AItem.MapServerLink.SheetTotal - 1, False),
           AFileName);
  Result := MS_SUCCESS;
end;  // TdlgMapOptions.ImportFileName 

{-------------------------------------------------------------------------------
}
procedure TdlgMapOptions.MapDatasetDeleteItem(Sender: TObject);
var
  lMapWindow: TfrmMap;
begin
  lMapWindow := dmFormActions.MapWindow(FLayerLegend.BaseMapKey);
  if Assigned(lMapWindow) then
    // Locate item to delete in Map window from index in FDatasetLegend.
    lMapWindow.DatasetLegend.DeleteItem(FDatasetLegend.IndexOf(TBaseLegendItem(Sender)));
end;  // TdlgMapOptions.MapDatasetDeleteItem 

{-------------------------------------------------------------------------------
}
procedure TdlgMapOptions.MapDatasetNeedRefresh(Sender: TObject);
var
  lMapWindow: TfrmMap;
  lSource, lTarget: TDatasetItem;
begin
  lMapWindow := dmFormActions.MapWindow(FLayerLegend.BaseMapKey);
  if Assigned(lMapWindow) then
  begin
    lSource := TDatasetItem(Sender);
    lTarget := lMapWindow.DatasetLegend[FDatasetLegend.IndexOf(lSource)];
    lTarget.Assign(lSource);
  end;
end;  // TdlgMapOptions.MapDatasetNeedRefresh

{-------------------------------------------------------------------------------
}
procedure TdlgMapOptions.MapLayerDeleteItem(Sender: TObject);
var
  lMapWindow: TfrmMap;
begin
  lMapWindow := dmFormActions.MapWindow(FLayerLegend.BaseMapKey);
  if Assigned(lMapWindow) then
    // Locate item to delete in Map window from index in FLayerLegend.
    lMapWindow.LayerLegend.DeleteItem(FLayerLegend.IndexOf(TBaseLegendItem(Sender)));
end;  // TdlgMapOptions.MapLayerDeleteItem 

{-------------------------------------------------------------------------------
}
procedure TdlgMapOptions.MapLayerMove(ALayer: TLayerItem; AFromPos, AToPos: Integer);
var
  lMapWindow: TfrmMap;
begin
  lMapWindow := dmFormActions.MapWindow(FLayerLegend.BaseMapKey);
  if Assigned(lMapWindow) then
    lMapWindow.LayerLegend.MoveLayer(AFromPos, AToPos);
end;  // TdlgMapOptions.MapLayerMove 

{-------------------------------------------------------------------------------
}
procedure TdlgMapOptions.MapLayerNeedRefresh(Sender: TObject);
var
  lMapWindow: TfrmMap;
  lSource, lTarget: TLayerItem;
begin
  lMapWindow := dmFormActions.MapWindow(FLayerLegend.BaseMapKey);
  if Assigned(lMapWindow) then
  begin
    lSource := TLayerItem(Sender);
    lTarget := lMapWindow.LayerLegend[FLayerLegend.IndexOf(lSource)];
    lTarget.Assign(lSource);
  end;
end;  // TdlgMapOptions.MapLayerNeedRefresh

{-------------------------------------------------------------------------------
}
procedure TdlgMapOptions.pcMapOptionsChange(Sender: TObject);
begin
  // Keep only one combo with map names, just change parent from one tab to the other
  if pcMapOptions.ActivePageIndex > 0 then begin
    lblMap.Parent := pcMapOptions.Pages[pcMapOptions.ActivePageIndex];
    // If the selected row on first page is a reset map, select it in the combo too.
    with cmbMaps do begin
      Parent    := lblMap.Parent;
      TabOrder  := 0;
      ItemIndex := Items.IndexOf(TSavedMap(sgMaps.Objects[COL_OBJECT, sgMaps.Row]).DisplayName);
      if ItemIndex = -1 then ItemIndex := 0;
      cmbMapsChange(nil);
      if CanFocus then SetFocus;
    end;
  end;
  UpdateMapLayerTabButtons(sgLayers.Row);
  btnDeleteDataset.Enabled := Assigned(FDatasetLegend[sgDatasets.Row]);
end;  // TdlgMapOptions.pcMapOptionsChange 

{-------------------------------------------------------------------------------
}
procedure TdlgMapOptions.pcMapOptionsChanging(Sender: TObject; var AllowChange: Boolean);
var
  i: Integer;
  lItem: TSavedMap;
begin
  if not Assigned(sgMaps.Objects[COL_OBJECT, sgMaps.Row]) then
    AllowChange := False
  else begin
    // Allow moving from one tab to another by default. Special case for first page though.
    AllowChange := True;

    if pcMapOptions.ActivePageIndex = 0 then begin
      with sgMaps do begin
        // Don't allow empty cells.
        if EditorMode then
          SetBaseMapDisplayName(TSavedMap(Objects[COL_OBJECT, Row]), Cells[COL_DISPLAY_NAME, Row]);

        // Repopulate the combo in case any display name was changed.
        cmbMaps.Items.Clear;
        for i := 1 to RowCount - 1 do begin
          lItem := TSavedMap(Objects[COL_OBJECT, i]);
          // Only allow base maps saved/reset. New ones need to be reset first.
          if lItem.ComputerMapKey <> '' then cmbMaps.Items.AddObject(lItem.DisplayName, lItem);
        end;
      end;

      // Can't change page if there isn't a reset map present.
      AllowChange := cmbMaps.Items.Count > 0;

      // If can change, see if the combo should be displayed.
      if AllowChange then begin
        cmbMaps.Visible := cmbMaps.Items.Count > 1;
        lblMap.Visible  := cmbMaps.Visible;
        if cmbMaps.Visible then begin
          // Only one map available.
          sgDatasets.Top    :=  36;
          sgDatasets.Height := 153;
          sgLayers.Top      :=  36;
          sgLayers.Height   := 181;
        end else begin
          // More than one map to choose from.
          sgDatasets.Top    :=  16;
          sgDatasets.Height := 173;
          sgLayers.Top      :=  16;
          sgLayers.Height   := 201;
        end;
      end;
    end;
  end;
  if not AllowChange then
    MessageDlg(ResStr_NeedOneMapReset, mtInformation, [mbOk], 0);
end;  // TdlgMapOptions.pcMapOptionsChanging

{-------------------------------------------------------------------------------
  Returns true if there are any polygon layers in the map_sheet table. When resetting a map 
      for the first time, need to detect if we are attaching to the existing layers already 
      created by someone else, or are the first to reset therefore must pick up our own
}
function TdlgMapOptions.PolygonLayersPresent(const ABaseMapKey: TKeyString): Boolean;
begin
  with dmDatabase.ExecuteSQL('SELECT Map_Sheet_Key FROM Map_Sheet WHERE Sheet_Type = 3 ' +
                             'AND Base_Map_Key = ''' + ABaseMapKey + '''', True) do
  begin
    Result := RecordCount > 0;
    Close;
  end;
end;  // TdlgMapOptions.PolygonLayersPresent

{-------------------------------------------------------------------------------
  a) Deletes any old dataset on the Map File Path folder.
  b) Goes through MAP_SHEET table and deletes any files from MapFilePath which may have been
      created by the last dataset. (ie. importing a non-MapServer vector file will create the
      4 MapServer type files in the MapFile Path, stored in the MAP_SHEET table in the 
      DATASET_SHEET_FILENAME field )
  c) Calls to clean out the MAP_SHEET table
  d) Cleans out location boundary and admin area boundaries 
}
procedure TdlgMapOptions.ResetDataset(const ABaseMapKey: String; AResetPolygons: Boolean);
var
  lSQL: String;
begin
  // Remove users polygon drawing layers before removing entries from Map_Sheet.
  if AResetPolygons then begin
    dmDatabase.ExecuteSQL('UPDATE Admin_Boundary SET Object_ID = -1 ' +
                          'WHERE Map_Sheet_Key IN (SELECT Map_Sheet_Key FROM Map_Sheet ' +
                          'WHERE Base_Map_Key = ''' + ABaseMapKey + ''')');

    dmDatabase.ExecuteSQL('UPDATE Location_Boundary SET Object_ID = -1 ' +
                          'WHERE Map_Sheet_Key IN (SELECT Map_Sheet_Key FROM Map_Sheet ' +
                          'WHERE Base_Map_Key = ''' + ABaseMapKey + ''')');
  end;

  // Only proceed is necessary
  if FileExists(AppSettings.MapFilePath + ABaseMapKey + '.gds') then
  begin
    DeleteFile(AppSettings.MapFilePath + ABaseMapKey + '.gds');

    if AResetPolygons then
      // Global reset for base map.
      lSQL := 'SELECT * FROM Map_Sheet ' +
              'WHERE Base_Map_Key = ''' + ABaseMapKey + ''''
    else
      // Local reset, don't mess up other maps using same base map.
      lSQL := 'SELECT * FROM Map_Sheet ' +
              'WHERE Base_Map_Key = ''' + ABaseMapKey + ''' AND Computer_ID = Host_Name()';

    with dmDatabase.ExecuteSQL(lSQL, True) do
      try
        while not Eof do begin
          FdmMap.DeleteMapSheetFiles(VarToStr(Fields['Dataset_Sheet_FileName'].Value),
                                     TSheetType(Fields['Sheet_Type'].Value));
          dmDatabase.ExecuteSQL('DELETE Map_Sheet WHERE Map_Sheet_Key = ''' +
                                Fields['Map_Sheet_Key'].Value + '''');
          MoveNext;
        end;
      finally
        Close;
      end;
  end;
end;  // TdlgMapOptions.ResetDataset

{-------------------------------------------------------------------------------
}
procedure TdlgMapOptions.SetBaseMapDisplayName(AItem: TSavedMap; const ADisplayName: String);
var
  lMapWindow: TfrmMap;
  i: Integer;
begin
  if Trim(ADisplayName) <> '' then begin
    AItem.DisplayName := Trim(ADisplayName);

    dmDatabase.ExecuteSQL(Format('UPDATE Map_Sheet SET Sheet_Name=%s, ' +
                          'Dataset_Sheet_Name=%s, Modified_Data=1 ' +
                          'WHERE Base_Map_Key =''%s'' AND Sheet_Type=0',
                          [QuotedStr(AItem.DisplayName), QuotedStr(AItem.DisplayName),
                           AItem.BaseMapKey]));
    // Refresh the base layer name on appropriate map window, if it's there.
    lMapWindow := dmFormActions.MapWindow(AItem.BaseMapKey);
    if Assigned(lMapWindow) then
      for i := 0 to lMapWindow.LayerLegend.Count - 1 do
        if lMapWindow.LayerLegend[i] is TBaseMapLayerItem then begin
          lMapWindow.LayerLegend[i].Title := AItem.DisplayName;
          Break;
        end;
  end;
end;

{-------------------------------------------------------------------------------
  Enables various pages depending on the user access level
}
procedure TdlgMapOptions.SetupUserAccess;
begin
  //Want to be able to reset map if there is no map
  if not AppSettings.MapDatasetPresent then
    pcMapOptions.ActivePage := tsBaseMap
  else
  if AppSettings.UserAccessLevel in [ualReadOnly, ualRecorder, ualAddOnly] then
  begin
    tsBaseMap.Enabled := True;
    tsMapLayers.Enabled := True;
    tsDistributionSymbols.Enabled := True;
    pcMapOptions.ActivePage := tsDistributionSymbols;
  end;
end;  // TdlgMapOptions.SetupUserAccess 

{-------------------------------------------------------------------------------
}
procedure TdlgMapOptions.sgDatasetsSelectCell(Sender: TObject; ACol, ARow: Integer;
    var CanSelect: Boolean);
begin
  btnDeleteDataset.Enabled := (AppSettings.UserAccessLevel >= ualFullUser) and
                              Assigned(FDatasetLegend[ARow]);
end;  // TdlgMapOptions.sgDatasetsSelectCell

{-------------------------------------------------------------------------------
}
procedure TdlgMapOptions.sgLayersSelectCell(Sender: TObject; ACol, ARow: Integer;
    var CanSelect: Boolean);
begin
  UpdateMapLayerTabButtons(ARow);
end;  // TdlgMapOptions.sgLayersSelectCell

{-------------------------------------------------------------------------------
}
procedure TdlgMapOptions.sgMapsClick(Sender: TObject);
begin
  if FNavigating then begin
    FNavigating := False;
    sgMaps.Col := sgMaps.Col;
  end;
end;  // TdlgMapOptions.sgMapsClick

{-------------------------------------------------------------------------------
}
procedure TdlgMapOptions.sgMapsDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect;
    State: TGridDrawState);
var
  lIdx: Integer;
  lItem: TSavedMap;
begin
  inherited;
  with sgMaps do begin
    Canvas.FillRect(Rect);
    if ARow = 0 then
      DrawChoppedText(Cells[ACol, ARow], Canvas, Rect, 2)
    else begin
      lItem := TSavedMap(Objects[COL_OBJECT, ARow]);
      if Assigned(lItem) then
        case ACol of
          COL_DEFAULT:
              DrawCheckBox(Canvas,
                     Rect.Left + (ColWidths[COL_DEFAULT] - 13) div 2,
                     Rect.Top + (DefaultRowHeight - 13) div 2,
                     lItem.IsDefault, lItem.ComputerMapKey <> '');
          COL_INITIALIZED:
              begin
                Canvas.Brush.Color := MergeColours(clWindow, clBtnFace, 50);
                if (State * [gdSelected, gdFocused] <> []) or (ARow = Row) then
                  Canvas.Brush.Color := MergeColours(Canvas.Brush.Color, clHighlight, 50);
                Canvas.Font.Color :=
                    GetContrastColour(MergeColours(Canvas.Brush.Color, Canvas.Brush.Color, 50));
                Canvas.FillRect(Rect);
                if lItem.NeedReset then lIdx := 22
                                   else lIdx := 21;
                dmFormActions.ilButtons.Draw(Canvas,
                       Rect.Left + (ColWidths[COL_INITIALIZED] - 16) div 2,
                       Rect.Top + (DefaultRowHeight - 16) div 2, lIdx);
              end;
          COL_DISPLAY_NAME:
              DrawChoppedText(lItem.DisplayName, Canvas, Rect, 2);
          COL_BASE_MAP:
              with cmbBaseMaps.Items do
                for lIdx := 0 to Count - 1 do
                  if TBaseMap(Objects[lIdx]).FileName = lItem.OriginalFileName then begin
                    DrawChoppedText(Strings[lIdx], Canvas, Rect, 2);
                    Break;
                  end;
        end;
    end;
  end;
end;  // TdlgMapOptions.sgMapsDrawCell

{-------------------------------------------------------------------------------
}
procedure TdlgMapOptions.sgMapsKeyPress(Sender: TObject; var Key: Char);
var
  lItem: TSavedMap;
  lEditor: TInplaceEdit;
begin
  with sgMaps do begin
    lItem := TSavedMap(Objects[COL_OBJECT, Row]);
    if Assigned(lItem) then
      if Col = COL_DISPLAY_NAME then
        if Key = #27 then begin  // Cancel changes, reset value in grid
          Cells[Col, Row] := lItem.DisplayName;
          EditorMode      := False;  // Hide editor
          Options         := Options - [goEditing];
          sgMapsTopLeftChanged(nil);
          Key := #0;
        end else
        if Key = #13 then begin
          SetBaseMapDisplayName(lItem, Cells[COL_DISPLAY_NAME, Row]);
          EditorMode := False;  // Hide editor
          sgMapsTopLeftChanged(nil);
          Key := #0;
        end else begin
          lEditor := TGridAccessor(sgMaps).InplaceEditor;
          if Assigned(lEditor) and
             (SendMessage(lEditor.Handle, EM_GETLIMITTEXT, 0, 0) <> MAX_TITLE_LENGTH) then
            SendMessage(lEditor.Handle, EM_LIMITTEXT, MAX_TITLE_LENGTH, 0);
        end;
    Invalidate;
  end;
end;  // TdlgMapOptions.sgMapsKeyPress

{-------------------------------------------------------------------------------
}
procedure TdlgMapOptions.sgMapsMouseWheel(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
begin
  FNavigating := True;
end;  // TdlgMapOptions.sgMapsMouseWheel

{-------------------------------------------------------------------------------
}
procedure TdlgMapOptions.sgMapsSelectCell(Sender: TObject; ACol, ARow: Integer; var CanSelect:
    Boolean);
var
  lRect: TRect;
  i: Integer;
  lItem: TSavedMap;
begin
  inherited;
  with sgMaps do begin
    lItem := TSavedMap(Objects[COL_OBJECT, Row]);
    // If no object on current row, it means grid is empty, so exit as there's nothing to do.
    if not Assigned(lItem) then Exit;

    // Save display anme of current item before moving off.
    if (Col = COL_DISPLAY_NAME) and EditorMode and ((Col <> ACol) or (Row <> ARow)) then
      SetBaseMapDisplayName(lItem, Cells[COL_DISPLAY_NAME, Row]);

    // Need to get the object on ARow, as it can be different from the one on Row.
    lItem := TSavedMap(Objects[COL_OBJECT, ARow]);
  
    lRect := CellRect(ACol, ARow);
    case ACol of
      COL_DEFAULT:
          begin
            Options := Options - [goEditing];
            cmbBaseMaps.Visible := False;
            if not FNavigating then
              if lItem.ComputerMapKey <> '' then begin
                for i := 1 to RowCount - 1 do
                  TSavedMap(Objects[COL_OBJECT, i]).IsDefault := False;
                lItem.IsDefault := True;
              end;
          end;
      COL_INITIALIZED:
          begin
            Options := Options - [goEditing];
            cmbBaseMaps.Visible := False;
          end;
      COL_DISPLAY_NAME:
          begin
            cmbBaseMaps.Visible := False;
            Cells[COL_DISPLAY_NAME, ARow] := lItem.DisplayName;
            Options := Options + [goEditing];
            if TGridAccessor(sgMaps).InPlaceEditor <> nil then
              with TGridAccessor(sgMaps).InPlaceEditor do begin
                SelStart  := 0;
                SelLength := Length(Text); // select the text
              end;
            if pcMapOptions.ActivePageIndex = 0 then
              ActiveControl := sgMaps;
          end;
    else
        begin
          Options := Options - [goEditing];
          with cmbBaseMaps do
              // Locate correct item in combo
            for i := 0 to Items.Count - 1 do
              if TBaseMap(Items.Objects[i]).FileName = lItem.OriginalFileName then begin
                ItemIndex := i;
                Break;
              end;
          cmbBaseMaps.Visible := True;
          cmbBaseMaps.SetBounds(lRect.Left + 1 + Left, lRect.Top + 1 + Top,
                                lRect.Right - lRect.Left + 2, lRect.Bottom - lRect.Top + 2);
          if pcMapOptions.ActivePageIndex = 0 then
            ActiveControl := cmbBaseMaps;
        end;
    end;
    Invalidate;
  end;
end;  // TdlgMapOptions.sgMapsSelectCell

{-------------------------------------------------------------------------------
}
procedure TdlgMapOptions.sgMapsTopLeftChanged(Sender: TObject);
var
  lDummy: Boolean;
begin
  inherited;
  // The lDummy parameter is necessary for the function call, variable required.
  sgMapsSelectCell(nil, sgMaps.Col, sgMaps.Row, lDummy);
end;  // TdlgMapOptions.sgMapsTopLeftChanged 

{-------------------------------------------------------------------------------
}
procedure TdlgMapOptions.UpdateEnabledMapsTabButtons;
begin
  btnAddMap.Enabled    := AppSettings.UserAccessLevel >= ualFullUser;
  btnRemoveMap.Enabled := (AppSettings.UserAccessLevel >= ualFullUser) and
                          Assigned(sgMaps.Objects[COL_OBJECT, sgMaps.FixedRows]);
  btnReset.Enabled     := Assigned(sgMaps.Objects[COL_OBJECT, sgMaps.FixedRows]);
end;

{-------------------------------------------------------------------------------
}
procedure TdlgMapOptions.UpdateMapLayerTabButtons(ARow: Integer);
var
  lCanMoveOrDelete: Boolean;
  lCanDeleteItem: Boolean;
begin
  lCanMoveOrDelete := AppSettings.UserAccessLevel > ualAddOnly;
  // In case the grid is empty and user managed to get to that tab, which shouldn't happen...
  lCanDeleteItem := False;

  // This method is called through sgLayersSelectCell which is also called
  // when the form is discarded, but FLayerLegend is gone by then.
  if Assigned(FLayerLegend) and Assigned(FLayerLegend[ARow]) then
    lCanDeleteItem := FLayerLegend[ARow].CanDelete;

  // Set the buttons enabled as appropriate
  btnMoveUp.Enabled      := lCanMoveOrDelete and (ARow > 0);
  btnMoveDown.Enabled    := lCanMoveOrDelete and (ARow < sgLayers.RowCount - 1);
  btnDeleteLayer.Enabled := lCanMoveOrDelete and lCanDeleteItem;
end;  // TdlgMapOptions.UpdateMapLayerTabButtons

{-------------------------------------------------------------------------------
  Does what it says it does  
}
procedure TdlgMapOptions.ValidateCutOffYear;
var
  lNow: TDateTime;
  lDate: TDateTime;
  lYear: Integer;
begin
  lNow := Now;
  try
    if eCutOffYear.Text = '' then
      raise EMapConfigError.CreateValidation(ResStr_DateEmpty, eCutOffYear);
  
    lYear := StrToInt(eCutOffYear.Text);
    if (lYear < MIN_YEAR) or (lYear > MAX_YEAR) then begin
      pcMapOptions.ActivePage := tsDistributionSymbols;
      raise EMapConfigError.CreateValidation(ResStr_DateOutOfRange, eCutOffYear);
    end;
  
    lDate := EncodeDate(StrToInt(eCutOffYear.Text), 1, 1);
    if lDate > lNow then
    begin
      { Invalid Date }
      eCutOffYear.Text := FormatDateTime('yyyy', AppSettings.CutOffDate);
      raise EMapConfigError.CreateValidation(ResStr_ValidDate, eCutOffYear);
    end
  except
    on EConvertError do begin
      pcMapOptions.ActivePage := tsDistributionSymbols;
      raise EMapConfigError.CreateValidation(ResStr_ValidDate, eCutOffYear);
    end;
  end;
end;  // TdlgMapOptions.ValidateCutOffYear 

{-------------------------------------------------------------------------------
  Update menu icon appearnance & XP Menus when requested  
}
procedure TdlgMapOptions.WMUpdateMenuIcons(var Msg:TMessage);
begin
  FXPMenu.Active   := AppSettings.ShowMenuIcons;
  FXPMenu.Gradient := AppSettings.GraduatedMenus;
end;  // TdlgMapOptions.WMUpdateMenuIcons 

{-==============================================================================
    TBaseMap
===============================================================================}
{-------------------------------------------------------------------------------
}
constructor TBaseMap.Create(const AFileName, ASpatialSystem, ADescription: String);
begin
  FFileName      := AFileName;
  FSpatialSystem := ASpatialSystem;
  FDescription   := ADescription;
end;  // TBaseMap.Create 

{-==============================================================================
    TSavedMap
===============================================================================}
{-------------------------------------------------------------------------------
}
constructor TSavedMap.Create;
begin
  inherited Create;
  
  FDisplayName := ResStr_BaseMapSheet;
  FIsDefault   := False;
  FComputerID  := AppSettings.ComputerID;
  
  FMapServerLink := TMapServerLink.Create(nil);
end;  // TSavedMap.Create 

{-------------------------------------------------------------------------------
}
destructor TSavedMap.Destroy;
begin
  FMapServerLink.Free;

  inherited Destroy;
end;  // TSavedMap.Destroy 

{-------------------------------------------------------------------------------
}
function TSavedMap.GetNeedReset: Boolean;
begin
  Result := (FOriginalFileName <> FOriginalFileNameBeforeReset) or
            (FComputerMapKey = '') or (FComputerResetIndex <> FBaseResetIndex);
end;  // TSavedMap.GetNeedReset 

{-------------------------------------------------------------------------------
}
procedure TSavedMap.InsertData;
begin
  FBaseMapKey     := dmGeneraldata.GetNextKey('Base_Map', 'Base_Map_Key');
  FBaseResetIndex := 1;
  dmDatabase.ExecuteSQL(
      Format('INSERT INTO Base_Map (Base_Map_Key, Original_FileName, ' +
             'Spatial_System, Display_Name, Reset_Index, ' +
             'Original_FileName_Before_Reset, Spatial_System_Before_Reset, ' +
             'Entered_By) VALUES (''%s'', %s, ''%s'', %s, 1, %s, ''%s'', ''%s'')',
             [FBaseMapKey, QuotedStr(FOriginalFileName), FSpatialSystem,
             QuotedStr(FDisplayName), QuotedStr(FOriginalFileName), FSpatialSystem,
             AppSettings.UserID]));
end;  // TSavedMap.InsertData 

{-------------------------------------------------------------------------------
}
procedure TSavedMap.ResetData(const AResetAll: Boolean);
begin
  // Increase the Base Map Reset Index only if the file changed, or user asks for a
  // complete reset, including polygon layers.
  if AResetAll or
     (FOriginalFileNameBeforeReset <> FOriginalFileName) or
     (FSpatialSystemBeforeReset <> FSpatialSystem) then
    Inc(FBaseResetIndex);

  // Synchronize values so that NeedReset returns false (until filename changes).
  FComputerResetIndex          := FBaseResetIndex;
  FOriginalFileNameBeforeReset := FOriginalFileName;
  FSpatialSystemBeforeReset    := FSpatialSystem;

  if FComputerMapKey = '' then begin
    with dmDatabase.ExecuteSQL('SELECT Count(*) AS Total FROM Computer_Map WHERE ' +
                               'Computer_ID = Host_Name() AND Default_Map = 1', True) do
    begin
      FIsDefault := Fields['Total'].Value = 0;
      Close;
    end;
    // Need to create new record in Computer_Map.
    FComputerMapKey := dmGeneralData.GetNextKey('Computer_Map', 'Computer_Map_Key');

    dmDatabase.ExecuteSQL(Format(
                       'INSERT INTO Computer_Map (Computer_Map_Key, Computer_ID, ' +
                       'Base_Map_Key, Reset_Index, Default_Map, Entered_By) ' +
                       'VALUES (''%s'', Host_Name(), ''%s'', %d, %d, ''%s'')',
                       [FComputerMapKey, FBaseMapKey, FComputerResetIndex,
                        Ord(FIsDefault), AppSettings.UserID]));
  end else
    // If record already exists, just need to update the Reset Index value. Don't touch the
    // others.
    dmDatabase.ExecuteSQL(Format(
                       'UPDATE Computer_Map SET Reset_Index = %d , Changed_By = ''%s'', ' +
                       'Changed_Date = GetDate() WHERE Computer_Map_Key = ''%s''',
                       [FComputerResetIndex, AppSettings.UserID, FComputerMapKey]));

  // Also update the Base_Map table.
  dmDatabase.ExecuteSQL(Format(
                     'UPDATE Base_Map SET Original_FileName_Before_Reset = %s, ' +
                     'Spatial_System_Before_Reset = ''%s'', Reset_Index = %d, ' +
                     'Changed_By = ''%s'', Changed_Date = GetDate() ' +
                     'WHERE Base_Map_Key = ''%s''',
                     [QuotedStr(FOriginalFileName), FSpatialSystem, FBaseResetIndex,
                      AppSettings.UserID, FBaseMapKey]));
end;  // TSavedMap.ResetData 

{-------------------------------------------------------------------------------
}
procedure TSavedMap.SaveData;
begin
  if FBaseMapKey = '' then InsertData
                      else UpdateData;
end;  // TSavedMap.SaveData 

{-------------------------------------------------------------------------------
}
procedure TSavedMap.SetDisplayName(const Value: String);
begin
  if Value <> FDisplayName then begin
    FDisplayName := Value;
    SaveData;
  end;
end;  // TSavedMap.SetDisplayName 

{-------------------------------------------------------------------------------
}
procedure TSavedMap.SetIsDefault(const Value: Boolean);
begin
  if Value <> FIsDefault then begin
    FIsDefault := Value;
    SaveData;
  end;
end;  // TSavedMap.SetIsDefault 

{-------------------------------------------------------------------------------
}
procedure TSavedMap.UpdateData;
var
  lResetIndex: Integer;
begin
  // Add 1 if NeedReset is True, so that database remembers even if map option screen is
  // closed without reset being done. But only if required.
  lResetIndex := FBaseResetIndex;
  if (FOriginalFileNameBeforeReset <> FOriginalFileName) or
     (FSpatialSystemBeforeReset <> FSpatialSystem) then
    Inc(lResetIndex);

  dmDatabase.ExecuteSQL(
      Format('UPDATE Base_Map SET Original_FileName = %s, Display_Name = %s, ' +
             'Spatial_System = ''%s'', Reset_Index = %d, Changed_By = ''%s'', ' +
             'Changed_Date = GetDate() WHERE Base_Map_Key = ''%s''',
             [QuotedStr(FOriginalFileName), QuotedStr(FDisplayName), FSpatialSystem,
             lResetIndex, AppSettings.UserID, FBaseMapKey]));

  if FComputerMapKey <> '' then
    // Don't update Reset_Index here, do it only when Reset is actually requested.
    dmDatabase.ExecuteSQL(
        Format('UPDATE Computer_Map SET Default_Map = %d , Changed_By = ''%s'', ' +
               'Changed_Date = GetDate() WHERE Computer_Map_Key = ''%s''',
               [Ord(FIsDefault), AppSettings.UserID, FComputerMapKey]));
end;  // TSavedMap.UpdateData

end.
