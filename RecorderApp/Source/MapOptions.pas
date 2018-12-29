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
//    //9:30
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
    tsRecover: TTabSheet;
    rgSecure: TRadioGroup;
    btnSecure: TButton;
    lblCurrentObjectSheet: TLabel;
    lblCurrentMapSheetPath: TLabel;
    lblMapSheet: TLabel;
    lblObjectSheetPath: TLabel;
    edWorkstation: TEdit;
    lblWorkStation: TLabel;
    cbRetain: TCheckBox;
    lblUserFile: TLabel;
    lblUserFilePath: TLabel;
    lblBaseMap: TLabel;
    lblCurrentBasemap: TLabel;
    btnFixMaster: TButton;
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
    procedure btnSecureClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure rgSecureClick(Sender: TObject);
    procedure btnFixMasterClick(Sender: TObject);
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
    function QuotedNullStr(Value,cType : string) :string;
    procedure ResetMaps(const AAuto: Boolean);
    procedure BackupMapFiles;
    procedure RestoreAllMapFiles(const AScope: string);
    function GetHostName : string;
    function RemovePathDelim(AFolder: string) :string;
    function RemoveFolderContent(const path,AType: string; AKeepList: TStringList): Boolean;
    function CreateBackup(AFromFolder, AToFolder, AType,ASystem : string) : Boolean;
    function RestoreBackup(AFromFolder, AToFolder,AType : string; AAction : integer): Boolean;
    procedure RecoverMapSheet(const AFilePath: string; ASheetType: string; ACompId: string);
    procedure RecoverMapData(const AScope : string);
    procedure PopulateMapLocBoundary;
    procedure RecoverBdyLinks;
    function GetMasterObjectSheet: string;
    function UpdateComputerMap: Boolean;
    procedure PopulateMapFileMff(const AFilePath : string; ASheetTypes:string; ACheckExists: Boolean; CompId : string);
    function CheckComputerMap: Boolean;
    function CheckObjectSheet : Boolean;
    function ArchiveBackup: string;
    function ExecuteArchivebackup : string;
    function UserFilePath: string;
    function DeleteSpecifiedFiles(Apath, AFilename : string) : Boolean;
    function BaseMapWidowOpen: Boolean;
    function ObjectSheetPathUpdate: boolean;
    function GetMasterWorkStation: string;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

//==============================================================================
implementation

{$R *.DFM}

uses
  ApplicationSettings, SpatialRefFuncs, Recorder2000_TLB, GeneralData, Registry,
  Maintbar, RegisterMap, APIUtils, Contnrs, DatabaseAccessADO,
  ADOInt, StrUtils,FormActions;

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
                           'select All to DELETE '#13'everything including shared polygon layers,' +
                           ' or select No to cancel.';

  ResStr_MaynotSetBaseMap = 'You may not set the base map to this map because its system ' +
                            'is different to the current system.';

  ResStr_ResetMapDSRemovePolygons = 'This will reset the ''%s'' map dataset.'#13#13 +
                                    'If you wish to retain existing map data then do not proceed ' +
                                    'unless you have backup.'#13#13 +
                                    'Proceed ? ';

  ResStr_RemoveAllDataset = 'This action will remove all datasets except %s.'#13#10 +
                            'Do you want to continue?';

  ResStr_NeedOneMapReset = 'You need at least one map reset to be able to access the other options.';


  ResStr_FailedToDelete = 'This file has failed to delete - check that it is not read only';

  ResStr_Continue_Recover = 'This process will copy the maps files from the latest backup and ' +
                            'use these to replace the existing mapping for the selected base map.'#13#10 +
                             #13#10  +
                             'Do you wish to proceed?' ;

  ResStr_Continue_Refresh = 'This process will refresh the links for the maps file  ' +
                            'by creating replacement database entries for the map sheets.'#13#10 +
                             #13#10  +
                             'Do you wish to proceed?' ;


  ResStr_RecoverComplete = 'Restore complete';

  ResStr_AddBaseMap = 'Are you sure you wish to add a new base map ?'#13#10 +
                      'If you have more than one base map please change the default names';

  ResStr_Object_Sheet = 'The object sheet path on this workstation does not appear to be correct'#13#10 +
                        'Do not add new base maps,reset the maps or carry out any other actions until this is fixed.'#13#10 +
                        'See Fix Button in Map Options/Admin ';
  ResStr_ObjectSheetSync = 'This option will bring the object sheet folder on this workstation ' +
                             'in line with that of workstaton %s.'#13#10  +
                             'Do not proceeed unless maps are working correctly on workstation %s'#13#10 +
                             'Proceed ? ';
  ResStr_Admin_No_Master = 'Mapping is not set up. Use Map Options/Reset/All to set up mapping';

  ResStr_Object_Sheet_Fail = 'The changes to the object sheet path did not work'#13#10 +
                            'Changes to the registry may be required';

  ResStr_Admin_Permission  = 'This change can only be made by an Administrator';

  ResStr_BaseMap_Permission  = 'The Map Window must be closed to make these changes.';

  ResStr_Number_Saved = 'Update Complete ''%s'' files have been processed';

  ResStr_NoFilesToBackup = 'There are no files to backup. Backup will ' +
                           'not complete. Open a map to create the files.';

  ResStr_NoFilesToRestore = 'There are no files to restore. ' +
                            'No action has been taken';

  ResStr_RestoreDifWS = 'Background maps from the selected workstation will loaded onto this workstation.'#13#10 +
                        'Do you wish to proceed ?';

  ResStr_Complete = '%s Complete';

  ResStr_Archive_Complete = 'Archived to path %s';

  ResStr_Restore = 'Restore';

  ResStr_Backup = 'Backup';

  ResStr_Archive = 'Archive';
  ResStr_Failed = '''%s'' did not fully complete as '+
                   'there may be no files to backup or you may not have the required permissions.';

  ResStr_DoArchive = 'Backup of %s complete. Do you wish to archive your map file backups now ?';

  // rgButton captions

  Resstr_Button0 =  'Backup Map Data';
  Resstr_Button1 =  'Archive Backups';
  Resstr_Button2 =  'Restore Map Data';
  Resstr_Button3 =  'Restore Background Map Data';
  Resstr_Button4 =  'Relink Map Data';
  Resstr_Button5 =  'Relink Background Map Data';

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
  { But if the file is not there and the database entry is then another db entry will be created
   which we dont want so make sure it isn't there}

  MapCheck(ImportFileName(AItem, AppSettings.BaseMapPath + AItem.OriginalFileName),
           ResStr_CannotImportBaseMap);

  //Changed here for 6.27
  try
    lKey := dmGeneralData.GetNextKey('MAP_SHEET', 'Map_Sheet_Key');
    dmDatabase.ExecuteSQL(Format('Delete From Map_Sheet where Base_map_key = ''%s''' +
                          ' and Sheet_Type = 0 AND Computer_Id = Host_Name()',
                          [AItem.BaseMapKey]));
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
  If MessageDlg(ResStr_AddBaseMap, mtWarning, [mbYes, mbNo], 0) = mrYes then begin
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
      ResetMaps(False);

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
  end;
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
begin
  Resetmaps(False);
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
  //dmFormActions.actMapWindow.Enabled:= True;
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
  if pcMapOptions.ActivePageIndex = 3 then
    lblCurrentBasemap.caption := TSavedMap(sgMaps.Objects[COL_OBJECT, sgMaps.Row]).DisplayName;

  // Keep only one combo with map names, just change parent from one tab to the other
  if pcMapOptions.ActivePageIndex in [1,2] then begin
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
  if not Assigned(sgMaps.Objects[COL_OBJECT, sgMaps.Row])  then
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
          if lItem.ComputerMapKey <> '' then  cmbMaps.Items.AddObject(lItem.DisplayName, lItem);
        end;
      end;

      // Can't change page if there isn't a reset map present.
      AllowChange := cmbMaps.Items.Count > 0;

      // If can change, see if the combo should be displayed.
      if AllowChange then begin
        cmbMaps.Visible := cmbMaps.Items.Count > 0;
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
  Returns True if there are any polygon layers in the map_sheet table. When resetting a map
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


procedure TSavedMap.ResetData(const AResetAll: Boolean);
begin
  // Increase the Base Map Reset Index only if the file changed, or user asks for a
  // complete reset, including polygon layers.
  if AResetAll or
     (FOriginalFileNameBeforeReset <> FOriginalFileName) or
     (FSpatialSystemBeforeReset <> FSpatialSystem) then
   begin
      Inc(FBaseResetIndex);
   end;
  // Synchronize values so that NeedReset returns False (until filename changes).
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
                       'Base_Map_Key, Reset_Index, Default_Map, Entered_By,Object_Sheet_Folder,Master) ' +
                       'VALUES (''%s'', Host_Name(), ''%s'', %d, %d, ''%s'',''%s'',0)',
                       [FComputerMapKey, FBaseMapKey, FComputerResetIndex,
                        Ord(FIsDefault), AppSettings.UserID,AppSettings.ObjectSheetFilePath]));
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


function TdlgMapOptions.QuotedNullStr(Value, cType: string): string;
begin
  if value = '' then
    Result := 'NULL'
  else if CType = 'S' then
    Result := QuotedStr(Value)
  else if uppercase(Value) = 'FALSE' then
    Result := '0'
  else if uppercase(Value) = 'TRUE' then
    Result := '1'
  else
    Result := Value;
end;

procedure TdlgMapOptions.RecoverMapSheet(const AFilePath: string;
                         ASheetType: string; ACompId: string);
var
lPath,lQuery,lCompId,lBaseMapSystem : string;
lSearchRec: TSearchRec;
lFileList,lMapSheetList: TStringList;
lMapSheetKey,lUserId,lSheetName,lFileName,lSheetType,lSWSpatialRef, lNESpatialRef,
    lSpatialRefSystem, lSWLat,lSWLong,lNELat,lNeSpatialRefQualifier,
    lSwSpatialRefQualifier,lCutInScale, lCutOutScale,lSheetDisplayed,lNewData,
    lModifiedData, lDataSetSheetName, lRemoveSheet, lDataSheetFileName,
    lDatasetSheetOrder, lSelectedColour, lUnselectedColour,lPatternIndex,
    lComputerId,lBaseMapKey,lFileBaseMapkey,lNELong,lSpatialSystem : string;
i,lRow : integer;
lItem : TSavedMap;
begin
  lCompid := ACompId;
  if ACompId = 'NULL' then
    lCompId := 'IS NULL'
  else
    lCompId := ' = Host_Name() ';

  lMapSheetList := TStringList.Create;
  with sgMaps do begin
    lRow:= Row;
    lItem := TSavedMap(Objects[COL_OBJECT,lRow]);
    lBaseMapKey := lItem.BaseMapKey;
    lBaseMapSystem := lItem.FSpatialSystem;
  end;

  lquery := 'IF NOT EXISTS (SELECT * FROM MAP_SHEET WHERE DATASET_SHEET_FILENAME = ' +
            ' %s ) INSERT INTO MAP_SHEET ' +
            '(MAP_SHEET_KEY,USER_ID,SHEET_NAME,FILE_NAME,SHEET_TYPE,' +
            'SW_SPATIAL_REF,NE_SPATIAL_REF,SPATIAL_REF_SYSTEM,SW_LAT,SW_LONG,' +
            'NE_LAT,NE_LONG,NE_SPATIAL_REF_QUALIFIER,SW_SPATIAL_REF_QUALIFIER, ' +
            'CUT_IN_SCALE,CUT_OUT_SCALE,SHEET_DISPLAYED,ENTERED_BY,ENTRY_DATE, ' +
            'NEW_DATA,MODIFIED_DATA,DATASET_SHEET_NAME,REMOVE_SHEET,DATASET_SHEET_FILENAME, ' +
            'DATASET_SHEET_ORDER,SELECTED_COLOUR,UNSELECTED_COLOUR,PATTERN_INDEX,' +
            'COMPUTER_ID,BASE_MAP_KEY) ' +
            'VALUES (''%s'',%s,%s,%s,%s,' +
            '%s,%s,%s,%s,%s, ' +
            '%s,%s,%s,%s,' +
            '%s,%s,%s,''%s'',getdate(),' +
            '%s,%s,%s,%s,%s,' +
            '%s,%s,%s,%s,'+
            '%s,''%s'')';

  if not cbRetain.checked then dmDatabase.ExecuteSQL(Format('Delete FROM MAP_SHEET WHERE Sheet_type in(%s)' +
                              ' AND COMPUTER_ID %s AND ' +
                              ' Base_Map_Key = ''' + lBaseMapKey + '''',[ASheetType,lCompId]));
  lFileList := TStringList.Create;
  lPath := AFilePath;
  if FindFirst(lPath + '*.mff', 0, lSearchRec) = 0 then begin
    lFileList.add (lPath + lSearchRec.Name);
    while FindNext(lSearchRec) = 0 do
      lFileList.add (lPath + lSearchRec.Name);
    FindClose(lSearchRec);
  end;

  for i := 0 to lFileList.count-1 do begin
    if FileExists(lPath + ExtractWithoutExt(lFileList[i]) + '.gsf') then begin
      lMapSheetList.LoadFromFile(lFileList[i]);
      lUserId := QuotedNullStr(lMapSheetList.Values['USER_ID'],'S');
      lSheetName :=  QuotedNullStr(lMapSheetList.Values['SHEET_NAME'],'S');
      lFileName := QuotedNullStr(lMapSheetList.Values['FILE_NAME'],'S');
      lSheetType := QuotedNullStr(lMapSheetList.Values['SHEET_TYPE'],'');
      lSWSpatialRef := QuotedNullStr(lMapSheetList.Values['SW_SPATIAL_REF'],'S');
      lNESpatialRef := QuotedNullStr(lMapSheetList.Values['NE_SPATIAL_REF'],'S');
      lSpatialRefSystem := QuotedNullStr(lMapSheetList.Values['SPATIAL_REF_SYSTEM'],'S');
      lSWLat := QuotedNullStr(lMapSheetList.Values['SW_LAT'],'');
      lSWLong := QuotedNullStr(lMapSheetList.Values['SW_LONG'],'');
      lNELat := QuotedNullStr(lMapSheetList.Values['NE_LAT'],'');
      lNELong := QuotedNullStr(lMapSheetList.Values['NE_LONG'],'');
      lNeSpatialRefQualifier := QuotedNullStr(lMapSheetList.Values['NE_SPATIAL_REF_QUALIFIER'],'S');
      lSwSpatialRefQualifier  := QuotedNullStr(lMapSheetList.Values['SW_SPATIAL_REF_QUALIFIER'],'S');
      lCutInScale := QuotedNullStr(lMapSheetList.Values['CUT_IN_SCALE'],'S');
      lCutOutScale := QuotedNullStr(lMapSheetList.Values['CUT_OUT_SCALE'],'S');
      lSheetDisplayed := QuotedNullStr(lMapSheetList.Values['SHEET_DISPLAYED'],'');
      lNewData := QuotedNullStr(lMapSheetList.Values['NEW_DATA'],'');
      lModifiedData := QuotedNullStr(lMapSheetList.Values['MODIFIED_DATA'],'');
      lDataSetSheetName := QuotedNullStr(lMapSheetList.Values['DATASET_SHEET_NAME'],'S');
      lRemoveSheet := QuotedNullStr(lMapSheetList.Values['REMOVE_SHEET'],'');
      lDataSheetFileName := QuotedNullStr(lMapSheetList.Values['DATASET_SHEET_FILENAME' ],'S');
      lDatasetSheetOrder := QuotedNullStr(lMapSheetList.Values['DATASET_SHEET_ORDER'],'');
      lSelectedColour := QuotedNullStr(lMapSheetList.Values['SELECTED_COLOUR'],'');
      lUnselectedColour := QuotedNullStr(lMapSheetList.Values['UNSELECTED_COLOUR'],'');
      lPatternIndex := QuotedNullStr(lMapSheetList.Values['PATTERN_INDEX'],'');
      lComputerId := QuotedNullStr(lMapSheetList.Values['COMPUTER_ID'],'S');
      lFileBaseMapKey := lMapSheetList.Values['BASE_MAP_KEY'];
      lSpatialSystem := lMapSheetList.Values['SPATIAL_SYSTEM'];
      //Only proceed if the spatial systems are the same.
      if lBaseMapSystem = lSpatialSystem then begin
        lMapSheetKey := dmGeneraldata.GetNextKey('MAP_SHEET', 'Map_Sheet_Key');
        dmDatabase.ExecuteSQL(Format(lQuery,
        [lDataSheetFileName,lMapSheetKey,lUserId,lSheetName,lFileName,lSheetType,
        lSWSpatialRef,lNESpatialRef,lSpatialRefSystem,lSWLat,lSWLong,
        lNELat,lNELong,lNeSpatialRefQualifier,lSwSpatialRefQualifier,
        lCutInScale,lCutOutScale,lSheetDisplayed,AppSettings.UserID,
        lNewData,lModifiedData,lDataSetSheetName,lRemoveSheet,lDataSheetFileName,
        lDatasetSheetOrder,lSelectedColour,lUnselectedColour,lPatternIndex,
        ACompId,lBaseMapKey]),False);
      end;
    end;
  end;
  lMapSheetList.Free;
  lFileList.Free;
end;

procedure TdlgMapOptions.ResetMaps(const AAuto: Boolean);
var
  lCursor: TCursor;
  lMapShowing,lClearPolygonLayers: Boolean;
  lProceed: Integer;
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
    if (lItem.BaseResetIndex <> lItem.ComputerResetIndex) or (AAuto) then
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
    if (lProceed <> mrNo) or not (FileExists(AppSettings.MapFilePath + lItem.BaseMapKey + '.gds')) then
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
            if lClearPolygonLayers then begin
              // Always make a machine doing a full reset the master we are not concerned about the basemap
              dmDatabase.ExecuteSQL('Update Computer_Map set master = 0');
              dmDatabase.ExecuteSQL('Update Computer_Map set master = 1 where Computer_Id = host_name()');
            end;
            // check that the Object sheet path is OK
            CheckObjectSheet;
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
  end;
  sgMaps.Invalidate;
end;

procedure TdlgMapOptions.btnSecureClick(Sender: TObject);
begin
  if rgSecure.ItemIndex = 0 then
    BackupMapFiles
  else begin
    if (AppSettings.UserAccessLevel = ualAdmin) and (Not BaseMapWidowOpen) then begin
      case rgSecure.ItemIndex of
        1 : ExecuteArchiveBackup;
        2 : RestoreAllMapFiles('');
        3 : RestoreAllMapFiles('MS');
        4 : RecoverMapData('');
        5 : RecoverMapData('MS');
      end;
    end else
      if (Not BaseMapWidowOpen)then
        MessageDlg(ResStr_Admin_Permission,mtInformation, [mbOk], 0)
      else  MessageDlg(ResStr_BaseMap_Permission,mtInformation, [mbOk], 0);
  end;
end;

procedure TdlgMapOptions.PopulateMapFileMff(const AFilePath : string; ASheetTypes:string; ACheckExists: Boolean; CompId : string);
var
lMSFileName : string;
lString : TStringList;
begin
  if ASheetTypes <> '1' then
    DeleteSpecifiedFiles(AppSettings.ObjectSheetFilePath, '*.mff')
  else DeleteSpecifiedFiles(AppSettings.MapFilePath, '*.mff');
  with dmDatabase.ExecuteSQL(Format('SELECT MS.*,BM.SPATIAL_SYSTEM ' +
                                   ' FROM Map_Sheet MS INNER JOIN ' +
                                   ' BASE_MAP BM ON BM.BASE_MAP_KEY = MS.BASE_MAP_KEY' +
                                   ' WHERE ISNull(Dataset_Sheet_FileName,'''') <>'''' AND Sheet_Type in(%s) ' +
                                   ' and Computer_Id %s',[ASheetTypes,CompId]), True) do
  begin
    while not Eof do begin
        // Get Filename with path to where it should be found.
        lMSFileName := AFilePath +
                       ExtractWithoutExt(Fields['Dataset_Sheet_FileName'].Value) + '.mff';
        if (not FileExists(lMSFileName)) or (not ACheckExists) then
        begin
          lString := TStringList.Create;
          lString.add('MAP_SHEET_KEY=' + VarToStr(Fields['MAP_SHEET_KEY'].Value));
          lString.add('USER_ID=' + VarToStr(Fields['USER_ID'].Value));
          lString.add('SHEET_NAME=' + VarToStr(Fields['SHEET_NAME'].Value));
          lString.add('FILE_NAME=' + VarToStr(Fields['FILE_NAME'].Value));
          lString.add('SHEET_TYPE=' + VarToStr(Fields['SHEET_TYPE'].Value));
          lString.add('SW_SPATIAL_REF=' + VarToStr(Fields['SW_SPATIAL_REF'].Value));
          lString.add('NE_SPATIAL_REF=' + VarToStr(Fields['NE_SPATIAL_REF'].Value));
          lString.add('SPATIAL_REF_SYSTEM=' + VarToStr(Fields['SPATIAL_REF_SYSTEM'].Value));
          lString.add('SW_LAT=' + VarToStr(Fields['SW_LAT'].Value));
          lString.add('SW_LONG=' + VarToStr(Fields['SW_LONG'].Value));
          lString.add('NE_LAT=' + VarToStr(Fields['NE_LAT'].Value));
          lString.add('NE_LONG=' + VarToStr(Fields['NE_LONG'].Value));
          lString.add('NE_SPATIAL_REF_QUALIFIER=' + VarToStr(Fields['NE_SPATIAL_REF_QUALIFIER'].Value));
          lString.add('SW_SPATIAL_REF_QUALIFIER=' + VarToStr(Fields['SW_SPATIAL_REF_QUALIFIER'].Value));
          lString.add('CUT_IN_SCALE=' + VarToStr(Fields['CUT_IN_SCALE'].Value));
          lString.add('CUT_OUT_SCALE=' + VarToStr(Fields['CUT_OUT_SCALE'].Value));
          lString.add('SHEET_DISPLAYED=' + VarToStr(Fields['SHEET_DISPLAYED'].Value));
          lString.add('NEW_DATA=' + VarToStr(Fields['NEW_DATA'].Value));
          lString.add('MODIFIED_DATA=' + VarToStr(Fields['MODIFIED_DATA'].Value));
          lString.add('DATASET_SHEET_NAME=' + VarToStr(Fields['DATASET_SHEET_NAME'].Value));
          lString.add('REMOVE_SHEET=' + VarToStr(Fields['REMOVE_SHEET'].Value));
          lString.add('DATASET_SHEET_FILENAME=' + VarToStr(Fields['DATASET_SHEET_FILENAME'].Value));
          lString.add('DATASET_SHEET_ORDER=' + VarToStr(Fields['DATASET_SHEET_ORDER'].Value));
          lString.add('SELECTED_COLOUR=' + VarToStr(Fields['SELECTED_COLOUR'].Value));
          lString.add('UNSELECTED_COLOUR=' + VarToStr(Fields['UNSELECTED_COLOUR'].Value));
          lString.add('PATTERN_INDEX=' + VarToStr(Fields['PATTERN_INDEX'].Value));
          lString.add('COMPUTER_ID=' + VarToStr(Fields['COMPUTER_ID'].Value));
          lString.add('BASE_MAP_KEY=' + VarToStr(Fields['Base_Map_Key'].Value));
          lString.add('SPATIAL_SYSTEM=' + VarToStr(Fields['Spatial_System'].Value));
          lString.SaveToFile(lMSFileName);
          lString.Free;
        end; //end of write
      MoveNext;
    end; // end of while
    Close;
  end; //end of with
end;

// Procedure will create a .rbd (recorder boundary)file in the Map file folder for entries
// in Map Sheet table for this work station
procedure TdlgMapOptions.PopulateMapLocBoundary;
var
  lString,lObsSheets,lFileList : TStringList;
  lQuery,lData,lFileName,lMSFileName: string;
  i,x: integer;
begin
  lObsSheets := TStringlist.create;
  lFileList :=  TStringlist.create;
  lQuery := 'Select ''LOCATION'',MS.DATASET_SHEET_FILENAME,LB.MAP_SHEET_KEY,LB.LOCATION_BOUNDARY_KEY,' +
            'LB.LOCATION_KEY,LB.FROM_VAGUE_DATE_START,LB.FROM_VAGUE_DATE_END,LB.FROM_VAGUE_DATE_TYPE,' +
            'LB.TO_VAGUE_DATE_START,LB.TO_VAGUE_DATE_END,LB.TO_VAGUE_DATE_TYPE, ' +
            'LB.VERSION,LB.[OBJECT_ID],LB.SYSTEM_SUPPLIED_DATA,BM.SPATIAL_SYSTEM,NULL,NULL ' +
            'FROM MAP_SHEET MS INNER JOIN LOCATION_BOUNDARY LB  ON ' +
            'MS.MAP_SHEET_KEY = LB.MAP_SHEET_KEY ' +
            'INNER JOIN BASE_MAP BM ON BM.BASE_MAP_KEY = MS.BASE_MAP_KEY ' +
            'WHERE MS.DATASET_SHEET_FILENAME = ''%s'''  +
            'UNION Select ''ADMIN'',MS.DATASET_SHEET_FILENAME,AB.MAP_SHEET_KEY,AB.ADMIN_BOUNDARY_KEY,' +
            'AB.ADMIN_AREA_KEY,NULL,NULL,NULL, ' +
            'NULL,NULL,NULL,NULL,NULL,AB.SYSTEM_SUPPLIED_DATA,BM.SPATIAL_SYSTEM,AB.DATE_FROM,AB.DATE_TO ' +
            'FROM MAP_SHEET MS INNER JOIN ADMIN_BOUNDARY AB  On ' +
            'MS.MAP_SHEET_KEY = AB.MAP_SHEET_KEY ' +
            'INNER JOIN BASE_MAP BM ON BM.BASE_MAP_KEY = MS.BASE_MAP_KEY ' +
            'WHERE MS.DATASET_SHEET_FILENAME = ''%s''';

  {Allow for the fact that links may have been deleted so that a .rbd may be there, but not
  required, so delete all a start again}
  DeleteSpecifiedFiles(appsettings.ObjectSheetFilePath, '*.rbd');

  With dmDatabase.ExecuteSQL('SELECT MS.DATASET_SHEET_FILENAME ' +
                             'FROM LOCATION_BOUNDARY LB INNER JOIN Map_Sheet MS ' +
                             'ON MS.MAP_SHEET_KEY = LB.MAP_SHEET_KEY '  +
                             'UNION SELECT MS.DATASET_SHEET_FILENAME ' +
                             'FROM ADMIN_BOUNDARY AB INNER JOIN Map_Sheet MS ' +
                             'ON MS.MAP_SHEET_KEY = AB.MAP_SHEET_KEY', True) do
  begin
    While not Eof do begin
      lObsSheets.Add(VarToStr(Fields['DATASET_SHEET_FILENAME'].Value));
      movenext;
    end; //end of while
    close;
  end; // end of Dataset with
  { For each object sheet get the file name and the data and write the
   data to the .rbd file. One line for each entry in the Location or Admin Table
   Not that a Location may have links to mutiple polygons.
  }
  for i := 0 to lObsSheets.count - 1 do begin
    lFileName := lObsSheets[i];
    with dmDatabase.ExecuteSQL(Format(lQuery,[lFileName,lFileName]), True) do
    begin
      lMSFileName := AppSettings.ObjectSheetFilePath +
                   ExtractWithoutExt(lFileName) + '.rbd';
      lString := TStringList.Create;
      while not Eof do begin
        for x := 0 to 16 do
          lData := lData +  VarToStr(Fields[x].Value)+ ',';
        lString.add(lData);
        lData := '';
        movenext;
      end; //end of file read
      close;
    end; // end of with
    try
      lString.SaveToFile(lMSFileName);
    finally
      lString.Free;
    end;
  end; // end of for
  lObsSheets.Free;
  lFileList.Free;
end;

// recovers the link from the Location and Admin Area to Map_Sheets
procedure TdlgMapOptions.RecoverBdyLinks;
var
  lBaseMapKey,lQueryLoc,lQueryAdm,lFileName,lMapSheetKey,lBoundaryKey: string;
  lSearchRec: TSearchRec;
  lFileList: TStringList;
  x,i,lRow: integer;
  lItem: TSavedMap;
  lMapSheetList: TStringList;
  lData: TStrings;
  lQueryDelete, lSystem : String;
begin
  lData := TStringList.Create;
  lQueryLoc := 'IF EXISTS (SELECT * FROM LOCATION WHERE LOCATION_KEY = ''%s'')' +
               ' AND NOT EXISTS (SELECT * FROM LOCATION_BOUNDARY WHERE LOCATION_BOUNDARY_KEY = ''%s'')' +
               'Insert Into Location_Boundary (LOCATION_BOUNDARY_KEY,LOCATION_KEY,' +
               'FROM_VAGUE_DATE_START,FROM_VAGUE_DATE_END,FROM_VAGUE_DATE_TYPE,' +
               'TO_VAGUE_DATE_START,TO_VAGUE_DATE_END,TO_VAGUE_DATE_TYPE,VERSION,' +
               'OBJECT_ID,SYSTEM_SUPPLIED_DATA,ENTERED_BY,ENTRY_DATE,MAP_SHEET_KEY) VALUES(''%s'',''%s'',' +
               '%s,%s,''%s'',%s,%s,''%s'',%s,''%s'',%s,''%s'',GetDate(),''%s'')';

  lQueryAdm := 'IF NOT EXISTS (SELECT * FROM ADMIN_BOUNDARY WHERE ADMIN_BOUNDARY_KEY = ''%s'')' +
               'Insert Into ADMIN_BOUNDARY (ADMIN_BOUNDARY_KEY,ADMIN_AREA_KEY,DATE_FROM,DATE_TO,SYSTEM_SUPPLIED_DATA,' +
               'ENTERED_BY,ENTRY_DATE,MAP_SHEET_KEY) VALUES(''%s'',''%s'',Cast(left(''%s'',10) AS DateTime),' +
               'Cast(left(''%s'',10) AS DateTime),%s,''%s'',GetDate(),''%s'')';

  lQueryDelete := 'DELETE FROM %s_Boundary FROM %s_Boundary INNER JOIN MAP_SHEET MS ON ' +
                  'MS.MAP_SHEET_KEY = %s_Boundary.MAP_SHEET_KEY INNER JOIN BASE_MAP BM ' +
                  'ON BM.BASE_MAP_KEY = MS.BASE_MAP_KEY WHERE BM.SPATIAL_SYSTEM = ''%s''';

  lMapSheetList := TStringList.Create;
  with sgMaps do begin
    lRow:= Row;
    lItem := TSavedMap(Objects[COL_OBJECT,lRow]);
    lBaseMapKey := lItem.BaseMapKey;
    lSystem := lItem.SpatialSystem;
  end;
  lFileList := TStringList.Create;
  if FindFirst(Appsettings.ObjectSheetFilePath + '*.rbd', 0, lSearchRec) = 0 then begin
    lFileList.add (lSearchRec.Name);
    while FindNext(lSearchRec) = 0 do
      lFileList.add (lSearchRec.Name);
    FindClose(lSearchRec);
  end;
  if (not cbRetain.checked) and (lFileList.count > -1) then begin //dont delete if if retain is true
    dmDatabase.ExecuteSQL(Format(lQueryDelete,['Location','Location','Location',lSystem]));
    dmDatabase.ExecuteSQL(Format(lQueryDelete,['Admin','Admin','Admin',lSystem]));
  end;
  for i := 0 to lFileList.count-1 do begin
    lFileName := ExtractWithoutExt(lFileList[i]) + '.gsf';
    if FileExists(Appsettings.ObjectSheetFilePath+ lFileName) then begin
      with dmDatabase.ExecuteSQL(Format('Select Map_Sheet_Key from MAP_SHEET INNER JOIN ' +
                                'BASE_MAP BM ON BM.Base_Map_Key = MAP_SHEET.BASE_MAP_KEY ' +
                                'WHERE BM.SPATIAL_SYSTEM = ''%s'' AND ' +
                                'SHEET_TYPE In(2,3) AND ' +
                                'DATASET_SHEET_FILENAME = ''%s''',[lSystem,lFileName]),True) do
      begin
        if not eof then lMapSheetKey := VarToStr(Fields['Map_Sheet_Key'].Value);
        close;
      end; // end of with
      if lMapSheetKey <> '' then begin
        lMapSheetList.LoadFromFile(Appsettings.ObjectSheetFilePath+lFileList[i]);
        for x := 0 to lMapSheetList.count - 1 do begin
          ParseStringIntoList(lMapSheetList[x],',',lData);
          if lData[0] = 'LOCATION' then begin
            lBoundaryKey := dmGeneraldata.GetNextKey('Location_Boundary', 'Location_Boundary_Key');
            dmDataBase.ExecuteSQL(format(lQueryLoc,[lData[4],lData[3],lBoundaryKey,lData[4],lData[5],lData[6],
                               lData[7],lData[8],lData[9],lData[10],lData[11],
                               ldata[12],QuotedNullStr(ldata[13],''),AppSettings.UserID,lMapSheetKey]));
          end
          else begin
            lBoundaryKey := dmGeneraldata.GetNextKey('Admin_Boundary', 'Admin_Area_Key');
            dmDataBase.ExecuteSQL(format(lQueryAdm,[lData[3],lBoundaryKey,lData[4],lData[15],
                     lData[16],QuotedNullStr(ldata[13],''),AppSettings.UserID,lMapSheetKey]));
          end;
          lData.Clear;
        end; /// end for
      end; //if no map sheet
    end;
  end;
  lMapSheetList.Free;
  lFileList.Free;
  lData.Free;
  sgMaps.Invalidate;
end;

procedure TdlgMapOptions.RecoverMapData(const AScope : string);
begin
  if MessageDlg(ResStr_Continue_Recover, mtConfirmation,
     [mbYes, mbNo], 0) = mrYes then
  begin
    if AScope = '' then begin
      RecoverMapSheet(AppSettings.ObjectSheetFilePath, '2,3' , 'NULL',);
      RecoverBdyLinks;
      Resetmaps(True);
    end;
    RecoverMapSheet(Appsettings.MapFilePath,'1',' Host_Name()');
  end;
end;

procedure TdlgMapOptions.FormActivate(Sender: TObject);
begin
  rgsecure.Buttons[0].Caption := ResStr_Button0;
  rgsecure.Buttons[1].Caption := ResStr_Button1;
  rgsecure.Buttons[2].Caption := ResStr_Button2;
  rgsecure.Buttons[3].Caption := ResStr_Button3;
  rgsecure.Buttons[4].Caption := ResStr_Button4;
  rgsecure.Buttons[5].Caption := ResStr_Button5;
  lblMapsheet.caption := appsettings.MapFilePath;
  lblCurrentObjectSheet.caption := appsettings.ObjectSheetFilePath;
  lblUserFilePath.caption := UserFilePath;
  edWorkstation.Text := GetHostName;
  UpdateComputerMap;
  if (CheckObjectSheet) then
    btnfixMaster.visible := false
  else
    btnfixMaster.visible := true;
end;

procedure TdlgMapOptions.BackupMapFiles;
var
  lToFilepath: String;
  lReturn: Boolean;
  lItem: TSavedMap;
  lRow: Integer;
begin
  with sgMaps do begin
    lRow        := Row;
    lItem       := TSavedMap(Objects[COL_OBJECT, lRow]);
  end;
  lReturn := True;
  Try
    PopulateMapFileMff(appsettings.ObjectSheetFilePath,'2,3',False, 'IS NULL');
    PopulateMapLocBoundary;
    lToFilepath := UserFilePath + 'Security\' +  lItem.SpatialSystem + '\ObjectSheets\';
    lReturn := lReturn AND CreateBackup(AppSettings.ObjectSheetFilePath, lToFilePath,'OS',lItem.SpatialSystem);
    PopulateMapFileMff(appsettings.MapFilePath,'1',False, ' = Host_Name() ');
    lToFilepath := UserFilePath + 'Security\' + lItem.SpatialSystem + '\MapSheets\' + GetHostName + '\';
    lReturn := lReturn AND CreateBackup(AppSettings.MapFilePath, lToFilepath, 'MS',lItem.SpatialSystem);
  Except
    lReturn := False;
  end;
  if lReturn then begin
    If MessageDlg(Format(ResStr_DoArchive,[lItem.DisplayName]), mtConfirmation, [mbYes,mbNo], 0) = mrYes then
      ExecuteArchivebackup;
  end else
    MessageDlg(Format(ResStr_Failed,[ResStr_Backup]), mtInformation, [mbOK], 0);
  sgMaps.Invalidate;
end;

function TdlgMapOptions.CreateBackup(AFromFolder, AToFolder, AType,ASystem: string): Boolean;
var
  lFileList: TStringList;
  lKeepList: TStringlist;
  lNewFile: string;
  lSearchRec: TSearchRec;
  lDataSetFile: string;
  i: integer;
begin
  lFileList := TStringList.Create;
  lKeepList := TStringList.Create;
  Result := True;
  //Check that there is something to copy
  if (Not DirectoryExists(RemovePathDelim(AFromFolder))) or
       (FindFirst(AFromFolder + '*.*', 0, lSearchRec) <> 0) then begin
    MessageDlg(ResStr_NoFilesToBackup, mtInformation, [mbOK], 0);
    Result := false;
  end;
  // Don't want to delete the folder just the files
  if Result then begin
    if DirectoryExists(RemovePathDelim(AToFolder)) then
      Result :=  RemoveFolderContent(AToFolder,'',lKeeplist)
    else
      Result :=  ForceDirectories(RemovePathDelim(AToFolder));

    If DirectoryExists(RemovePathDelim(AFromFolder)) then begin// Not concerned as nothing to copy
      if Result then begin
        // Folders should have final \
        if FindFirst(AFromFolder + '*.*', 0, lSearchRec) = 0 then begin
          lFileList.add (AFromFolder + lSearchRec.Name);
          while FindNext(lSearchRec) = 0 do
            lFileList.add (AFromFolder + lSearchRec.Name);
          FindClose(lSearchRec);
        end;
        for i := 0 to lFileList.count-1 do begin
          { We only want files which are listed in map files
           and which are for the correct base map and spatial system, buit not basemaps }
          lDatasetFile := rightStr(lFilelist[i],length(lFilelist[i])-LastDelimiter('\',lFilelist[i]));
          with dmDatabase.ExecuteSQL(Format('Select * from Map_Sheet ' +
                         ' inner join Base_map BM ON BM.Base_Map_Key = Map_Sheet.Base_Map_Key ' +
                         ' WHERE BM.Spatial_System = ''%s'' and (''%s'' = ' +
                         ' LEFT(DataSet_Sheet_FileName,LEN(DataSet_Sheet_FileName)-4) ' +
                         ' AND SHEET_TYPE <> 0)'
                         , [ASystem,ExtractWithoutExt(lDatasetFile),ASystem]),true) do
          begin
            //Base Maps are not copied
            if not eof then begin
              lNewFile := AToFolder + lDataSetFile;
              CopyFile(PChar(lFilelist[i]), PChar(lNewFile),True);
            end;
            close;
          end;
        end;
      end;
    end;
  end;
  lFileList.Free;
  lKeepList.Free;
end;

procedure TdlgMapOptions.rgSecureClick(Sender: TObject);
begin
  edWorkStation.enabled := False;
  cbRetain.Enabled := False;
  cbRetain.Checked := False;
  if rgSecure.ItemIndex in[3,4,5] then begin
    cbRetain.Enabled := True;
    if rgSecure.ItemIndex = 3 then edWorkstation.Enabled := True;
  end
  else begin
    edWorkstation.Enabled := False;
    cbRetain.Enabled := False;
  end;
end;

function TdlgMapOptions.GetHostName: string;
begin
  with dmDatabase.ExecuteSQL('Select host_name() as hostname', True) do begin
    Result := VarToStr(Fields['hostname'].Value);
    Close;
  end;
end;
function TdlgMapOptions.GetMasterWorkStation: string;
begin
  Result := '';
  with dmDatabase.ExecuteSQL('Select * FROM Computer_Map where Master = 1' , True) do begin
    if not eof then Result := VarToStr(Fields['Computer_Id'].Value);
    Close;
  end;
end;

function TdlgMapOptions.RemovePathDelim(AFolder: string): string;
begin
  Result:= Afolder;
  if LastDelimiter('\',AFolder) = length(AFolder) then
    Result:= leftstr(AFolder,length(AFolder)-1)

end;

procedure TdlgMapOptions.RestoreAllMapFiles(const AScope: string);
var
  lFromFilePath: string;
  lAction: integer;
  lReturn: Boolean;
  lRow: Integer;
  lItem: TSavedMap;
begin
  lReturn := True;
  lAction := 1;
  with sgMaps do begin
    lRow        := Row;
    lItem       := TSavedMap(Objects[COL_OBJECT, lRow]);
  end;
  if edWorkStation.Text <> GetHostName then begin
    lAction := 2;
    if MessageDlg(ResStr_RestoreDifWS, mtWarning, [mbYes, mbNo], 0) = mrNO then lAction := 3;
  end;
  if lAction < 3 then begin
    Try
      Try
        if AScope = '' then begin
          lFromFilepath := UserFilePath + 'Security\' + lItem.SpatialSystem + '\ObjectSheets\';
          lReturn := RestoreBackup(lFromFilepath,AppSettings.ObjectSheetFilePath,'OS',lAction);
        end;
        lFromFilepath := UserFilePath + 'Security\' + lItem.SpatialSystem + '\MapSheets\'+ edWorkStation.text  + '\';
        lReturn := lReturn AND RestoreBackup(lFromFilepath, AppSettings.MapFilePath,'MS', lAction);
      except
         lReturn := False;
      end;
       RecoverMapData('');
    Finally
      if lReturn then
        MessageDlg(Format(ResStr_Complete,[ResStr_Restore]), mtInformation, [mbOK], 0)
      else
        MessageDlg(Format(ResStr_Failed,[ResStr_Restore]), mtInformation, [mbOK], 0);
    end;
  end;
end;

function TdlgMapOptions.RemoveFolderContent(const path, AType: String; AKeepList: TStringList): Boolean;
var
  lSearchrec: TSearchRec;
  lFileList: TStringList;
  i: integer;
begin
  lFileList := TStringList.Create;
  Result := True;
  if FindFirst(Path + '*.*', 0, lSearchRec) = 0 then begin
    // dont add it if the file name is in the keeplist
    if AKeepList.IndexOf(ExtractWithoutExt(lSearchRec.Name)) = -1  then lFileList.add (Path + lSearchRec.Name);
    while FindNext(lSearchRec) = 0 do
      if AKeepList.IndexOf(ExtractWithoutExt(lSearchRec.Name)) = -1  then lFileList.add (Path + lSearchRec.Name);
    FindClose(lSearchRec);
  end;
  for i := 0 to lFileList.count - 1 do begin
    FileSetAttr(lFileList[i], not SysUtils.faReadOnly);
    Result := Result and DeleteFile(lFileList[i]);
    if not Result then
        MessageDlg(lFileList[i] + ' ' + ResStr_FailedToDelete, mtInformation, [mbOK], 0);
  end;
lFileList.free;
end;

function TdlgMapOptions.RestoreBackup(AFromFolder, AToFolder, AType : string; AAction : integer): Boolean;
var
  lFileList,lKeepList: TStringList;
  lNewFile: string;
  lSearchRec: TSearchRec;
  i,lRow: integer;
  lItem: TSavedMap;
begin
  lFileList := TStringList.Create;
  lKeepList := TstringList.Create;
  Result := True;
  with sgMaps do begin
    lRow:= Row;
    lItem := TSavedMap(Objects[COL_OBJECT,lRow]);
  end;
  //Check that there is something to copy if not do nothing on a restore do not return an error
  if (DirectoryExists(RemovePathDelim(AFromFolder))) and
     (FindFirst(AFromFolder + '*.*', 0, lSearchRec) = 0) then
  begin
    {Keeplist will contain the base maps files plus those relating to
     spatial systems other then the one are restoring. Everthing else will be deleted to keep things tidy}
    with dmDatabase.ExecuteSQL(Format('Select DATASET_SHEET_FILENAME FROM MAP_SHEET MS ' +
                              'INNER JOIN Base_Map BM ON BM.Base_Map_Key = MS.Base_Map_Key ' +
                              'WHERE BM.Spatial_System <> ''%s'' ' +
                              'UNION SELECT Base_Map_Key+ ''.gsf'' FROM Base_Map',[lItem.SpatialSystem]),True) do
    begin
      While not eof do begin
        lKeepList.Add(ExtractWithoutExt(varToStr(Fields['DATASET_SHEET_FILENAME'].Value)));
        Movenext;
      end;
      Close;
    end; //end of with
    if DirectoryExists(RemovePathDelim(AToFolder)) then
      if not cbRetain.checked then RemoveFolderContent(RemovePathDelim(AToFolder) + '\',AType,lKeepList)
    else
      Result :=  ForceDirectories(RemovePathDelim(AToFolder));

    If DirectoryExists(RemovePathDelim(AFromFolder)) then begin// Not concerned as nothing to copy
      if Result then begin
        // Folders should have final \
        if FindFirst(AFromFolder + '*.*', 0, lSearchRec) = 0 then begin
          lFileList.add (AFromFolder + lSearchRec.Name);
          while FindNext(lSearchRec) = 0 do
            lFileList.add (AFromFolder + lSearchRec.Name);
          FindClose(lSearchRec);
        end;
        for i := 0 to lFileList.count-1 do begin
          lNewFile := AToFolder + rightstr(lFilelist[i],length(lFilelist[i])-LastDelimiter('\',lFilelist[i]));
          CopyFile(PChar(lFilelist[i]), PChar(lNewFile), True);
        end;
      end;
    end;
  end else // if OS and no files then return false as there are no backups
    If AType = 'OS' then Result := false;

  lFileList.Free;
  lKeepList.Free;
end;


function TdlgMapOptions.GetMasterObjectSheet: string;
begin
  Result:= AppSettings.ObjectSheetFilePath;
  with dmDatabase.ExecuteSQL('Select Object_Sheet_Folder FROM COMPUTER_MAP WHERE  ' +
                                ' MASTER = 1', True) do
  try
    if not eof then Result := Fields['Object_Sheet_Folder'].Value;
  finally
     Close;
  end;
end;

function TdlgMapOptions.UpdateComputerMap: boolean;
begin
  dmDatabase.ExecuteSQL(
  format('UPDATE COMPUTER_MAP SET OBJECT_SHEET_FOLDER = ''%s'' WHERE ' +
         ' COMPUTER_ID = host_name()', [AppSettings.ObjectSheetFilePath]));
  Result := True;
end;

function TdlgMapOptions.CheckComputerMap: boolean;
begin
 if AppSettings.standalone then begin   // Added so as not to do network test on standalone
     dmDatabase.ExecuteSQL ('UPDATE COMPUTER_MAP SET Master= 0 WHERE ' +
          ' COMPUTER_ID <> host_name() ');
     dmDatabase.ExecuteSQL(Format('UPDATE COMPUTER_MAP SET OBJECT_SHEET_FOLDER = ''%s'', ' +
          ' Master=1 WHERE ' +
          ' COMPUTER_ID = host_name()', [AppSettings.ObjectSheetFilePath]));
  end
  else
     UpdateComputerMap;

  Result:= CompareText(GetMasterObjectSheet,AppSettings.ObjectSheetFilePath) = 0;
end;

function TdlgMapOptions.CheckObjectSheet: boolean;
begin
 Result:= CheckComputerMap;
 if not Result then
   MessageDlg(ResStr_Object_Sheet, mtWarning, [mbOK], 0);
end;

function TdlgMapOptions.UserFilePath: String;
begin
   Result := leftstr(AppSettings.ReportPath,pos('\Reports\',AppSettings.ReportPath));
end;

function TdlgMapOptions.ArchiveBackup: String;
var
  lArchiveSuffix: string;
  lFileList: TStringList;
  lFolderList: TStringList;
  lSearchRec: TSearchRec;
  lFromFile: string;
  lResult: boolean;
  lToFile: string;
  i,lRow: integer;
  lItem: TSavedMap;
begin
  with sgMaps do begin
    lRow := Row;
    lItem:= TSavedMap(Objects[COL_OBJECT, lRow]);
  end;
  Result := '';
  lResult := True;
  lFolderList := TStringList.Create;
  lFileList := TStringList.Create;
  // Find all the folder we need
  // Add the Object Sheet Folder
  lFolderList.add (UserFilePath + 'Security%s\' + lItem.SpatialSystem + '\ObjectSheets\');
  // Add the sub folders for Map Sheets
  if FindFirst(UserFilePath + 'Security\' + lItem.SpatialSystem + '\MapSheets\*', faDirectory, lSearchRec) = 0 then begin
    if (lSearchRec.Name <> '.') and (lSearchRec.Name <> '..') and
       ((lSearchRec.attr and faDirectory) = faDirectory) then
       lFolderList.add (UserFilePath + 'Security%s\' + lItem.SpatialSystem + '\MapSheets\' + lSearchRec.Name);
    while FindNext(lSearchRec) = 0 do begin
      if (lSearchRec.Name <> '.') and (lSearchRec.Name <> '..') and
        ((lSearchRec.attr and faDirectory) = faDirectory) then
        lFolderList.add (UserFilePath + 'Security%s\' + lItem.SpatialSystem + '\MapSheets\' + lSearchRec.Name);
    end;
    FindClose(lSearchRec);
  end;
  //Create the Archive folder and find all the files we need
  for i := 0 to lFolderList.count-1 do begin
    lArchiveSuffix := FloatToStr(round(now*10000));
    if Not DirectoryExists(Format(lFolderList[i],[lArchiveSuffix])) then
      lResult :=  ForceDirectories(Format(lFolderList[i],[lArchiveSuffix]));
      if (lResult = True) and (FindFirst(Format(lFolderList[i],[''])+'\*.*', 0, lSearchRec) = 0) then begin
        lFileList.add (lFolderList[i] + '\' + lSearchRec.Name);
        while FindNext(lSearchRec) = 0 do begin
          lFileList.add (lFolderList[i] + '\' + lSearchRec.Name);
        end;
        FindClose(lSearchRec);
    end;
  end;
  for i := 0 to lFileList.count -1 do begin
    lFromFile := Format(lFilelist[i],['']);
    lToFile :=  format(lFilelist[i],[lArchiveSuffix]);
    lResult := lResult AND CopyFile(PChar(lFromFile), PChar(lToFile), True);
  end;
  if lResult then Result := UserFilePath + 'Security' + lArchiveSuffix;
  lFileList.Free;
  lFolderList.Free;
end;

function TdlgMapOptions.ExecuteArchiveBackup: string;
var
lArchiveResult: string;
begin
  lArchiveResult := ArchiveBackup;
  if lArchiveResult  <> '' then
    MessageDlg(Format(ResStr_Archive_Complete,[lArchiveResult]), mtInformation, [mbOK], 0)
  else MessageDlg(Format(ResStr_Failed,[ResStr_Archive]), mtInformation, [mbOK], 0);
end;

function TdlgMapOptions.DeleteSpecifiedFiles(Apath, AFilename: string): Boolean;
var
 lSearchRec: TSearchRec;
 lFileList: TStringList;
 i: integer;
begin
  Result := True;
  lFileList := TStringList.Create;
  if FindFirst(APath + AFileName, 0, lSearchRec) = 0 then begin
    lFileList.add (APath + lSearchRec.Name);
    while FindNext(lSearchRec) = 0 do
      lFileList.add (Apath + lSearchRec.Name);
    FindClose(lSearchRec);
  end;
  for i := 0 to lFileList.count-1 do
    Result := DeleteFile(lFilelist[i]);
  lFileList.Free;
end;

function TdlgMapOptions.BaseMapWidowOpen: Boolean;
var
  lMapWindow: TfrmMap;
begin
  lMapWindow := dmFormActions.MapWindow(FLayerLegend.BaseMapKey);
  Result := Assigned(lMapWindow);
end;
{Changes the object sheet path for this workstation to that of the master workstation
   Any polygon files already in the wrong object sheet folder
   any links in the wrong folder or any boundaries created within them will be lost.}
function TdlgMapOptions.ObjectSheetPathUpdate: boolean;
var
 lCurrentPath, lNewPath : string;
begin
  Result := True;
  if AppSettings.UserAccessLevel = ualAdmin then begin
    lCurrentPath := AppSettings.ObjectSheetFilePath;
    lNewPath := GetMasterObjectSheet;
    try
      begin
        if CompareText(lNewPath,LCurrentPath) <> 0 then begin
          AppSettings.ObjectSheetFilePath := lNewPath;
          AppSettings.WriteRegistrySettings;
          UpdateComputerMap;
        end;
      end;
      except begin
        MessageDlg(ResStr_Object_Sheet_Fail, mtInformation, [mbOK], 0);
        Result := False;
      end;
    end;
  end else begin
    MessageDlg(ResStr_Admin_Permission,mtInformation, [mbOk], 0);
    Result := False;
  end;
end;

procedure TdlgMapOptions.btnFixMasterClick(Sender: TObject);
var
lMasterWorkStation : string;
begin

 lMasterWorkStation := GetMasterWorkStation;
 if lMasterWorkStation = '' then
   MessageDlg(ResStr_Admin_No_Master,mtInformation, [mbOk], 0)
 else
   if Not AppSettings.Standalone then   // Remove if testing as if network
      If MessageDlg(Format(ResStr_ObjectSheetSync,[lMasterWorkStation,lMasterWorkStation]), mtConfirmation, [mbYes, mbNo], 0) = mrYes then
        ObjectSheetPathUpdate;
end;

end.





