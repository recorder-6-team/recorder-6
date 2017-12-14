//==============================================================================
//  Unit:        Map
//
//  Implements:  TfrmMap
//
//  Description: Implements the mapping part of the application.
//
//  Author:      John van Breda
//  Created:     8 April 1999
//
//  Last Revision Details:
//    $Revision: 510 $
//    $Date: 8/04/10 17:05 $
//    $Author: Andrewkemp $
//
//==============================================================================

unit Map;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, DB,
  ExtCtrls, BaseChildUnit, StdCtrls, ComCtrls, ImgList, ToolWin, Buttons, Grids,
  Menus, BaseFormUnit, MS5, MS5User, MapData, DataClasses, DropSource, ClipBrd,
  DropStruct, DropTarget, SpatialRefFuncs, ExceptionForm, Printers, ColorBtn,
  JNCCDataSets, OnlineHelp, Constants, ActnList, Recorder2000_tlb, RapTree,
  SelectDistributionPoint, MapServerLink, GeneralFunctions, ComObj, Contnrs,
  BaseLegend, DatasetLegend, LayerLegend, ADOInt, MapClasses, ADODB, GDIPAPI,
  GDIPObj, CRCOmmonClasses, CRConstants, GISReadWrite, GISShape, DynamicArrayUnit,
  AddinResourceStrings,DataFields, ImageListButton, BoundaryLocationMatch;

resourcestring
  ResStr_NoDataToPlot = 'No data is available to plot on the map.';
  ResStr_EastingNorthing = 'Easting/Northing';
  ResStr_LatitudeLongitude = 'Latitude/Longitude';
  ResStr_PleaseSelectPolygonOrLine =
      'Please select a polygon or line to link to a location boundary';
  ResStr_CannotExportWhileDrawing = 'Please finish drawing the current polygon before '+
      'attempting to export polygon layers';
  ResStr_CannotImportWhileDrawing = 'Please finish drawing the current polygon before '+
      'attempting to import polygon layers';
  ResStr_EmptySheetNotExported = 'The %s sheet was not exported because it contains no polygons.';
  ResStr_NoPolygonLayer = 'No polygon layer selected.';   
  ResStr_InsertFailed = 'Unable to insert location boundary record.';
  ResStr_UnableToMatchBoundary = 'Unable to link location to polygon.';

type
  EMapBrowserError = class (TExceptionPath)
  end;

  EMissingObjectID = class (TExceptionPath)
  end;

  EMapImportError = class (EMapBrowserError)
  end;

  EMapDatabaseError = class (EMapBrowserError)
  end;

  TFilterQuery = (fqTaxon, fqBiotope);

  {-----------------------------------------------------------------------------
  }
  TBoundingBox = record
    SouthWest: TLatLong;
    NorthEast: TLatLong;
  end;

  {-----------------------------------------------------------------------------
  }
  TPolygonID = record
    SheetID: Integer;
    ObjectID: Integer;
  end;

  {-----------------------------------------------------------------------------
  }
  TLocationArray = Array of TPolygonID;

  {-----------------------------------------------------------------------------
  }
  TPrintCanvas = record
    Rect: TRect;
    LeftGap: Integer;
    TopGap: Integer;
    RightGap: Integer;
    BotGap: Integer;
  end;

  {-----------------------------------------------------------------------------
  }
  TQueryDetails = record
    MapPosition: msCoord;
    ObjectName: String;
    ObjectID: Integer;
    ObjectStaticID: Integer;
    ObjectCentroid: msCoord;
    ObjectLocationKey: TKeyString;
    SheetName: String;
    SheetID: Integer;
    IsAdminArea: Boolean;
  end;

  {-----------------------------------------------------------------------------
  }
  TMapCursorPos = record
    MousePos: TPoint;
    MapPos: msCoord;
  end;

  TRectangleElement = (BottomLeftCorner, TopLeftCorner, TopRightCorner,
      BottomRightCorner, LeftSide, TopSide, RightSide, BottomSide,
      NoSideOrCorner);

  {-----------------------------------------------------------------------------
  }
  TfrmMap = class (TBaseChild)
    actAddBackgroundLayer: TAction;
    actAddPolygonLayer: TAction;
    actAssociateBoundary: TAction;
    actCancelDraw: TAction;
    actCancelSubtract: TAction;
    actClear: TAction;
    actCopyMapToClipboard: TAction;
    actDeletePolygon: TAction;
    actDraw: TAction;
    actFindSourceData: TAction;
    actFinishLineCurrent: TAction;
    actFinishPolygonCurrent: TAction;
    actMovePolygon: TAction;
    actPan: TAction;
    actPointer: TAction;
    actSubtractBoundary: TAction;
    actUnZoom: TAction;
    actZoom: TAction;
    alMap: TActionList;
    alMapTools: TActionList;
    Bevel1: TBevel;
    cmbActiveMap: TComboBox;
    cmbCurrentLayer: TComboBox;
    dlgOpen: TOpenDialog;
    HorizontalSplitter: TSplitter;
    Label1: TLabel;
    lblCurrentPolygonLayer: TLabel;
    mnuChildMap: TMenuItem;
    mnuEdit: TMenuItem;
    mnuEditCancel: TMenuItem;
    mnuEditCopyMap: TMenuItem;
    mnuEditFinishLineAny: TMenuItem;
    mnuEditFinishLineCurrent: TMenuItem;
    mnuEditFinishPolygonAny: TMenuItem;
    mnuEditFinishPolygonCurrent: TMenuItem;
    mnuEditReturnData: TMenuItem;
    mnuEditSep1: TMenuItem;
    mnuMapAddBackgroundLayer: TMenuItem;
    mnuMapAddBoundary: TMenuItem;
    mnuMapAddPolygonLayer: TMenuItem;
    mnuMapAddSample: TMenuItem;
    mnuMapAssociateBoundary: TMenuItem;
    mnuMapBoundaryDraw: TMenuItem;
    mnuMapBoundaryImport: TMenuItem;
    mnuMapBrowser: TMenuItem;
    mnuMapCancelSubtract: TMenuItem;
    mnuMapClear: TMenuItem;
    mnuMapDeletePolygon: TMenuItem;
    mnuMapDistPointsSep: TMenuItem;
    mnuMapDistributionPoints: TMenuItem;
    mnuMapFindSourceData: TMenuItem;
    mnuMapGridLines: TMenuItem;
    mnuMapGridLinesSep: TMenuItem;
    mnuMapLines100kms: TMenuItem;
    mnuMapLines10kms: TMenuItem;
    mnuMapLines20kms: TMenuItem;
    mnuMapLines50kms: TMenuItem;
    mnuMapLinesNone: TMenuItem;
    mnuMapMovePolygon: TMenuItem;
    mnuMapOptions: TMenuItem;
    mnuMapPan: TMenuItem;
    mnuMapPointer: TMenuItem;
    mnuMapPoints100m: TMenuItem;
    mnuMapPoints10kms: TMenuItem;
    mnuMapPoints1km: TMenuItem;
    mnuMapPoints2kms: TMenuItem;
    mnuMapPoints5kms: TMenuItem;
    mnuMapPointsDefault: TMenuItem;
    mnuMapSep1: TMenuItem;
    mnuMapSep2: TMenuItem;
    mnuMapSep3: TMenuItem;
    mnuMapSubtractBoundary: TMenuItem;
    mnuMapUnZoom: TMenuItem;
    mnuMapWindow: TMenuItem;
    mnuMapZoom: TMenuItem;
    mnuMapZoomExtents: TMenuItem;
    mnuSubtract: TMenuItem;
    mnuSubtractBoundary: TMenuItem;
    N1: TMenuItem;
    N5: TMenuItem;
    N7: TMenuItem;
    N8: TMenuItem;
    pmBoundary: TPopupMenu;
    pmBoundaryDraw: TMenuItem;
    pmBoundaryImport: TMenuItem;
    pmDistributionPoints: TPopupMenu;
    pmDrawingCancel: TMenuItem;
    pmDrawingFinishLineAny: TMenuItem;
    pmDrawingFinishLineCurrent: TMenuItem;
    pmDrawingFinishPolygonAny: TMenuItem;
    pmDrawingFinishPolygonCurrent: TMenuItem;
    pmGridLines: TPopupMenu;
    pmLines100kms: TMenuItem;
    pmLines10kms: TMenuItem;
    pmLines20kms: TMenuItem;
    pmLines50kms: TMenuItem;
    pmLinesNone: TMenuItem;
    pmPoints100m: TMenuItem;
    pmPoints10kms: TMenuItem;
    pmPoints1km: TMenuItem;
    pmPoints2kms: TMenuItem;
    pmPoints5kms: TMenuItem;
    pmPointsDefault: TMenuItem;
    pmPolygonDrawing: TPopupMenu;
    pmPolygonToLayer: TPopupMenu;
    pmSampleTypes: TPopupMenu;
    pmSubtractBoundary: TPopupMenu;
    pnlDistPointsLabel: TPanel;
    pnlDockableLegend: TPanel;
    pnlDockSiteLeft: TPanel;
    pnlDockSiteRight: TPanel;
    pnlMapLayerLabel: TPanel;
    pnlMapPanel: TPanel;
    pnlMapSelector: TPanel;
    pnlSelectedPolySheet: TPanel;
    sgDatasets: TStringGrid;
    sgLayers: TStringGrid;
    tmRefresh: TTimer;
    pnlBottom: TPanel;
    shpDrag: TShape;
    pnlDrag: TPanel;
    eSpatialRef: TEdit;
    eLocation: TEdit;
    staticSpatialRef: TStaticText;
    staticLocation: TStaticText;
    mnuMapExtractGridSquares: TMenuItem;
    actExtractGridSquares: TAction;
    mnuMapExportPolygonLayer: TMenuItem;
    mnuMapExportAllPolygonLayers: TMenuItem;
    pmPolygonReport: TPopupMenu;
    pmQuickReports: TMenuItem;
    OccurrencesForPlaces1: TMenuItem;
    pmBatchUpdate: TMenuItem;
    pmPlaceHolder: TMenuItem;
    actExportAllPolygonLayers: TAction;
    N2: TMenuItem;
    mnuMapExportDataset: TMenuItem;
    btnLayerDown: TImageListButton;
    btnLayerUp: TImageListButton;
    pmAssociateBoundary: TMenuItem;
    pmDeletePolygon: TMenuItem;
    pmExtractGridSquares: TMenuItem;
    pmDivider: TMenuItem;
    lblXPThemeWorkaround: TLabel;
    tmBoundingBox: TTimer;
    procedure actAddBackgroundLayerExecute(Sender: TObject);
    procedure actAddPolygonLayerExecute(Sender: TObject);
    procedure mnuMapExportPolygonLayerClick(Sender: TObject);
    procedure actExportAllPolygonLayersClick(Sender: TObject);
    procedure actAssociateBoundaryExecute(Sender: TObject);
    procedure actCancelDrawExecute(Sender: TObject);
    procedure actCancelSubtractExecute(Sender: TObject);
    procedure actCopyMapToClipboardExecute(Sender: TObject);
    procedure actDrawExecute(Sender: TObject);
    procedure actDrawUpdate(Sender: TObject);
    procedure actFindSourceDataExecute(Sender: TObject);
    procedure actFinishLineCurrentExecute(Sender: TObject);
    procedure actFinishPolygonCurrentExecute(Sender: TObject);
    procedure actMovePolygonExecute(Sender: TObject);
    procedure actPanExecute(Sender: TObject);
    procedure actPointerExecute(Sender: TObject);
    procedure actSubtractBoundaryExecute(Sender: TObject);
    procedure actUnZoomExecute(Sender: TObject);
    procedure actZoomExecute(Sender: TObject);
    procedure cmbActiveMapChange(Sender: TObject);
    procedure cmbCurrentLayerChange(Sender: TObject);
    procedure cmbCurrentLayerDrawItem(Control: TWinControl; Index: Integer; ARect: TRect;
        State: TOwnerDrawState);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormDeactivate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure HorizontalSplitterMoved(Sender: TObject);
    procedure mnuMapBoundaryImportClick(Sender: TObject);
    procedure mnuMapClearClick(Sender: TObject);
    procedure mnuMapDeletePolygonClick(Sender: TObject);
    procedure mnuMapDistributionPointsClick(Sender: TObject);
    procedure mnuMapGridLinesClick(Sender: TObject);
    procedure mnuMapZoomExtentsClick(Sender: TObject);
    procedure pnlDockSiteDockDrop(Sender: TObject; Source: TDragDockObject; X, Y: Integer);
    procedure pnlDockSiteLeftDockOver(Sender: TObject; Source: TDragDockObject; X, Y: Integer;
        State: TDragState; var Accept: Boolean);
    procedure pnlDockSiteLeftGetSiteInfo(Sender: TObject; DockClient: TControl; var
        InfluenceRect: TRect; MousePos: TPoint; var CanDock: Boolean);
    procedure pnlMapPanelClick(Sender: TObject);
    procedure pnlMapPanelMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
        X, Y: Integer);
    procedure pnlMapPanelMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure pnlMapPanelMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X,
        Y: Integer);
    procedure pnlMapPanelResize(Sender: TObject);
    procedure tmRefreshTimer(Sender: TObject);
    procedure actExtractGridSquaresExecute(Sender: TObject);
    procedure pmQuickReportsClick(Sender: TObject);
    procedure pmBatchUpdateClick(Sender: TObject);
    procedure sgLayersSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure btnLayerDownClick(Sender: TObject);
    procedure btnLayerUpClick(Sender: TObject);
    procedure tmBoundingBoxTimer(Sender: TObject);
  private
    FActivated: Boolean;
    FAddinTables: TStringList;
    FBaseMapKey: TKeyString;
    FBoundingBox: TBoundingBox;
    FBoundingBoxFake: TRect;
    FBoundingBoxResizeDragElement: TRectangleElement;
    FCalledFromWizard: Boolean;
    FComMapFormat: IBaseMapFormat;
    FDatasetLegend: TDatasetLegend;
    FDatasetSpatialSystem: String;
    FDevice: HDc;
    FDistributionScaleX: Double;
    FDistributionScaleY: Double;
    FdmMap: TdmMap;
    FDuplicatePresent: Boolean;
    FFilterResultScreen: TForm;
    FFirstCoord: Boolean;
    FGridActive: Boolean;
    FGridLineScaleX: Double;
    FGridLineScaleY: Double;
    FGridOffsetX: Double;
    FGridOffsetY: Double;
    FGridReference: String;
    FImportSheetID: Integer;
    FInitialisedToolbar: Boolean;
    FLastFilePath: String;
    FLastSheetRefresh: TDateTime;
    FLayerLegend: TLayerLegend;
    FMapCanvasSize: TPoint;
    FMapCoordsPerMetre: Double;
    FMapCursor: TCursor;
    FMapPopupMenu: TPopupMenu;
    FMapScale: Double;
    FMapServerLink: TMapServerLink;
    FMapUserData: psUserData;
    FMapWindowOpen: Boolean;
    FMouseDownMapPos: msCoord;
    FMouseDownPixPos: TPoint;
    FMouseUpMapPos: msCoord;
    FMouseUpPixPos: TPoint;
    FMultiLocations: TLocationArray;
    FObjectDetails: TQueryDetails;
    FOriginStartPan: msCoord;
    FPCanvas: TPrintCanvas;
    FPointsDrawn: Boolean;
    FPrinting: Boolean;
    FReferenceSystem: String;
    FSampleTypeKeyList: TEditableKeyList;
    FSampleTypeKeyValue: TKeyString;
    FSelectedRegion: TPolygonID;
    FSelectingArea: Boolean;
    FShownHint: Boolean;
    FHintPos: TPoint;
    FSpecificHintPos: boolean;
    FSuperBoundaryID: TPolygonID;
    FSurveyList: TEditableKeyList;
    FTool: Byte;
    FVisibleExtents: MsExtent;
    FFileName: String;
    FImportObjectTotal: Integer;
    FCurrentWindowState: TWindowState;
    FShortName: String;
    FPreBoundaryImportMapCursor: TCursor;
    FDeleteTemporaryMapFile: Boolean;
    FLocationKeys: TStringList;
    FBoundaryLocationMatch: TdlgBoundaryLocationMatch;
    FCalledFromCustom : boolean;

    procedure DrawBoundingBox;
    function GetPossibleBoundingBoxResizeDragElement(const MousePixel: TPoint): TRectangleElement;
    procedure UpdateFakeRect;
    procedure ActivateMapDataSet;
    procedure ActivePolygonLayersChange;
    procedure actSubtractBoundaryToolReset(Value: Integer);
    function AddDatasetItem(AItem: TDatasetItem; const AName: String): Boolean;
    procedure AddinDataDropped(AClsID: TGuid; AKeyList: TKeyList);
    procedure AddPolygonLayerToMap(const AMapSheetKey: TKeyString);
    function AllowForAccuracy(const ASpatialReference, ASystem: String): String;
    procedure BiotopeDropped(iSourceData: TKeyList);
    function BoundingBoxLLtoSpatialRef(const iBox: TBoundingBox): TStringCoord;
    procedure CancelDrawing;
    procedure ChangedMapDataset;
    procedure ChangeMapUserExtents;
    function CheckPointInBoundingBox(iLat,iLong: Double; iBotLeft, iTopRight: msCoord):
        Boolean;
    function CheckPointInCircle(iLat, iLong: Double; iPosition: TLatLong): Boolean;
    procedure CleanUpBoundaryImport;
    procedure CompleteBoundaryImport(sheetIndex, importObjectTotal, destSheetID:
        Integer; locationKeys: TStringList; polygonIndices: Array of Integer);
    function NearestPointInReferenceSystem(X, Y: Integer): TPoint;
    function CopyObjectToObjectSheet(ASourceSheetID, AObjectID, ADestinationSheetID,
        AObjectType: Integer; AImporting, ACopyKeyValue: Boolean): Integer;
    function CreateDistributionList(AItem: TDatasetItem; ARecordset: _Recordset; AMapPoints:
        IMapDropFormat; const ATitle, ATableName, AKeyField: String): TDatasetItem;
    procedure CreateMapUserSheet;
    procedure CreateNewMapObject(iObjectType: byte);
    function CurrentDrawingLayer: TObject;
    procedure DefineSheetQueryStatus;
    procedure DeleteMapUserSheets;
    procedure DeletePolygon;
    procedure DepressPolygonDropdown(itfState: Boolean);
    procedure DetachSheetByFileName(const iFileName: String);
    procedure DrawBigPixel(Coords: TPoint; Color: Cardinal; iBrush: HDc);
    procedure DrawBigSquare(const spatialRef: String; square: TGridSquare; graphics: TGPGraphics;
      brush: TGPSolidBrush; pen: TGPPen; gridExtents: MSExtent; paintedSquares, graySquares: TStringList);
    procedure DrawDistributionPoints(AGridExtents: MSExtent; AItem: TDatasetItem;
      AGraySquares: TStringList);
    procedure EnableDrawMenu(iState: Boolean);
    procedure EndPan;
    procedure EndSelectArea;
    procedure EndZoom;
    procedure EventDropped(ASourceData: TKeyList);
    procedure FindLatLongBoundingBox(const iPosition: TMapCursorPos; out oNELatLong,
        oSWLatLong: TLatLong);
    function FindNextFArrayObject: TPolygonID;
    procedure FindSettingsBySRSystem;
    procedure FindSourceDataUnderCursor(const iPosition: TMapCursorPos);
    procedure FinishLineClick(Sender: TObject);
    procedure FinishLineOntoSheet(ALayer: TObject);
    procedure FinishObjectOntoSheet(ALayer: TObject; AType: Byte);
    procedure FinishPolygonClick(Sender: TObject);
    procedure FinishPolygonOntoSheet(ALayer: TObject);
    function FormatScale(AScale: double): String;
    procedure GetCoBoundingBox(var oCoBotLeft, oCoTopRight: msCoord; iCoCurrentPoint: msCoord;
        iDistScaleX, iDistScaleY: double);
    function GetComBaseMapSystem: Boolean;
    function GetCoordToPixelFactor: msCoord;
    procedure GetDataWithinBoundingBox(const iNELatLong, iSWLatLong: TLatLong; out
        oSurveyEventKeys, oSampleKeys: TStringList; const iMapPos: msCoord);

    function GetGridReference: String;
    procedure GetLocationKey(const Sender: TObject; var oDropSource: TJNCCDropSource);
    function GetMapUserSheetID: Integer;
    function GetMaxZoomScale: Double;
    function GetObjectCentroid(const iObject: TPolygonID): String;
    procedure GetObjectInfo(var ObInf: TQueryDetails);
    function GetPixelToCoordFactor: msCoord;
    function GetPrinterExtents: MsExtent;
    function GetRecordCardPathFromDB(const iSampleTypeKey: String): String;
    function GetSelectedSheetID: Integer;
    function GetSizeOfNPixels: Double;
    function GetSystemCoordsOverMapCoordsFactor: msCoord;
    procedure ImportBoundary;
    function ImportMapFile(const AFileName: String): Integer;
    function LayerBySheetID(ASheetID: Integer): TLayerItem;
    procedure LinkSelectedBoundaryToAdmin(const iAdminAreaKey: TKeyString);
    procedure LinkToBoundary(iSheetID, iObjectID: Integer); overload;
    procedure LinkToBoundary(iMapSheetKey: TKeyString; iObjectID: Integer); overload;
    function MakeSixDigits(const iString: String): String;
    procedure MakeTaxonAndBiotopeStrings(AEventKeys, ASamplekeys: TStringList; out AResults:
        TObjectList);
    procedure MapDataDropped(const Sender: TObject; const AFormat: Integer; const ASourceData:
        TKeyList; const ATextStrings: TStringList; var AHandled: Boolean);
    procedure MapDatasetChange(Sender: TObject);
    procedure MapDatasetDelete(Sender: TObject);
    procedure MapDatasetExportDistributionPoints(Sender: TObject);
    procedure MapDatasetItemRefresh(Sender: TObject);
    procedure MapDatasetMenuRefresh;
    procedure MapLayerBackgroundRefresh(Sender: TObject);
    procedure MapLayerBaseMapRefresh(Sender: TObject);
    procedure MapLayerChange(Sender: TObject);
    procedure MapLayerDelete(Sender: TObject);
    procedure MapLayerExportPolygons(Sender: TObject);
    procedure MapLayerMove(ALayer: TLayerItem; AFromPos, AToPos: integer);
    procedure MapLayerPolygonRefresh(Sender: TObject);
    procedure MapLayerUpdatePopup(Sender: TObject);
    function MapToPrinterCanvas(iEast, iNorth: double; iFactor: msCoord): TPoint;
    function MapToScreen(lat, long: double; Extents: MsExtent): TPoint;
    procedure MovePolygonClick(Sender: TObject);
    procedure NewLocation;
    procedure OrganiseSheets;
    function PointDisplayedStatus(const ASpatialRef, ASpatialSys: String): Boolean;
    procedure PrintBigPixel(ACoords: TPoint; AColor: Cardinal);
    procedure PrintBigSquare(const spatialRef: String; square: TGridSquare; graphics: TGPGraphics;
      brush: TGPSolidBrush; pen: TGPPen; factor: MSCoord; paintedSquares, graySquares: TStringList);
    procedure PrintDataPoints(AItem: TDatasetItem; AGraySquares: TStringList);
    procedure PrintGridLines;
    procedure PrintLegend;
    function PrintSymbol(iLegendRect: Trect; iTop: Integer; iColor: cardinal; iSymbol: Boolean;
        iStyle: TBrushStyle; const iComment: String; iCircle: Boolean): Integer;
    function ProceedWithSubtraction(super: Boolean): Boolean;
    function QueryCursorPosition: TMapCursorPos;
    function QueryForAdminName(const iAdminAreaKey: String): String;
    function QueryForLocationName(const iLocationKey: String): String;
    function QueryLocationsFound(iCoords: msCoord): TLocationArray;
    function QueryMapPos: msCoord;
    function QueryObjectAtPos(const iCursorPosition: TMapCursorPos): TQueryDetails;
    procedure QuerySpatialReference; overload;
    procedure QuerySpatialReference(mapPos: msCoord); overload;
    function QueryWindowXY: TPoint;
    function ReadMapDatabase(iSheet, iObjectID, iFieldNumber: Integer): String;
    function ReadMapExtents: MSExtent;
    procedure RefreshLocationBoundaries;
    procedure RelinkMovedBoundary(const iOldSheetKey: String; iOldStaticID: Integer; const
        iNewSheetKey: String; iNewStaticID: Integer);
    procedure RelinkToBoundaryRecord(iSheetID, iObjectID: Integer);
    procedure RemoveSheets;
    procedure RoundCoords(var AmsCoords: msCoord; APrecision: Integer);
    function SameArray(const iArrayA, iArrayB: TLocationArray): Boolean;
    procedure SampleTypeClick(Sender: TObject);
    function SampleTypeMenuItems: Integer;
    procedure ScreenDrawGridlines(GridExtents: MsExtent);
    function ScreenToMap(ScreenX, ScreenY: Integer): msCoord;
    procedure SelectRegion(const APolygon: TPolygonID);
    procedure SetBoundingBox(const Value: TBoundingBox);
    procedure SetDistributionScaleX(const Value: Double);
    procedure SetDistributionScaleY(const Value: Double);
    procedure SetGridLineScaleX(const Value: Double);
    procedure SetGridLineScaleY(const Value: Double);
    procedure SetGridOffsetX(value: Double);
    procedure SetGridOffsetY(value: Double);
    procedure SetMapCanvasSize(const Value: TPoint);
    procedure SetMapCursor(const Value: TCursor);
    function SetMapObjectDetails(iObjectType: Byte; iVisible: Boolean; iColor: Cardinal;
        iFillStyle: shortint; iCopyKeyValue: Boolean; iKeyValue, iBoundaryKey: pChar): Integer;
    procedure SetMapScale(const Value: Double);
    procedure SetMapUserData(const Value: psUserData);
    procedure SetMouseDownPixPos(const Value: TPoint);
    procedure SetMouseUpPixPos(const Value: TPoint);
    procedure SetSampleTypeKeyValue(const Value: TKeyString);
    procedure SetSelectedRegion(const APolygonID: TPolygonID);
    procedure SetShownHint(const Value: Boolean);
    procedure SetSuperBoundary;
    procedure SetTool(const Value: Byte);
    procedure SetUnselectedPolygonColour(ALayer: TPolygonLayerItem; ADoColour, ADoPattern:
        Boolean);
    procedure SetVisibleExtents(const Value: MsExtent);
    procedure StartDrawPoly;
    procedure StartPan;
    procedure SubtractBoundary;
    procedure SurveyDropped(ASourceData: TKeyList);
    procedure TaxonDropped(ASourceData: TKeyList);
    procedure UpdateMenus;
    procedure UpdatePointsDisplayed(AList: TObjectList);
    procedure UpdatePopups;
    procedure WMAssociateBoundary(var Message: TMessage); message WM_ASSOCIATE_BOUNDARY;
    procedure WMRefreshColours(var Msg: TMessage); message WM_REFRESH_COLOURS;
    procedure WMUMapUserDraw(var Msg: TMessage); message WMU_MAPUSERDRAW;
    procedure ZeroBoundingBox;
    procedure ZeroDragBox;
    function ZeroObjectsFound(const iCursorPosition: TMapCursorPos): TQueryDetails;
    function GetTaxonOccurrences(ATLIKey: string; AWantSubTaxa: boolean): _Recordset;
    function WriteSheetForDistributionPoints(points: TObjectList; const fileName: String): Boolean;
    function WriteSheetForPolygonLayers(sheetNum: Integer; const filename, title: String): Boolean;
    procedure ExportPolygonLayers(ALayerToExp: Integer);
    function GetIsBoundaryImportInProgress: Boolean;
    procedure SetPopUpMenu(ASet: boolean);
    procedure UpdateAreaSelectionToolCursor(ResizeDragElement: TRectangleElement);
    property ImportSheetID: Integer read FImportSheetID write FImportSheetID;
    property MouseDownMapPos: msCoord read FMouseDownMapPos write FMouseDownMapPos;
    property MouseDownPixPos: TPoint read FMouseDownPixPos write SetMouseDownPixPos;
    property MouseUpMapPos: msCoord read FMouseUpMapPos write FMouseUpMapPos;
    property MouseUpPixPos: TPoint read FMouseUpPixPos write SetMouseUpPixPos;
    property SampleTypeKeyValue: TKeyString read FSampleTypeKeyValue write SetSampleTypeKeyValue;
    procedure ShowClickPoint(position: TMapCursorPos);
  protected
    procedure DoShowHint(var HintStr: string; var CanShow: Boolean; var HintInfo: THintInfo);
        override;
    procedure SetupDestinationControls; override;
    property BoundingBox: TBoundingBox read FBoundingBox write SetBoundingBox;
    property BoundingBoxFake: TRect read FBoundingBoxFake;
    property CoordToPixelFactor: msCoord read GetCoordToPixelFactor;
    property DistributionScaleX: Double read FDistributionScaleX write SetDistributionScaleX;
    property DistributionScaleY: Double read FDistributionScaleY write SetDistributionScaleY;
    property GridLineScaleX: Double read FGridLineScaleX write SetGridLineScaleX;
    property GridLineScaleY: Double read FGridLineScaleY write SetGridLineScaleY;
    property GridOffsetX: Double read FGridOffsetX write SetGridOffsetX;
    property GridOffsetY: Double read FGridOffsetY write SetGridOffsetY;
    property GridReference: String read GetGridReference; //  write SetGridReference;
    property MapCanvasSize: TPoint read FMapCanvasSize write SetMapCanvasSize;
    property MapCursor: TCursor read FMapCursor write SetMapCursor;
    property MapScale: Double read FMapScale write SetMapScale;
    property MapUserData: psUserData read FMapUserData write SetMapUserData;
    property MapUserSheetID: Integer read GetMapUserSheetID;
    property MapWindowOpen: Boolean read FMapWindowOpen write FMapWindowOpen;
    property PixelToCoordFactor: msCoord read GetPixelToCoordFactor;
    property PrinterExtents: MsExtent read GetPrinterExtents;
    property SelectedSheetID: Integer read GetSelectedSheetID;
    property SizeOfNPixels: Double read GetSizeOfNPixels;
    property SystemCoordsOverMapCoordsFactor: msCoord read GetSystemCoordsOverMapCoordsFactor;
    property Tool: Byte read FTool write SetTool;
    property VisibleExtents: MsExtent read FVisibleExtents write SetVisibleExtents;
  public
    constructor Create(AOwner: TComponent; const ABaseMapKey: TKeyString); reintroduce;
        overload;
    destructor Destroy; override;
    procedure ApplySecurity; override;
    class procedure CentreOnLocation(const ALocationKey: TKeyString; const ALocationName:
        String = '');
    procedure CentreOnObject(AMapSheetKey: TKeyString; AStaticID: Integer; const
        ALocationName: String = ''; AWithZoom: Boolean = True; AWithSelect: Boolean = True);
    class procedure CentreOnPlace(const ALocationKey, ALocationName, ASpatialRef, ASystem:
        String);
    class procedure CentreOnRef(const ASpatialRef, ASystem: String; const ALocationName:
        String = '');
    procedure AddCOMDataset(const ATitle: string; ADataset: IMapDropFormat);
    procedure FilterDropped(AConnection: TADOConnection);
    function GetKeyList: TKeyList; override;
    procedure PreviewScreen; override;
    procedure PrintScreen; override;
    procedure RefreshMap;
    procedure RefreshMapSheets;
    procedure SaveMapImage(const iFilePath: String; iGetConfirmation: Boolean);
    procedure SaveMapImageAs;
    procedure SelectArea; overload;
    procedure SelectArea(SW, NE: TLatLong); overload;
    function ThreePoints: String;
    procedure UpdateMapWindowSelector; override;
    procedure ReadSamplesInPolygon(AConnection: TADOConnection;
        AIncludePartialOverlap: boolean);
    procedure ReadLocationsInPolygon(AConnection: TADOConnection;
        AIncludePartialOverlap: boolean);
    property BaseMapKey: TKeyString read FBaseMapKey;
    property CalledFromWizard: Boolean read FCalledFromWizard write FCalledFromWizard;
    property CalledFromCustom: Boolean read FCalledFromCustom write FCalledFromCustom;
    property DatasetLegend: TDatasetLegend read FDatasetLegend;
    property DatasetSpatialSystem: String read FDatasetSpatialSystem;
    property FilterResultScreen: TForm read FFilterResultScreen write FFilterResultScreen;
    property IsBoundaryImportInProgress: Boolean read GetIsBoundaryImportInProgress;
    property LayerLegend: TLayerLegend read FLayerLegend;
    property ObjectDetails: TQueryDetails read FObjectDetails write FObjectDetails;
    property SelectedRegion: TPolygonID read FSelectedRegion write SetSelectedRegion;
    property ShownHint: Boolean read FShownHint write SetShownHint;
    property MapServerLink: TMapServerLink read FMapServerLink;
    procedure MatchedBoundaries(save, showMap: Boolean);
  end;

//==============================================================================
// Redeclaration of functions because Graticule got it wrong in MS4.PAS
// And they still got it wrong in MS5!!!!!!!!
//==============================================================================
function MapQueryObjectsAtCoord(hMapWnd: HWND; var sCoord: MSCoord; nMaxQuery: Integer;
  nSheetID, nObjectID: pInteger; var nNumFound: Integer;
  dwObjectTypes: DWORD): Integer; stdcall; external 'MS5.DLL';

function PolygonID(ASheetID, AObjectID: Integer): TPolygonID;

//==============================================================================
implementation

{$R *.DFM}

{$R MapCursors.res}

uses
  Maintbar, FormActions, TermLists, SurveyDataOptions, ApplicationSettings,
  Locations, LocationDetails, Wizard, GeneralData, PrintTitle, MapLocationOptions,
  FilterResult, Genfuncs, SurveyDetails, DeleteDatasets, Math, Observations,
  Find, JPeg, MapPolygonInteraction, AdminAreaDictBrowser, SQLConstants,
  Variants, DatabaseAccessADO, HintPointer, MapSelection, ComAddinUnit, ComClasses,
  MapBrowser, ExtractGridSquares, Types, MapPolygonScanner, CRFrmParametersUnit,
  ExternalFilter, BoundaryImport, VagueDate;

const
  TOOL_POINTER = 0;
  TOOL_ZOOM_IN = 1;
  TOOL_ZOOM_OUT = 2;
  TOOL_ZOOM_TO_EXT = 3;
  TOOL_PAN = 4;
  TOOL_FIND_SOURCE_DATA = 5;
  TOOL_DRAW_POLY = 6;
  TOOL_DEL_POLY = 7;
  TOOL_SELECT_AREA = 8;
  TOOL_DISPLAY_RECORDCARD = 9;
  TOOL_SUBTRACT_BOUNDARY = 10;

  crZoomIn = 1;
  crZoomOut = 2;
  crFindSourceData = 3;

  GS_100KM_GRID = 0;
  GS_50KM_GRID = 1;
  GS_10KM_GRID = 2;
  GS_1KM_GRID = 3;
  GS_NONE = 5;

  LEGEND_TOP = 100;
  LEGEND_BOTTOM = 2000; // this is the maximum height, it will shrink to fit
  LEGEND_LEFT = 100;
  LEGEND_RIGHT = 800;

  DP_10KM_GRID = 0;
  DP_5KM_GRID = 1;
  DP_2KM_GRID = 2;
  DP_1KM_GRID = 3;
  DP_100M = 4;
  DP_DEFAULT = 6;

  // Printer Canvas Offsets
  //  - Some might say that these are hard coded.
  //  - When printing from MapServer, the printout is set to the screen scale and
  //    positioned in a rectangle which is slihtly smaller than the Printer Canvas
  //    rectangle.
  //  - These offsets were guessculated for the Laser III Printer and obviously
  //    will differ for other printers }
  LEFT_OFFSET = 66;
  TOP_OFFSET = 50;
  RIGHT_OFFSET = 64;
  BOTTOM_OFFSET = 50;

  PAPER_SIZE_X = 0.1895;
  PAPER_SIZE_Y = 0.2785;

  SCREEN_GRAY_COLOR = $00808080;

  GRIDLINE_COLOR = $00FF0000;    // rgb(0,0,255)    $00FF0001
  QUERY_CAPACITY = 2000; // the maximum number of records to return from a recordset.
  NUM_ACCURACY_PIX = 10;


  SQL_CHANGED_MAPSHEETS = 'SELECT * FROM Map_Sheet WHERE Base_Map_Key = ''%s'' ' +
           'AND (Computer_ID = Host_Name() OR Sheet_Type = 3) ' +
           'AND (Changed_Date > Convert(DATETIME, ''%s'', 102) ' +
           'OR Entry_Date > Convert(DATETIME, ''%s'', 102) ' +
           'OR Modified_Data = 1 OR New_Data = 1 OR Remove_Sheet = 1 ' +
           'OR Dataset_Sheet_Filename IS NULL)';

resourcestring
  ResStr_BBOutsideSystem = 'The bounding box selected extends beyond the limits ' +
                           'of the current spatial reference system.';

  ResStr_SampleOutsideLimits =  'The sample is outside the limits of the current spatial ' +
                                'reference system - cannot create record card';

  ResStr_Print = 'There has been a printing error - Printing has been aborted';
  ResStr_ScaleBounds = 'Zooming to the new scale is not permitted.';

  ResStr_CantFindSheet =  'Cannot find sheet %s in map dataset.  If you are using a ' +
                          'networked installation then please ensure that your ' +
                          'Object Sheet file path is the same as for all other users.';

  ResStr_ErrorSheetQryStatus =  'There has been an error setting the query status for sheet %s.';

  ResStr_RCNotFound = 'A recording card cannot be found on file path: ';

  ResStr_InvalidBoundingBox = 'The bounding box includes areas not covered by the' +
                              ' current spatial reference system.'#13#13 +
                              'Please select another';

  ResStr_InvalidGridLines = 'The Gridlines are too closely packed for the screen'#13 +
                            'and have been set to the closest suitable scale.';

  ResStr_Cursor = 'The Cursor style cannot be set.';
  ResStr_CantDrag = 'A spatial reference or location must be selected before data can be transferred.';
  ResStr_NoSpatialRef = 'A valid spatial reference must be selected by clicking on the map.';
  ResStr_Centroid = 'Unable to calculate the centroid of the selected object';

  ResStr_ComMapNotBaseMap = 'Com Base Map System does not support Base Maps.  '+
                            'Com Systems should not have the same name as existing systems.';

  ResStr_NoName = 'Location not named';
  ResStr_NoRecordings = 'There are no recordings to plot on the map';
  ResStr_Setup = 'Setting up the map.  Please Wait.';
  ResStr_SRUnable = 'Cannot calculate';

  ResStr_CannotCopyMap =  'The map could not be successfully copied to the clipboard';
  ResStr_CannotSetDataSetVisible =  'Failure to set dataset visible extents';
  ResStr_SelectBoundaryFirst =  'You must select a boundary first.';
  ResStr_SelectBoundaryToHole = 'Please select the boundary you wish to create the hole in, then press F8.';

  ResStr_DeleteBoundary = 'There is a location or admin area associated with this boundary.  If ' +
                          'you delete the boundary, the details of the association ' +
                          'will also be deleted from the database.'#13#13 +
                          'Are you sure you wish to proceed?' ;

  ResStr_DepressPolygonDropDownError =  'Error occurred during call to DepressPolygonDropdown';
  ResStr_LoadingDataset = 'Loading dataset...';

  ResStr_UnrecognisedBaseMapSystem = 'The map has a base map system ''%s''' +
                                     'which is not recognised. Reset the map '+
                                     'or install an addin which provides support for this spatial reference system.';

  ResStr_NoDistributionPoints = 'There are no distribution points displayed at this position.';
  ResStr_PolygonCoordinates = 'Polygons must be constructed from 3 or more coordinates';
  ResStr_CannotAddPolygon = 'Polygon cannot be added to the layer -  it is not a polygon layer';
  ResStr_ImportCancelled =  'Import Cancelled';
  ResStr_CopyingObjects = 'Copying Objects onto the Map Sheet - Please Wait';
  ResStr_SelectToDelete = 'Please select a region, line or sample to delete.';

  ResStr_ChangeSRSystem = 'Please change to another spatial reference '+
                          'system in order to assign a location.';

  ResStr_PrintingMap =  'Printing the map may take some time. Please Wait';
  ResStr_BoundaryHaveLocation = 'The boundary you have selected has an associated Location or Admin Area.  Proceed anyway?';
  ResStr_ErrorSavingMap = 'Error on saving map image';
  ReStr_SavingMapFiles =  'Map files can only be saved with a jpg or bmp extension.';

  ResStr_UnrecognisedSR = 'The bounding box contains a %s' +
                          ' spatial references that is not recognised by the ' +
                          'current spatial reference system.'#13#13 +
                          'Please select a different bounding box.';

  ResStr_SelectingLocationData =  'You are selecting location data to return to another screen.'#13 +
                                  'Selecting the pointer will allow you to set the return data';

  ResStr_CanReturnMapData = 'Map data can now be returned to other screens';
  ResStr_SetToolError = 'Error occurred during call to SetTool';
  ResStr_UpdatingPolygonColours = 'Please wait, updating polygon colours..';
  ResStr_CannotEstablishVisibleExtents = 'Unable to establish the visible extents of the map window';
  ResStr_BoundaryFullyContained = 'The boundary you subtract must be fully contained within the first boundary.';
  ResStr_NoInfoToLocateMap =  'There is no information available in order to locate the item on the map.';
  ResStr_SRBeyondLimits = 'The spatial reference is beyond the limits of the base map and cannot be displayed';
  ResStr_DeleteSelRegion =  'Are you sure you want to delete the selected region?';
  ResStr_CannotCompLine = 'Cannot complete a line from a single point.';
  ResStr_CannotAddLine =  'Line cannot be added to the layer -  it is not a polygon layer';
  ResStr_CannotDetermineObjCent = 'Unable to determine the object centroid';
  ResStr_ErrInsAdminBoundary =  'Error occurred inserting admin boundary into database';
  ResStr_LinkBoundToAdminArea = 'Link boundary to Admin Area';
  ResStr_MoveError =  'Move error at %s, %s';
  ResStr_LinkLocationWhileEditing = 'Cannot link boundaries while editing locations, '
                   + 'please finish editing the current location and save before attempting '
                   + 'to perform a link.';
  ResStr_LegScale = 'Scale: 1:%s';
  ResStr_LegGridInterval = 'Gridline Interval: %s';
  ResStr_DistrSpacing = 'Distribution Spacing: %s';
  ResStr_DefDistrSpacing = 'Default distribution spacing';
  ResStr_CutOffYearSym = 'Cut off year for hollow symbols: %s';
  ResStr_ErrAccessingSheetExt = 'Error accessing sheet extents for %s';
  ResStr_ErrSettingSheetExt = 'Error setting sheet extents for %s';
  ResStr_OverwriteImage = 'Are you sure you wish to overwrite the existing image file?';
  ResStr_SaveMapImg = 'Save Map Image...';
  ResStr_ErrorCallSetSelectedRegion = 'Error occurred during call to SetSelectedRegion';
  ResStr_AndOthers = ' and others';
  ResStr_MapWindow = 'Map Window';
  ResStr_ChooseFolder = 'Choose Folder';
  ResStr_ChooseFileName = 'Choose File name';
  ResStr_ExportDataset  = 'Export Dataset';
  ResStr_ExportPolygonLayer = 'Export Polygon Layer';
  ResStr_ExportAllPolygonLayers = 'Export All Polygon Layers';
  ResStr_MapLayersExport =  'All polygon layers exported successfully.';
  ResStr_MapLayerExport =  'The polygon layer exported successfully.';
  ResStr_DatasetExported = 'The dataset exported successfully.';
  ResStr_Exporting = 'Exporting %s...';
  ResStr_Squares  = 'Squares';
  ResStr_Points   = 'Points';
  ResStr_Polygons = 'Polygons';
  ResStr_Lines    = 'Lines';
  ResStr_Overwrite = 'A file by this name already exists, are you sure you want to overwrite it?';

  ResStr_NoLayersToExport =
      'No polygon layers to export.'#13#10#13#10
      + 'You need at least one visible polygon layer to use the Export All Polygon Layers functionality.';

  ResStr_UseNewPolygon =
      'A corrupt import file may result in all data attached to a polygon being lost.'#13#10 +
      'Please ensure that you have a a backup of your map data before proceeding - see Map Options'#13#10#13#10 +
      'Do you wish to continue ';

  //used in FilterDropped procedure
  ResStr_Local_ReportOutput = 'Report Output';
  ResStr_Local_Biotope =  ' - Biotopes';
  ResStr_Local_Taxa = ' - Taxa';
  ResStr_OnlyOneBoundaryImport = 'You can only run one Boundary Import at once.';
  ResStr_BoundaryImportInProgress = 'A Boundary Import is in progress, please cancel or complete the import before closing this window.';

var
  mBoundaryImportInProgress : Boolean;

function PolygonID(ASheetID, AObjectID: Integer): TPolygonID;
begin
  Result.SheetID  := ASheetID;
  Result.ObjectID := AObjectID;
end;

{-==============================================================================
    TfrmMap
===============================================================================}
{-------------------------------------------------------------------------------
}
constructor TfrmMap.Create(AOwner: TComponent; const ABaseMapKey: TKeyString);
begin
  inherited Create(AOwner);
  FdmMap := TdmMap.Create(nil);
  FInitialisedToolbar := False;
  FCalledFromWizard   := False;
  FCalledFromCustom   := False;
  FFilterResultScreen := nil;

  FAddinTables       := TStringList.Create;
  FSurveyList        := TEditableKeyList.Create;
  FSampleTypeKeyList := TEditableKeyList.Create;
  FBaseMapKey := ABaseMapKey;
  
  // Create list handling the datasets dropped on map, and link to grid.
  FDatasetLegend := TDatasetLegend.Create(Self, sgDatasets);
  with FDatasetLegend do begin
    OnChange     := MapDatasetChange;
    OnDeleteItem := MapDatasetDelete;
    AddPopupMenuItem(mnuMapExportDataset.Caption, MapDatasetExportDistributionPoints, 0);
    AddPopupMenuItem('-', nil, 1);
  end;

  // Create list handling the layers dropped on map, and link to grid.
  FLayerLegend := TLayerLegend.Create(Self, sgLayers);
  with FLayerLegend do begin
    OnChange               := MapLayerChange;
    OnDeleteItem           := MapLayerDelete;
    OnMoveLayer            := MapLayerMove;
    OnUpdatePopupItemState := MapLayerUpdatePopup;
    AddPopupMenuItem(actAddBackgroundLayer.Caption, actAddBackgroundLayer.OnExecute, 0);
    AddPopupMenuItem(actAddPolygonLayer.Caption, actAddPolygonLayer.OnExecute, 1);
    AddPopupMenuItem(ResStr_ExportPolygonLayer, MapLayerExportPolygons, 2);
    AddPopupMenuItem(actExportAllPolygonLayers.Caption, actExportAllPolygonLayersClick, 3);
    AddPopupMenuItem('-', nil, 4);
  end;

  //Setting the popup menu for the MapPanel
  SetPopUpMenu(true);

  // Create MapServerLink object and link it to the map panel
  FMapServerLink := TMapServerLink.Create(pnlMapPanel);
  FMapWindowOpen := True;

  dlgOpen.InitialDir := AppSettings.MapFilePath;
  FLastFilePath      := AppSettings.MapFilePath;
  
  SendMessage(Handle, WM_UPDATE_MENU_ICONS, 0, 0);
  // Setup help
  mnuEdit.HelpContext     := IDH_EDITMENU;
  mnuChildMap.HelpContext := IDH_MAPMENU;
  Self.HelpContext        := IDH_MAPBROWSER;
  Self.Resize;   // Puts Combo Box in correct place.  JNCC518
  ShownHint := True;

  FMapCursor := crArrow;
  FSuperBoundaryID.SheetID := -1;

  // The bounding box is not being resized.
  FBoundingBoxResizeDragElement := NoSideOrCorner;
  
  // Refresh Map sub-menu
  UpdateMapWindowSelector;
  
  // Select map dataset corresponding to chosen BaseMapKey
  ChangedMapDataset;

  // Fix for dynamically populated PopupMenus
  RefreshXPMenu;

  pmBatchUpdate.Visible := AppSettings.UserAccessLevel = ualAdmin;
  MapDatasetMenuRefresh;

  FReferenceSystem := AppSettings.SpatialRefSystem;
  // by default, map hints appear in the centre of the map
  FSpecificHintPos := false;
end;  // TfrmMap.Create

{-------------------------------------------------------------------------------
}
destructor TfrmMap.Destroy;
begin
  try
    // Clears up the bits and pieces of the map when the form is closed
    // while awaiting location info.
    cmbCurrentLayer.Clear;

    FreeAndNil(FdmMap);
    FreeAndNil(FDatasetLegend);
    FreeAndNil(FLayerLegend);
    FreeAndNil(FMapServerLink);
    FMapWindowOpen := False;

    if Assigned(FComMapFormat) then FComMapFormat := nil;

    FreeAndNil(FAddinTables);
    FreeAndNil(FSurveyList);
    FreeAndNil(FSampleTypeKeyList);

    frmMain.Status.Panels[0].Text := ''; // mustn't call Application.ProcessMessages.
  finally
    Screen.Cursor := crDefault;
    // Reactivate the wizard if comming from there, after the map has been properly
    // disposed of.  Don't if it is destroying (Close all menu option selected?)
    if CalledFromWizard then
      if not (csDestroying in FilterResultScreen.ComponentState) then
        PostMessage(FilterResultScreen.Handle, WM_RUN_WIZARD, 0, 0);
    inherited Destroy;
  end;
end;  // TfrmMap.Destroy

{-------------------------------------------------------------------------------
}
procedure TfrmMap.actAddBackgroundLayerExecute(Sender: TObject);
var
  lItem: TBackgroundLayerItem;
begin
  inherited;
  lItem := TBackgroundLayerItem.CreateNew(BaseMapKey);
  if Assigned(lItem) then begin
    lItem.OnNeedRefresh := MapLayerBackgroundRefresh;
    FLayerLegend.AddItem(lItem);
    RefreshMapSheets;
    RefreshMap;
    Application.ProcessMessages;
  end;
end;  // TfrmMap.actAddBackgroundLayerExecute

{-------------------------------------------------------------------------------
}
procedure TfrmMap.actAddPolygonLayerExecute(Sender: TObject);
var
  lItem: TPolygonLayerItem;
begin
  inherited;

  lItem := TPolygonLayerItem.CreateNew(BaseMapKey);
  if Assigned(lItem) then begin
    lItem.OnNeedRefresh := MapLayerPolygonRefresh;
    FLayerLegend.AddItem(lItem);
    RefreshMapSheets;
    ActivePolygonLayersChange;
    RefreshMap;
    AppSettings.UpdateMapWindowSelectors;
    Application.ProcessMessages;
    // The polygon will have been added to the bottom, but we want it at the top.
    FLayerLegend.MoveLayer(sgLayers.RowCount - 1, 0);
  end;
end;  // TfrmMap.actAddPolygonLayerExecute

{-------------------------------------------------------------------------------
}
procedure TfrmMap.MapDatasetExportDistributionPoints(Sender: TObject);
var
  item: TDatasetItem;
  dlgSave: TSaveDialog;
begin
  if actCancelDraw.Enabled  then
    raise EMapBrowserError.CreateNonCritical(ResStr_CannotExportWhileDrawing);

  if Sender is TMenuItem then begin
    if TMenuItem(Sender).Parent = mnuMapExportDataset then
      item := FDatasetLegend[TMenuItem(Sender).MenuIndex]
    else
      item := FDatasetLegend[sgDatasets.Row];

    //Preparing the save dialog
    dlgSave := TSaveDialog.Create(nil);
    try
      with dlgSave do
      begin
        Title       := ResStr_ExportDataset + ' - ' + ResStr_ChooseFileName;
        DefaultExt  := 'shp';
        FileName    := ValidateFileName(item.Title);
        Filter      := ResStr_FilterSHPFilesOnly;
        FilterIndex := 1;
        InitialDir  := FLastFilePath;

        if Execute then begin
          FLastFilePath := ExtractFilePath(FileName);
          frmMain.SetStatus(Format(ResStr_Exporting, [item.Title]));
          if WriteSheetForDistributionPoints(item.DistributionPoints, FileName) then
            ShowInformation(ResStr_DatasetExported);
        end;
      end;
    finally
      dlgSave.Free;
    end;
  end;
end;

{-------------------------------------------------------------------------------
}
procedure TfrmMap.actExportAllPolygonLayersClick(Sender: TObject);
var
  i: Integer;
begin
  // Do a quick check to see if there are any layers to export
  for i := 0 to FLayerLegend.Count - 1 do 
    if FLayerLegend.LegendItems[i].Visible and
       not ((FLayerLegend.LegendItems[i] is TBaseMapLayerItem) or
            (FLayerLegend.LegendItems[i] is TBackgroundLayerItem)) then
    begin
      // Found at least one to do.
      ExportPolygonLayers(-1);
      Exit;
    end;
  // Nothing to process, display message.
  ShowInformation(ResStr_NoLayersToExport);
end;

{-------------------------------------------------------------------------------
}
procedure TfrmMap.mnuMapExportPolygonLayerClick(Sender: TObject);
var
  lLayerToExp: Integer;
begin
  with cmbCurrentLayer do
    lLayerToExp := FMapServerLink.SheetIDByFileName(TLayerItem(Items.Objects[ItemIndex]).FileName);
  ExportPolygonLayers(lLayerToExp);
end;

{-------------------------------------------------------------------------------
}
procedure TfrmMap.MapLayerExportPolygons(Sender: TObject);
var
  lLayerToExp: Integer;
begin
  lLayerToExp := FMapServerLink.SheetIDByFileName(FLayerLegend.LegendItems[sgLayers.Row].FileName);
  ExportPolygonLayers(lLayerToExp);
end;

{-------------------------------------------------------------------------------
}
procedure TfrmMap.ExportPolygonLayers(ALayerToExp: Integer);
var
  lSaveDialog: TSaveDialog;
  i: Integer;
  lLayerToExp : Integer;
  lPolygonID : TPolygonID;
  lAllSuccessful : boolean;
  fileName: String;
begin
  if actCancelDraw.Enabled  then
    raise EMapBrowserError.CreateNonCritical(ResStr_CannotExportWhileDrawing);

  //Saving the current polygon selection settings to recover after export
  lPolygonID.SheetID := FSelectedRegion.SheetID;
  lPolygonID.ObjectID := FSelectedRegion.ObjectID;

  try
    //Setting the map to not selected anyting because selection on map
    //prevents export
    FSelectedRegion.SheetID := -1;
    FSelectedRegion.ObjectID := -1;
    SetSelectedRegion(FSelectedRegion);

    //Preparing the save dialog
    lSaveDialog := TSaveDialog.Create(nil);
    try
      with lSaveDialog do
      begin
        Title := ResStr_ExportPolygonLayer + ' - ' + ResStr_ChooseFileName;
        if ALayerToExp <> -1 then
          FileName := ValidateFileName(FLayerLegend.LegendItems[sgLayers.Row].Title)
        else
          FileName := ValidateFileName(cmbActiveMap.Text + ' - ' + ResStr_ExportAllPolygonLayers);
        Filter := ResStr_FilterSHPFilesOnly;
        FilterIndex := 1;
        InitialDir := FLastFilePath;
        if ALayerToExp <> -1 then DefaultExt := 'shp';
      end;

      if lSaveDialog.Execute
            and ((not FileExists(lSaveDialog.FileName))
                 or (MessageDlg(ResStr_Overwrite, mtWarning, [mbYes, mbNo], 0) = mrYes)) then
      begin
        FLastFilePath := ExtractFilePath(lSaveDialog.FileName);
        lAllSuccessful := true;
        MapCursor := crHourglass;
        if ALayerToExp <> -1 then //Checking if it is 'export' or 'export all'
        begin
          //Export selected layer
          frmMain.SetStatus(Format(ResStr_Exporting, [FLayerLegend.LegendItems[sgLayers.Row].Title]));
          //Writes the file in the selected folder
          lAllSuccessful := WriteSheetForPolygonLayers(
              ALayerToExp,
              lSaveDialog.FileName,
              FLayerLegend.LegendItems[sgLayers.Row].Title);
        end else begin
          fileName := lSaveDialog.FileName;

          //Export All Layers
          for i := 0 to FLayerLegend.Count - 1 do
            //Checking if it is base map because if it is base map we do not export it
            if FLayerLegend.LegendItems[i].Visible and
               not ((FLayerLegend.LegendItems[i] is TBaseMapLayerItem) or
                    (FLayerLegend.LegendItems[i] is TBackgroundLayerItem)) then
            begin
              frmMain.SetStatus(Format(ResStr_Exporting, [FLayerLegend.LegendItems[i].Title]));
              lLayerToExp := FMapServerLink.SheetIDByFileName(FLayerLegend.LegendItems[i].FileName);
              lAllSuccessful := WriteSheetForPolygonLayers(
                  lLayerToExp,
                  LeftStr(fileName, Length(fileName) - Length(ExtractFileExt(fileName)))
                      + ' [' + ValidateFileName(FLayerLegend.LegendItems[i].Title) + ']'
                      + ExtractFileExt(fileName),
                  FLayerLegend.LegendItems[i].Title) and lAllSuccessful;
            end;
        end;
        if lAllSuccessful then begin
          // Success message only displayed if all sheets exported
          if ALayerToExp=-1 then
            ShowInformation(ResStr_MapLayersExport)
          else
            ShowInformation(ResStr_MapLayerExport);
        end;
      end;
    finally
      lSaveDialog.Free;
    end;
  finally
    MapCursor := crArrow;
    //Recovering the selection setting from before the export started
    SetSelectedRegion(lPolygonID);
  end;
end;

{-------------------------------------------------------------------------------
 Creates a shp file given the sheetID and filename to save. Returns false if
    not exported.
}
function TfrmMap.WriteSheetForDistributionPoints(points: TObjectList; const fileName: String):
    Boolean;
var
  svoSquare: TSVOShapePointArray;
  squaresList, pointsList: TSVOShapeList;
  i: Integer;
  item: TMapDrawItem;
begin
  squaresList := TSVOShapeList.Create;
  pointsList  := TSVOShapeList.Create;
  try
    with squaresList.DataFields.AddField do begin
      FieldName := 'ID';
      FieldType := ctInteger;
    end;
    with pointsList.DataFields.AddField do begin
      FieldName := 'ID';
      FieldType := ctInteger;
    end;

    for i := 0 to points.Count - 1 do begin
      item := TMapDrawItem(points[i]);
      if item.HasGridSquare then begin
        // Seems the TSVOPolygonShape will take care of clearing this one up, so don't free.
        svoSquare := TSVOShapePointArray.Create(0);
        svoSquare.AddPoint(TDoublePoint(LatLongToSpecificEN(item.GridSquare.BottomLeft, DatasetSpatialSystem)));
        svoSquare.AddPoint(TDoublePoint(LatLongToSpecificEN(item.GridSquare.TopLeft, DatasetSpatialSystem)));
        svoSquare.AddPoint(TDoublePoint(LatLongToSpecificEN(item.GridSquare.TopRight, DatasetSpatialSystem)));
        svoSquare.AddPoint(TDoublePoint(LatLongToSpecificEN(item.GridSquare.BottomRight, DatasetSpatialSystem)));
        svoSquare.AddPoint(TDoublePoint(LatLongToSpecificEN(item.GridSquare.BottomLeft, DatasetSpatialSystem)));
        with TSVOPolygonShape.Create(squaresList, svoSquare) do
          FieldByName('ID').AsInteger := i;
      end else
        with TSVOPointShape.Create(pointsList) do begin
          SetShape(TDoublePoint(LatLongToSpecificEN(item.LatLong, DatasetSpatialSystem)));
          FieldByName('ID').AsInteger := i;
        end;
    end;

    try
      dmGeneralData.WriteSHPFiles(squaresList, pointsList, fileName, ResStr_Squares,
                                  ResStr_Points, DatasetSpatialSystem);
      Result := True;
    Except on E: EFileStreamError do
      begin
        ShowInformation(E.Message);
        Result := False;
      end;
    end;
  finally
    squaresList.Free;
    pointsList.Free;
  end;
end;  // WriteSheetForDistributionPoints

{-------------------------------------------------------------------------------
 Creates a shp file given the sheetID and filename to save. Returns false if
    not exported.
}
function TfrmMap.WriteSheetForPolygonLayers(sheetNum: Integer; const filename, title: String):
    Boolean;
var
  nObject, nCoord, j, k, objId: Integer;
  mcCoord: MSCoord;
  shapePointArray: TSVOShapePointArray;
  polyList, lineList: TSVOShapeList;
  objType: Byte;
  data: _Recordset;
begin
  Result := False;
  polyList := TSVOShapeList.Create;
  lineList := TSVOShapeList.Create;
  try
    with polyList.DataFields.AddField do begin
      FieldName := 'ID';
      FieldType := ctInteger;
    end;
    with polyList.DataFields.AddField do begin
      FieldName := 'Item_Name';
      FieldType := ctString;
      Size := 100;
    end;
    with polyList.DataFields.AddField do begin
      FieldName := 'Locatn_Key';
      FieldType := ctString;
      Size := 16;
    end;
    with polyList.DataFields.AddField do begin
      FieldName := 'File_Code';
      FieldType := ctString;
      Size := 20;
    end;
    with lineList.DataFields.AddField do begin
      FieldName := 'ID';
      FieldType := ctInteger;
    end;

    MapCheck(MapGetNumObjects(FMapServerLink.MapHandle, sheetNum, nObject));
    if nObject = 0 then
      ShowInformation(Format(ResStr_EmptySheetNotExported, [title]))
    else begin
      for j := 0 to nObject - 1 do
      begin
        // Seems the TSVOPolygonShape will take care of clearing this one up, so don't free.
        shapePointArray := TSVOShapePointArray.Create(0);
        //putting the object to edit store
        MapCheck(MapEditSelectObject(FMapServerLink.MapHandle, sheetNum, j));
        MapCheck(MapEditGetNumCoords(FMapServerLink.MapHandle, nCoord));
        MapCheck(MapEditGetID(FMapServerLink.MapHandle, objId));
        MapCheck(MapEditGetObjectType(FMapServerLink.MapHandle, objType));

        //Write the values of the coord to the shape file
        for k := 0 to nCoord - 1 do
        begin
          MapCheck(MapEditGetObjectCoord(FMapServerLink.MapHandle, k, mcCoord));
          shapePointArray.AddPoint(MakeDoublePoint(mcCoord.X, mcCoord.Y));
        end;

        // Different shape type for MS_LINE_OBJECT and MS_REGION_OBJECT
        if objType = MS_LINE_OBJECT then
          with TSVOLineShape.Create(lineList, shapePointArray) do
            FieldByName('ID').AsInteger := objId
        else begin
          //17578 Export beginning point again to make sure Polygon closes in different App
          MapCheck(MapEditGetObjectCoord(FMapServerLink.MapHandle, 0, mcCoord));
          shapePointArray.AddPoint(MakeDoublePoint(mcCoord.X, mcCoord.Y));

          with TSVOPolygonShape.Create(polyList, shapePointArray) do
          begin
            FieldByName('ID').AsInteger := objId;

            data := dmDatabase.ExecuteSQL(
                           Format('EXECUTE usp_Location_Get_ForPolygon %d, ''%s''',
                                  [FMapServerLink.StaticIDforObject(sheetNum, objId),
                                   FMapServerLink.SheetMapSheetKey(sheetNum)]),
                           True);

            try
              if data.Eof then begin
                FieldByName('Item_Name').Value  := '';
                FieldByName('Locatn_Key').Value := '';
                FieldByName('File_Code').Value  := '';
              end else begin
                FieldByName('Item_Name').AsString  := data.Fields['Item_Name'].Value;
                FieldByName('Locatn_Key').AsString := data.Fields[PK_LOCATION].Value;
                if VarIsNull(data.Fields['File_Code'].Value) then
                  FieldByName('File_Code').AsString := ''
                else
                  FieldByName('File_Code').AsString  := data.Fields['File_Code'].Value;
              end;
            finally
              data.Close;
            end;
          end;
        end;

        MapCheck(MapEditClearObject(FMapServerLink.MapHandle));
      end;

      try
        dmGeneralData.WriteSHPFiles(polyList, lineList, fileName, ResStr_Polygons, ResStr_Lines, 'LTLN');
        Result := True;
      Except on E: EFileStreamError do
        begin
          ShowInformation(E.Message);
          Result := False;
        end;
      end;

    end;
  finally
    polyList.Free;
    lineList.Free;
  end;
end;  // WriteSheetForPolygonLayers

{-------------------------------------------------------------------------------
}
procedure TfrmMap.actAssociateBoundaryExecute(Sender: TObject);
begin
  inherited;
  if SelectedRegion.ObjectID > -1 then
    // have to post message instead of doing this immediately because the
    //tool button is destroyed when the location screen is loaded, causing an
    //access violation.
    PostMessage(Handle, WM_ASSOCIATE_BOUNDARY, 0, 0)
  else
    MessageDlg(ResStr_PleaseSelectPolygonOrLine, mtInformation, [mbOK], 0);
end;  // TfrmMap.actAssociateBoundaryExecute

{-------------------------------------------------------------------------------
}
procedure TfrmMap.actCancelDrawExecute(Sender: TObject);
begin
  CancelDrawing;
end;  // TfrmMap.actCancelDrawExecute

{-------------------------------------------------------------------------------
  Cancels the Subtract Boundary from Boundary process. 
}
procedure TfrmMap.actCancelSubtractExecute(Sender: TObject);
begin
  Tool := TOOL_POINTER;
  SetPopUpMenu(true);
  FSuperBoundaryID.SheetID := -1;
end;  // TfrmMap.actCancelSubtractExecute 

{-------------------------------------------------------------------------------
}
procedure TfrmMap.actCopyMapToClipboardExecute(Sender: TObject);
var
  lSEExtent: Integer;
begin
  inherited;
  { Find the position of SouthEast corner of the map displayed in the browser }
  lSEExtent  := MakeLong(MapCanvasSize.x, MapCanvasSize.y);
  { Attempt to copy map to the clipboard }
  if (MapCopyToClipboard(FMapServerLink.MapHandle, $00000000, lSEExtent,
                         pChar(''), $FFFFFF, $000000) <> MS_SUCCESS) then
    raise EMapBrowserError.CreateNonCritical(ResStr_CannotCopyMap);
end;  // TfrmMap.actCopyMapToClipboardExecute 

{-------------------------------------------------------------------------------
  Switch to draw on the map mode
}
procedure TfrmMap.actDrawExecute(Sender: TObject);
begin
  inherited;
  SelectedRegion := PolygonID(0, -1);
  Tool := TOOL_DRAW_POLY;
  SetPopUpMenu(false);
end;  // TfrmMap.actDrawExecute

{-------------------------------------------------------------------------------
  This synchronises the polygon drawing/import toolbutton with the checked state of the drawing action.  Otherwise the user can't see when drawing is selected because it appears on a sub menu.
}
procedure TfrmMap.actDrawUpdate(Sender: TObject);
begin
  inherited;
  DepressPolygonDropdown(actDraw.Checked)
end;  // TfrmMap.actDrawUpdate 

{-------------------------------------------------------------------------------
  Switch to find source data mode 
}
procedure TfrmMap.actFindSourceDataExecute(Sender: TObject);
begin
  inherited;
  Tool:=TOOL_FIND_SOURCE_DATA;
  SetPopUpMenu(true);
end;  // TfrmMap.actFindSourceData4Execute

{-------------------------------------------------------------------------------
  Calls for the line object to be committed and then replaced with a line.
}
procedure TfrmMap.actFinishLineCurrentExecute(Sender: TObject);
begin
  FinishLineOntoSheet(CurrentDrawingLayer);
end;  // TfrmMap.actFinishLineCurrentExecute 

{-------------------------------------------------------------------------------
  Calls for the line object to be committed and then replaced with a region.  
}
procedure TfrmMap.actFinishPolygonCurrentExecute(Sender: TObject);
begin
  FinishPolygonOntoSheet(CurrentDrawingLayer);
end;  // TfrmMap.actFinishPolygonCurrentExecute

{-------------------------------------------------------------------------------
  Opens the dataset
  Initialises properties
  Creates a MapUser sheet
  Defines which sheets can be queried - which should only be the polygon layers
  Forces the map to refresh. 
}
procedure TfrmMap.ActivateMapDataSet;
var
  lExt: msExtent;
  i: Integer;
begin
  try
    Tool := TOOL_POINTER;

    // Set Base Map, that will get the list populated.
    LayerLegend.BaseMapKey := BaseMapKey;
    FDatasetSpatialSystem  := AppSettings.AvailableMaps.ItemsByKey[BaseMapKey].SpatialSystem;

    // Simply select the dataset
    FMapServerLink.ActiveDataset := FBaseMapKey + '.gds';
    // Enable polygon filling for all polygon sheets as map server tends to turn this off
    with FLayerLegend do
      for i := 0 to Count - 1 do
        if LegendItems[i] is TPolygonLayerItem then
          if FMapServerLink.SheetIDByFileName(LegendItems[i].FileName) <> -1 then
            MapSetSheetFillPoly(FMapServerLink.MapHandle,
                                FMapServerLink.SheetIDByFileName(LegendItems[i].FileName),
                                True);

    // Initialise Visible Extent Property
    MapCheck(MapGetVisibleExtent(FMapServerLink.MapHandle, lExt), ResStr_CannotSetDataSetVisible);
    VisibleExtents := lExt;

    // Initialise Scale Property
    MapCheck(MapGetScale(FMapServerLink.MapHandle, FMapScale));

    DistributionScaleX       :=  1;
    DistributionScaleY       :=  1;
    FSelectedRegion.ObjectID := -1;
    GridLineScaleX           :=  0;
    GridLineScaleY           :=  0;
    GridOffsetX              :=  0;
    GridOffsetY              :=  0;
  except
    on EMapBrowserError do begin
      Close;
      raise;
    end;
  end;// try.. except
end;  // TfrmMap.ActivateMapDataSet 

{-------------------------------------------------------------------------------
  Any time we change the list of active polygon layers, this method resets all the sub menus
      and combo box to the new list
}
procedure TfrmMap.ActivePolygonLayersChange;
var
  i: Integer;
  lLayer, lSelectedLayer: TLayerItem;
  
  { Must specify either a parent menu item or a parent menu }
  procedure NewMenuItem(AIndex: Integer; AClickMethod: TNotifyEvent;
      AParentItem: TMenuItem; AMenu: TMenu = nil);
  var
    lMenuItem: TMenuItem;
  begin
    lMenuItem := TMenuItem.Create(Self);
    with lMenuItem do begin
      Caption := FLayerLegend[AIndex].Title;
      BitMap  := TBitmap.Create;
      Bitmap.Height := 18;
      Bitmap.Width  := 18;
      Bitmap.Canvas.FillRect(Rect(0, 0, 17, 17));
      FLayerLegend[AIndex].DrawSymbol(Bitmap.Canvas, Rect(1, 1, 17, 17));
      Tag     := AIndex;
      OnClick := AClickMethod;
    end;
    if AParentItem <> nil then AParentItem.Add(lMenuItem)
                          else AMenu.Items.Add(lMenuItem);
  end;

  procedure EnableLayerActions(AEnabled: Boolean = True);
  var
    numCoords: Integer;
  begin
    if MapEditGetNumCoords(FMapServerLink.MapHandle, numCoords) <> MS_SUCCESS then
      numCoords := 0;
    mnuEditFinishPolygonAny.Enabled   := AEnabled and (numCoords > 2) and (not FDuplicatePresent);
    actFinishPolygonCurrent.Enabled   := AEnabled and (numCoords > 2) and (not FDuplicatePresent);
    pmDrawingFinishPolygonAny.Enabled := AEnabled and (numCoords > 2) and (not FDuplicatePresent);
    mnuEditFinishLineAny.Enabled      := AEnabled and (numCoords > 1);
    actFinishLineCurrent.Enabled      := AEnabled and (numCoords > 1);
    pmDrawingFinishLineAny.Enabled    := AEnabled and (numCoords > 1);
    actExtractGridSquares.Enabled     := AEnabled;
    pmBoundaryDraw.Enabled            := AEnabled;
    pmBoundaryImport.Enabled          := AEnabled;
    mnuMapAddBoundary.Enabled         := AEnabled;
  end;

begin
  // Remember the current layer so we can reselect it
  lSelectedLayer := TLayerItem(CurrentDrawingLayer);
  // Clear the combo box and sub menus
  cmbCurrentLayer.Clear;
  mnuMapMovePolygon.Clear;
  pmPolygonToLayer.Items.Clear;
  pmDrawingFinishPolygonAny.Clear;
  mnuEditFinishPolygonAny.Clear;
  pmDrawingFinishLineAny.Clear;
  mnuEditFinishLineAny.Clear;
  
  // Repopulate the combo box and sub menus
  for i := 0 to FLayerLegend.Count - 1 do begin
    lLayer := FLayerLegend[i];
    if (lLayer is TPolygonLayerItem) and lLayer.Visible then begin
      cmbCurrentLayer.Items.AddObject(lLayer.Title, lLayer);
      NewMenuItem(i, MovePolygonClick, mnuMapMovePolygon);           // main "move polygon"
      NewMenuItem(i, MovePolygonClick, nil, pmPolygonToLayer);       // toolbar "move polygon"
      NewMenuItem(i, FinishPolygonClick, pmDrawingFinishPolygonAny); // popup "finish polygon"
      NewMenuItem(i, FinishPolygonClick, mnuEditFinishPolygonAny);   // main "finish polygon"
      NewMenuItem(i, FinishLineClick, pmDrawingFinishLineAny);       // popup "finish line"
      NewMenuItem(i, FinishLineClick, mnuEditFinishLineAny);         // main "finish line"
    end;
  end;
  RefreshXPMenu;
  
  i := cmbCurrentLayer.Items.IndexOfObject(lSelectedLayer);
  if i <> -1 then begin
    EnableLayerActions;
    cmbCurrentLayer.ItemIndex := i  // reselect current layer as still available
  end else
    if cmbCurrentLayer.Items.Count > 0 then begin
      cmbCurrentLayer.ItemIndex := 0; // select first layer
      EnableLayerActions;
    end else
      EnableLayerActions(False);
  
  // This is reset if we ever have an empty popup, so reset it
  with frmMain.tbContext do
    for i := 0 to ButtonCount - 1 do
      if Buttons[i].DropDownMenu = pmPolygonToLayer then Buttons[i].Style := tbsDropdown;

  if cmbCurrentLayer.ItemIndex <> -1 then begin
    lLayer := TLayerItem(CurrentDrawingLayer);
    actFinishPolygonCurrent.Caption := Format(ResStr_Cap_FinishPolygonAdd, [lLayer.Title]);
    actFinishLineCurrent.Caption    := Format(ResStr_Cap_FinishLineAdd, [lLayer.Title]);
  end;
  cmbCurrentLayer.Invalidate;
  mnuMapExportPolygonLayer.Enabled := cmbCurrentLayer.ItemIndex <> -1;
end;  // TfrmMap.ActivePolygonLayersChange 

{-------------------------------------------------------------------------------
  Dummy handler so that this action can appear enabled 
}
procedure TfrmMap.actMovePolygonExecute(Sender: TObject);
begin
  inherited;
  // no code
end;  // TfrmMap.actMovePolygonExecute 

{-------------------------------------------------------------------------------
  Switch to pan the map mode 
}
procedure TfrmMap.actPanExecute(Sender: TObject);
begin
  inherited;
  Tool := TOOL_PAN;
  SetPopUpMenu(true);
end;  // TfrmMap.actPanExecute 

{-------------------------------------------------------------------------------
  Switch to mouse pointer mode 
}
procedure TfrmMap.actPointerExecute(Sender: TObject);
begin
  inherited;
  Tool := TOOL_POINTER;
  SetPopUpMenu(true);
end;  // TfrmMap.actPointerExecute

{-------------------------------------------------------------------------------
  Same action is used to
  (1) start the Subtract Boundary from Boundary process
  (2) select the super boundary
  (3) kick off the subtraction
}
procedure TfrmMap.actSubtractBoundaryExecute(Sender: TObject);
begin
  if actCancelSubtract.Visible then
    if (SelectedRegion.SheetID = -1) or (SelectedRegion.ObjectID = -1) then
      MessageDlg(ResStr_SelectBoundaryFirst, mtInformation, [mbOK], 0)
    else
    if (FSuperBoundaryID.SheetID = -1) or (FSuperBoundaryID.ObjectID = -1) then
      SetSuperBoundary
    else
      SubtractBoundary
  else
    if (SelectedRegion.SheetID = -1) or (SelectedRegion.ObjectID = -1) then begin
      MessageDlg(ResStr_SelectBoundaryToHole, mtInformation, [mbOK], 0);
      actSubtractBoundary.Caption := ResStr_Cap_SetBoundaryToSubtract;
      actSubtractBoundary.Caption := ResStr_Cap_SetBoundaryToSubtractFrom;
      Tool := TOOL_SUBTRACT_BOUNDARY;
    end else
      SetSuperBoundary;
end;  // TfrmMap.actSubtractBoundaryExecute 

{-------------------------------------------------------------------------------
  Called by the SetTool method. 
}
procedure TfrmMap.actSubtractBoundaryToolReset(Value: Integer);
begin
  actCancelSubtract.Enabled := Value = TOOL_SUBTRACT_BOUNDARY;
  if actCancelSubtract.Visible and not actCancelSubtract.Enabled then
    FSuperBoundaryID.SheetID := -1;
  actCancelSubtract.Visible := actCancelSubtract.Enabled;
  actSubtractBoundary.Enabled := (not actCancelSubtract.Visible) or
                                 ((SelectedRegion.SheetID <> -1) and
                                  (SelectedRegion.ObjectID <> -1));
  if actCancelSubtract.Visible then
    pnlMapPanel.PopupMenu := pmSubtractBoundary
  else begin
    actSubtractBoundary.Caption := ResStr_Cap_SubtractBoundaryLink;
    actSubtractBoundary.Hint := ResStr_Cap_SubtractBoundary;
    if pnlMapPanel.PopupMenu = pmSubtractBoundary then
      pnlMapPanel.PopupMenu := nil;
  end;
end;  // TfrmMap.actSubtractBoundaryToolReset 

{-------------------------------------------------------------------------------
  Switch to zoom out map mode 
}
procedure TfrmMap.actUnZoomExecute(Sender: TObject);
begin
  inherited;
  Tool := TOOL_ZOOM_OUT;
  SetPopUpMenu(true);
end;  // TfrmMap.actUnZoomExecute 

{-------------------------------------------------------------------------------
  Switch to zoom the map mode 
}
procedure TfrmMap.actZoomExecute(Sender: TObject);
begin
  inherited;
  Tool := TOOL_ZOOM_IN;
  SetPopUpMenu(true);
end;  // TfrmMap.actZoomExecute 

{-------------------------------------------------------------------------------
}
function TfrmMap.AddDatasetItem(AItem: TDatasetItem; const AName: String): Boolean;
begin
  if DatasetLegend.AddItem(AItem) then begin
    Result := True;
    if AppSettings.MapSingleDataset then
      while DatasetLegend.Count > 1 do DatasetLegend.DeleteItem(0);
    AItem.Title := AName;
    AItem.OnNeedRefresh := MapDatasetItemRefresh;
    MapDatasetMenuRefresh;
  end else
    Result := False;
end;  // TfrmMap.AddDatasetItem 

{-------------------------------------------------------------------------------
}
procedure TfrmMap.AddinDataDropped(AClsID: TGuid; AKeyList: TKeyList);
var
  lAddin: IMapDropFormat;
  lKeys: IKeyList;
begin
  lKeys := TComKeyList.Create(AKeyList) as IKeyList;
  lAddin := CreateComObject(AClsID) as IMapDropFormat;
  lAddin.InitialiseDataset(lKeys);

  if CreateDistributionList(nil, nil, lAddin, ReadableFormat(lAddin.DatasetTableName),
                                    lAddin.DatasetTableName, '') = nil then
    MessageDlg(ResStr_NoRecordings, mtInformation, [mbOk], 0);
end;  // TfrmMap.AddinDataDropped

{-------------------------------------------------------------------------------
  When a new sheet is added for polygon drawing, this method actually attaches it to the map.
      Return result is the index of the polygon layer for Map Server, or -1 if it already
      existed.
}
procedure TfrmMap.AddPolygonLayerToMap(const AMapSheetKey: TKeyString);
var
  lOrigNorth, lOrigSouth, lOrigWest, lOrigEast: Double;
  lSheetIndex: Integer;
  lSheetType: Integer;
  lFileName: String;
  lDestFile, lSourceFile: String;
begin
  MapCheck(MapGetSheetExternalExtents(FMapServerLink.Maphandle, 0,
                                      lOrigNorth, lOrigSouth, lOrigEast, lOrigWest));

  lFileName := AppSettings.ObjectSheetFilePath + AMapSheetKey + '.gsf';
  if FileExists(lFileName) then begin
    { This should only occur if we have had a failure setting up the sheet
      previously, in which case we throw it away and start again }
    DetachSheetByFileName(lFileName);
    DeleteFiles(AppSettings.ObjectSheetFilePath, AMapSheetKey + '.*');
  end;

  lSourceFile := AppSettings.BaseMapPath + 'SheetSource';
  lDestFile   := Appsettings.ObjectSheetFilePath + AMapSheetKey;
  CopyFile(PChar(lSourceFile + '.gdb'), PChar(lDestFile + '.gdb'), False);
  CopyFile(PChar(lSourceFile + '.gix'), PChar(lDestFile + '.gix'), False);
  CopyFile(PChar(lSourceFile + '.gsf'), PChar(lDestFile + '.gsf'), False);
  CopyFile(PChar(lSourceFile + '.mdx'), PChar(lDestFile + '.mdx'), False);

  MapCheck(MapAttachSheet(FMapServerLink.MapHandle, PChar(lFileName)), 'Failed to create the sheet layer files. '+
      'This may be because the Object Sheet File Path ('+AppSettings.ObjectSheetFilePath+') cannot be written to by Recorder because of file permissions.');

  lSheetIndex := FMapServerLink.SheetIDByFileName(lFileName);
  MapSetSheetExternalExtents(FMapServerLink.Maphandle, lSheetIndex, lOrigNorth,
                             lOrigSouth, lOrigEast, lOrigWest);
  MapCheck(MapGetSheetType(FMapServerLink.MapHandle, lSheetIndex, lSheetType));

  AppSettings.UpdateMapWindowSelectors;
end;  // TfrmMap.AddPolygonLayerToMap

{-------------------------------------------------------------------------------
  Prevents spatial reference strings from being presented with Spurious Accuracy but only for 
      UK and Irish Grid References.
}
function TfrmMap.AllowForAccuracy(const ASpatialReference, ASystem: String):
    String;
var
  lNPix: Double;
  lStringlNPix: String;
  lLength, i: Integer;
  lPrefix, lEasting, lNorthing: String;
  lValidSR: TValidSpatialRef;
  lNumbers: String;
  lLatLong: TLatLong;
begin
    { Take an area of the number of pixels specified and find out how many metres that would be
      i.e. if it = 200 meters, then the Last 3 zeros should be removed }
  lValidSR := ValidSpatialRef(ASpatialReference, ASystem, ASystem);
    { if it IS valid... }
  if lValidSR.Valid then begin
    if (ASystem = OS_GB) or (ASystem = OS_NI) then begin
      { Format 'SV 12345 67890' as 'SV 12 67' }
      lNPix := SizeOfNPixels;
      lStringlNPix := FloatToStr(trunc(lNPix));
      lLength := Length(lStringlNPix) -1;
      { Find the prefix }
      i := 1;
      while (i < Length(lValidSR.FormattedSR)) and (lValidSR.FormattedSR[i] in ['A'..'Z']) do
        Inc(i);
      lPrefix := Copy(lValidSR.FormattedSR, 1, i - 1);
      lNumbers:= Copy(lValidSR.FormattedSR, i, length(lValidSR.FormattedSR));
      { Split numbers into east/north }
      lEasting := Copy(lNumbers, 1, Length(lNumbers) div 2);
      lNorthing:= Copy(lNumbers, Length(lNumbers) div 2 + 1, length(lNumbers));

      { iGridReference is always returned in the for ##_#####_#####
                                                     12345678901234
      Remove from 14 and Remove from 8 }
      lEasting := Copy(lEasting, 1, Length(lEasting) - lLength);
      lNorthing:= Copy(lNorthing, 1, Length(lNorthing) - lLength);
      Result   := lPrefix + lEasting + lNorthing;
    end else
    if ASystem = LAT_LONG then begin
      lLatLong:=SpatialRefFuncs.FormattedLTLNToLTLN(ASpatialReference);
      Result:=LTLNToAccurateFormattedLTLN(lLatLong);
    end else
      Result:=ASpatialReference;
  end else
    Result := ASpatialReference;// if not a valid spatialref
end;  // TfrmMap.AllowForAccuracy

{-------------------------------------------------------------------------------
  On dropping a taxon onto the map, this calls methods to create an FEventList and an
      FSampleList of Event Keys dropped and of the samples contained within those events
  Dependencies: FMapHandle, FMapServerLink.dmMap must exist
  Side Effects: Creates TMapDrawItems and populates the taxon list with them 
  Special Notes: Triggers a WMU_MAPUSER message and redraws the map. 
}
procedure TfrmMap.BiotopeDropped(iSourceData: TKeyList);
var
  i: Integer;
  lKeys: String;
  lTitle: String;
  lRS: _Recordset;
begin
  lKeys := '''' + iSourceData.Items[0].KeyField1 + '''';
  lTitle := dmGeneralData.GetBiotopeCodeName(iSourceData.Items[0].KeyField1);
  for i := 0 to iSourceData.Header.ItemCount - 1 do
    lKeys := lKeys + ',''' + iSourceData.Items[i].KeyField1 + '''';

  lRS := dmDatabase.ExecuteSQL(Format(SQL_MAP_BIOTOPES_LIST,
      [AppSettings.UserID, lKeys]), True);
  if CreateDistributionList(nil, lRS, nil, lTitle,
                            TN_BIOTOPE_LIST_ITEM, PK_BIOTOPE_LIST_ITEM) = nil then
    MessageDlg(ResStr_NoRecordings, mtInformation, [mbOk], 0);
end;  // TfrmMap.BiotopeDropped

{-------------------------------------------------------------------------------
  Spatial data is held in the boundingBox property as LatLong. This converts to spatial 
      reference strings. 
}
function TfrmMap.BoundingBoxLLtoSpatialRef(const iBox: TBoundingBox): TStringCoord;
begin
  if ValidLatLongRecord(iBox.NorthEast) and ValidLatLongRecord(iBox.SouthWest) then begin
    try
      Result.x := ConvertFromLatLong(iBox.NorthEast, Appsettings.SpatialRefSystem);
    except
      on EOutOfRangeError do
        Result.x := ConvertOutOfRangeToSystemLimit(iBox.NorthEast,
                                                   Appsettings.SpatialRefSystem);
    end;
    try
      Result.y := ConvertFromLatLong(iBox.SouthWest, Appsettings.SpatialRefSystem);
    except
      on EOutOfRangeError do
        Result.y := ConvertOutOfRangeToSystemLimit(iBox.SouthWest,
                                                   Appsettings.SpatialRefSystem);
    end;
  end;
end;  // TfrmMap.BoundingBoxLLtoSpatialRef

{-------------------------------------------------------------------------------
  if in draw polygon mode, this clears any drawing objects from the Edit store before doing
      any other actions.
}
procedure TfrmMap.CancelDrawing;
begin
  if FFirstCoord = False then begin
    MapCheck(MapEditClearObject(FMapServerLink.MapHandle));
    FFirstCoord := True;
  end;
  RefreshMap;
  EnableDrawMenu(False);
end;  // TfrmMap.CancelDrawing

{-------------------------------------------------------------------------------
}
class procedure TfrmMap.CentreOnLocation(const ALocationKey: TKeyString; const ALocationName:
    String = '');
var
  lMapWindow: TfrmMap;
  lRS: _Recordset;
begin
  lRS := dmDatabase.ExecuteSQL('SELECT Map_Sheet_Key, Object_ID FROM Location_Boundary ' +
                               ' WHERE Location_Key = ''' + ALocationKey +
                               ''' ORDER BY ENTRY_DATE DESC', True);
  // If no link between location and a map sheet, go for spatial reference, and default map.
  if lRS.Eof or (lRS.Fields['Object_ID'].Value = -1) then
  begin
    lRS.Close;
    lRS := dmDatabase.ExecuteSQL('SELECT Spatial_Ref, Spatial_Ref_System, Lat, Long ' +
                                 'FROM Location WHERE Location_Key = ''' + ALocationKey + '''',
                                     True);
    if lRS.Eof then begin
      lRS.Close;
      raise EMapBrowserError.CreateNonCritical('Location Not Found.');
    end else begin
      TfrmMap.CentreOnRef(VarToStr(lRS.Fields['Spatial_Ref'].Value),
                          lRS.Fields['Spatial_Ref_System'].Value, ALocationName);
      lRS.Close;
    end;
  end else begin
    // Get the correct map up.
    with dmDatabase.ExecuteSQL('SELECT Base_Map_Key FROM Map_Sheet WHERE Map_Sheet_Key = ''' +
                               lRS.Fields[PK_MAP_SHEET].Value + '''', True) do
    begin
      lMapWindow := dmFormActions.MapWindow(Fields[FK_BASE_MAP_KEY].Value, True);
      if Assigned(lMapWindow) and not lMapWindow.IsBoundaryImportInProgress then
      begin
        lMapWindow.BringToFront;
        lMapWindow.SetFocus;
        lMapWindow.Activate;
      end;
      Close;
    end;
    if Assigned(lMapWindow) and not lMapWindow.IsBoundaryImportInProgress then
      lMapWindow.CentreOnObject(lRS.Fields[PK_MAP_SHEET].Value,
                                lRS.Fields['Object_ID'].Value, ALocationName);
    lRS.Close;
  end;
end;  // TfrmMap.CentreOnLocation

{-------------------------------------------------------------------------------
}
procedure TfrmMap.CentreOnObject(AMapSheetKey: TKeyString; AStaticID: Integer; const
    ALocationName: String = ''; AWithZoom: Boolean = True; AWithSelect: Boolean = True);
var
  lExtent: MSExtent;
  lSheetID: Integer;
  lLayer: TLayerItem;
  lSelectedTool: Byte;
  lObjectID: Integer;
begin
  lSheetID := -1;
  // Find SheetID in map dataset from layer's filename.
  lLayer := FLayerLegend.LayerByKey(AMapSheetKey);
  if Assigned(lLayer) then
    lSheetID := FMapServerLink.SheetIDByFileName(lLayer.FileName);

  if lSheetID <> -1 then begin
    // Get ObjectID from StaticID first.
    MapCheck(MapGetObjectFromStaticID(FMapServerLink.MapHandle, lSheetID, AStaticID, lObjectID));
    // Now get Object's details.
    MapCheck(MapGetObjectExtents(FMapServerLink.MapHandle, lSheetID, lObjectID, lExtent));

    if AWithZoom then
      if MapCanvasSize.X / (lExtent.dEast - lExtent.dWest) >
         MapCanvasSize.Y / (lExtent.dNorth - lExtent.dSouth) then
        MapScale := MapScale *
                    (lExtent.dNorth - lExtent.dSouth) * 2 /
                    (MapCanvasSize.Y * PixelToCoordFactor.Y)
      else
        MapScale := MapScale *
                    (lExtent.dEast - lExtent.dWest) * 2 /
                    (MapCanvasSize.X * PixelToCoordFactor.X);

    MapCheck(MapSetViewOrigin(
        FMapServerLink.MapHandle,
        (lExtent.dEast + lExtent.dWest) / 2,
        (lExtent.dNorth + lExtent.dSouth) / 2));
    FObjectDetails.MapPosition.X := 0;
    FObjectDetails.MapPosition.Y := 0;
    FObjectDetails.ObjectID      := lObjectID;
    FObjectDetails.SheetID       := lSheetID;
    GetObjectInfo(FObjectDetails);

    if AWithSelect then begin
      lSelectedTool := Tool;
      // Make sure tool is POINTER before selecting polygon, or it might not work.
      Tool := TOOL_POINTER;
      SelectedRegion := PolygonID(lSheetID, lObjectID);
      // Reselect previous tool, so it's transparent to user.
      Tool := lSelectedTool;
    end;
    QuerySpatialReference;

    if ALocationName = '' then pnlMapPanel.Hint := eLocation.Text
                          else pnlMapPanel.Hint := ALocationName;
    ShownHint := False;
  end;
end;  // TfrmMap.CentreOnObject

{-------------------------------------------------------------------------------
  Takes a location key and spatial ref.  If the ref is specified, then centres the map on it,
  else find's the location's ref and centres on that.  Also brings the map to the front.
}
class procedure TfrmMap.CentreOnPlace(const ALocationKey, ALocationName, ASpatialRef, ASystem: String);
begin
  if (ALocationKey = '') and (ASpatialRef = '') then
    raise EMapBrowserError.CreateNonCritical(ResStr_NoInfoToLocateMap);
  // Pass the buck to another function that will know what to do.
  if ASpatialRef = '' then TfrmMap.CentreOnLocation(ALocationKey, ALocationName)
                      else TfrmMap.CentreOnRef(ASpatialRef, ASystem, ALocationName);
end;  // TfrmMap.CentreOnPlace 

{-------------------------------------------------------------------------------
}
class procedure TfrmMap.CentreOnRef(const ASpatialRef, ASystem: String; const ALocationName: 
    String = '');
var
  lCoord: TMapCoord;
  lMapWindow: TfrmMap;
  lBaseMapKey: TKeyString;
begin
  if ASpatialRef = ResStr_BeyondSystemLimits then
    raise EMapBrowserError.CreateNonCritical(ResStr_SRBeyondLimits);

  lBaseMapKey := '';
  // Only one map, can't be any other. Otherwise, ask which one to use.
  if AppSettings.AvailableMaps.Count = 1 then
    lBaseMapKey := AppSettings.AvailableMaps[0].BaseMapKey
  else
    with TdlgMapSelection.Create(nil) do
      try
        if ShowModal = mrOk then
          lBaseMapKey := BaseMapKey;
      finally
        Free;
      end;

  // Only proceed if a map has been selected.
  if lBaseMapKey <> '' then begin
    // Go for first map, or default if none already displayed.
    lMapWindow := dmFormActions.MapWindow(lBaseMapKey, True);
    if Assigned(lMapWindow) and not lMapWindow.IsBoundaryImportInProgress then
      with lMapWindow do begin
        BringToFront;
        SetFocus;
        Activate;

        lCoord := SpatialRefToSpecificEastNorth(
            ASpatialRef, ASystem, lMapWindow.DatasetSpatialSystem);
        MapCheck(MapSetViewOrigin(FMapServerLink.MapHandle, lCoord.x, lCoord.y));

        if ALocationName = '' then pnlMapPanel.Hint := ASpatialRef
                              else pnlMapPanel.Hint := ALocationName;
        ShownHint := False;
      end;
  end;
end;  // TfrmMap.CentreOnRef

{-------------------------------------------------------------------------------
  Creates the required DataModule and MapWindow.
  Sets up the dataset and imports / deletes / modifies config sheets.
  Sets up the menus etc.
  Creates objects
}
procedure TfrmMap.ChangedMapDataset;
var
  lRect: TRect;
  lScaleList: IBaseMapScaleList;
  i: Integer;
  lMenuItem, lToolbarItem, lMenuSep, lMenuNone, lToolbarSep, lToolbarNone: TMenuItem;
begin
  frmMain.SetStatus(ResStr_Setup, False);
  FLastSheetRefresh := 0;

  // Before we change the dataset.
  DeleteMapUserSheets;
  // Initialise the Dataset
  ActivateMapDataSet;
  FindSettingsBySRSystem;
  CreateMapUserSheet;
  RefreshMapSheets;

  // Set up the pointer tool.
  actPointer.Checked := True;
  
  SelectedRegion     := PolygonID(0, -1);
  lRect.Top          := 0;
  lRect.Left         := 0;
  lRect.Bottom       := pnlDockSiteLeft.Height;
  lRect.Right        := pnlDockSiteLeft.Width;
  eSpatialRef.Text   := '';
  eLocation.Text     := '';

  // Align left panel stuff correctly.
  pnlSelectedPolySheet.Align := alNone;
  pnlSelectedPolySheet.Align := alBottom;
  ApplySecurity;

  if GetComBaseMapSystem and
    Supports(FComMapFormat, IID_IBaseMapScaleList, lScaleList) then
    begin
      lScaleList   := (FComMapFormat as IBaseMapScaleList);
      lMenuSep     := TMenuItem.Create(nil);
      lMenuNone    := TMenuItem.Create(nil);
      lToolbarSep  := TMenuItem.Create(nil);
      lToolbarNone := TMenuItem.Create(nil);
      lMenuSep.Caption     := '-';
      lMenuNone.Caption    := ResStr_Cap_Default;
      lMenuNone.OnClick    := mnuMapDistributionPointsClick;
      lToolbarSep.Caption  := '-';
      lToolbarNone.Caption := ResStr_Cap_Default;
      lToolbarNone.OnClick := mnuMapDistributionPointsClick;
      mnuMapDistributionPoints.Clear;
      pmDistributionPoints.Items.Clear;
      for i := 0 to lScaleList.PointSizeCount - 1 do
      begin
        lMenuItem := TMenuItem.Create(nil);
        lMenuItem.Caption    := lScaleList.PointSizeCaption[i];
        lMenuItem.OnClick    := mnuMapDistributionPointsClick;
        lToolbarItem         := TMenuItem.Create(nil);
        lToolbarItem.Caption := lScaleList.PointSizeCaption[i];
        lToolbarItem.OnClick := mnuMapDistributionPointsClick;
        mnuMapDistributionPoints.Add(lMenuItem);
        pmDistributionPoints.Items.Add(lToolbarItem);
      end;
      mnuMapDistributionPoints.Add(lMenuSep);
      mnuMapDistributionPoints.Add(lMenuNone);
      pmDistributionPoints.Items.Add(lToolbarSep);
      pmDistributionPoints.Items.Add(lToolbarNone);
      pmDistributionPoints.Items[pmDistributionPoints.Items.IndexOf(lToolbarNone)].Default := true;

      lMenuSep     := TMenuItem.Create(nil);
      lMenuNone    := TMenuItem.Create(nil);
      lToolbarSep  := TMenuItem.Create(nil);
      lToolbarNone := TMenuItem.Create(nil);
      lMenuSep.Caption     := '-';
      lMenuNone.Caption    := ResStr_Cap_None;
      lMenuNone.OnClick    := mnuMapGridLinesClick;
      lToolbarSep.Caption  := '-';
      lToolbarNone.Caption := ResStr_Cap_None;
      lToolbarNone.OnClick := mnuMapGridLinesClick;
      mnuMapGridLines.Clear;
      pmGridLines.Items.Clear;
      for i:=0 to lScaleList.GridScaleCount-1 do
      begin
        lMenuItem := TMenuItem.Create(nil);
        lMenuItem.Caption    := lScaleList.GridScaleCaption[i];
        lMenuItem.OnClick    := mnuMapGridLinesClick;
        lToolbarItem         := TMenuItem.Create(nil);
        lToolbarItem.Caption := lScaleList.GridScaleCaption[i];
        lToolbarItem.OnClick := mnumapGridLinesClick;
        mnuMapGridLines.Add(lMenuItem);
        pmGridLines.Items.Add(lToolbarItem);
      end;
      mnuMapGridLines.Add(lMenuSep);
      mnuMapGridLines.Add(lMenuNone);
      pmGridLines.Items.Add(lToolbarSep);
      pmGridLines.Items.Add(lToolbarNone);
      pmGridLines.Items[pmGridLines.Items.IndexOf(lToolbarNone)].Default := true;
      
    end else begin
      mnuMapDistributionPoints.Clear;
      mnuMapGridLines.Clear;  
      pmDistributionPoints.Items.Clear;
      pmGridLines.Items.Clear;
      for i := 0 to 6 do
      begin
        lMenuItem := TMenuItem.Create(nil);
        lToolbarItem := TMenuItem.Create(nil);
        if i <> 5 then
        begin
          lMenuItem.OnClick := mnuMapDistributionPointsClick;
          lToolbarItem.OnClick := mnuMapDistributionPointsClick;
        end;
        case i of
          0: lMenuItem.Caption := '10 kms';
          1: lMenuItem.Caption := '5 kms';
          2: lMenuItem.Caption := '2 kms';
          3: lMenuItem.Caption := '1 km';
          4: lMenuItem.Caption := '100 m';
          5: lMenuItem.Caption := '-';
          6: lMenuItem.Caption := ResStr_Cap_Default;
        end;
        lToolbarItem.Caption := lMenuItem.Caption;
        mnuMapDistributionPoints.Add(lMenuItem);
        pmDistributionPoints.Items.Add(lToolbarItem);
      end;
      pmDistributionPoints.Items[pmDistributionPoints.Items.Count-1].Default := true;
      for i := 0 to 5 do
      begin
        lMenuItem    := TMenuItem.Create(nil);
        lToolbarItem := TMenuItem.Create(nil);
        if i <> 4 then
        begin
          lMenuItem.OnClick    := mnuMapGridLinesClick;
          lToolbarItem.OnClick := mnuMapGridLinesClick;
        end;
        case i of
          0: lMenuItem.Caption := '100 kms';
          1: lMenuItem.Caption := '50 kms';
          2: lMenuItem.Caption := '10 kms';
          3: lMenuItem.Caption := '1 km';
          4: lMenuItem.Caption := '-';
          5: lMenuItem.Caption := ResStr_Cap_None;
        end;
        lToolbarItem.Caption := lMenuItem.Caption;
        mnuMapGridLines.Add(lMenuItem);
        pmGridLines.Items.Add(lToolbarItem);
      end;
      pmGridLines.Items[pmGridLines.Items.Count-1].Default := true;
    end;

  frmMain.SetStatus('');
end;  // TfrmMap.ChangedMapDataset

{-------------------------------------------------------------------------------
}
procedure TfrmMap.ChangeMapUserExtents;
var
  lExtents: MSExtent;
begin
  if FMapWindowOpen then begin
    lExtents := ReadMapExtents;
    if MapUserSheetID<>-1 then  // is user sheet ready
      MapCheck(MapSetSheetExternalExtents(FMapServerLink.MapHandle, MapUserSheetID,
                                          lExtents.dNorth, lExtents.dSouth,
                                          lExtents.dEast, lExtents.dWest));
  end;
end;  // TfrmMap.ChangeMapUserExtents 

{-------------------------------------------------------------------------------
  This function returns true if the lat long coordinates in the input represent points inside 
      the bounding box defined by iBotRight, iTopLeft
}
function TfrmMap.CheckPointInBoundingBox(iLat,iLong: Double; iBotLeft, iTopRight: msCoord): Boolean;
var
  lDataPoint: TMapCoord;
  lLatLong: TLatLong;
begin
  lLatLong   := LatLong(iLat, iLong);
  lDataPoint := SpatialRefFuncs.LatLongToSpecificEN(lLatLong, DatasetSpatialSystem);
  Result := (lDataPoint.X >= iBotLeft.X) and (lDataPoint.X <= iTopRight.X) and
            (lDataPoint.Y >= iBotLeft.Y) and (lDataPoint.Y <= iTopRight.Y);
end;  // TfrmMap.CheckPointInBoundingBox

{-------------------------------------------------------------------------------
  This function checks whether the two points are within 2 pixels of each other. Only used
      when DistributionScale=1
}
function TfrmMap.CheckPointInCircle(iLat, iLong: Double; iPosition: TLatLong): Boolean;
var
  lCursorPoint, lOtherPoint: TPoint;
begin
  lCursorPoint := MapToScreen(iPosition.Lat, iPosition.Long, VisibleExtents);
  lOtherPoint  := MapToScreen(iLat, iLong, VisibleExtents);
  Result := (Power(lCursorPoint.X - lOtherPoint.X, 2) +
             Power(lCursorPoint.Y - lOtherPoint.Y, 2)) <= 4;
end;  // TfrmMap.CheckPointInCircle 

{-------------------------------------------------------------------------------
}
procedure TfrmMap.cmbActiveMapChange(Sender: TObject);
var
  lMapWindow: TfrmMap;
  lMap: TAvailableMap;
begin
  inherited;

  with cmbActiveMap do begin
    lMap := TAvailableMap(Items.Objects[ItemIndex]);
    Caption := Format(ResStr_Cap_MapWindow,[lMap.DisplayName]);;
    lMapWindow := dmFormActions.MapWindow(lMap.BaseMapKey);
    if Assigned(lMapWindow) then begin
      lMapWindow.BringToFront;
      lMapWindow.SetFocus;
      // Reselect correct item in current window.
      ItemIndex := Items.IndexOf(AppSettings.AvailableMaps.ItemsByKey[BaseMapKey].DisplayName);
    end else begin
      FBaseMapKey := lMap.BaseMapKey;
      ChangedMapDataset;
      Application.ProcessMessages;
      RefreshXPMenu;
    end;
  end;
end;  // TfrmMap.cmbActiveMapChange

{-------------------------------------------------------------------------------
}
procedure TfrmMap.cmbCurrentLayerChange(Sender: TObject);
begin
  ActivePolygonLayersChange;
end;  // TfrmMap.cmbCurrentLayerChange

{-------------------------------------------------------------------------------
  Draw the item using the attached object so it stays in synch
}
procedure TfrmMap.cmbCurrentLayerDrawItem(Control: TWinControl; Index: Integer; ARect: TRect;
    State: TOwnerDrawState);
var
  lItem: TLayerItem;
begin
  inherited;

  with cmbCurrentLayer do begin
    Canvas.FillRect(ARect);
    lItem := TLayerItem(Items.Objects[Index]);
    if Assigned(lItem) then begin
      lItem.DrawSymbol(Canvas, Rect(ARect.Left +  2, ARect.Top,
                                    ARect.Left + 18, ARect.Top + 16));
      Canvas.TextOut(ARect.Left + 20, ARect.Top + 2, lItem.Title);
    end;
  end;
end;  // TfrmMap.cmbCurrentLayerDrawItem 

{-------------------------------------------------------------------------------
  Takes an object drawn on any sheet and re draws it on the supplied polygon sheet id
  - if drawing directly onto the object sheet, objects are initially always MS_LINE_OBJECTs,
      so to make them shaded regions their points have to be copied from the object sheet, the 
      line deleted and a MS_REGION_OBJECT drawn.
  - while importing objects, the sheet is temporarily attached to the dataset and then each 
      object is selected and then copied before the sheet is detached.
  Dependencies: FMapHandle, FMapServerLink.dmMap must exist
  Returns: The Static ID of the created object.
}
function TfrmMap.CopyObjectToObjectSheet(ASourceSheetID, AObjectID, ADestinationSheetID,
    AObjectType: Integer; AImporting, ACopyKeyValue: Boolean): Integer;
var
  pKeyValue: Array[0..20] of char;
  pBoundaryKey: Array[0..20] of char;
  lLayer: TPolygonLayerItem;
begin
  // if necessary, then get the KeyValue
  if ACopyKeyValue then
  begin
    { Read to see if there is Key Value in the Object Sheets 'KEYVALUE' field.
      MS_FIELD_KEYVALUE is the index of the field (i.e. 2).
  
      The Last parameter is the number of characters to return.  Because the
      KeyString is 16 Char long, and using 16 characters does not return the
      full KeyString then we use 20 }
    MapCheck(MapDataGetAttribute(FMapServerLink.MapHandle, ASourceSheetID,
                                 MS_FIELD_LOCATION_KEY, AObjectID, pKeyValue, 20));
    MapCheck(MapDataGetAttribute(FMapServerLink.MapHandle, ASourceSheetID,
                                 MS_FIELD_LOCATION_BOUNDARY_KEY, AObjectID, pBoundaryKey, 20));
  end;
  MapCheck(MapEditSelectObject(FMapServerLink.MapHandle, ASourceSheetID, AObjectID));
  // Set object onto main object sheet
  MapCheck(MapEditSetSheet(FMapServerLink.MapHandle, ADestinationSheetID));
  // Set object to correct display format
  lLayer := TPolygonLayerItem(LayerBySheetID(ADestinationSheetID));
  Result := SetMapObjectDetails(AObjectType, True, lLayer.UnSelectedColour, Integer(lLayer.MSPattern),
                      ACopyKeyValue, pKeyValue, pBoundaryKey);
end;  // TfrmMap.CopyObjectToObjectSheet 

{-------------------------------------------------------------------------------
  This function encapsulate the creation of a new distribution list, or the adding of new 
      items to an existing one. The result value is either the newly create list, or the 
      existing one containing the additional items.
}
function TfrmMap.CreateDistributionList(AItem: TDatasetItem; ARecordset: _Recordset;
  AMapPoints: IMapDropFormat; const ATitle, ATableName, AKeyField: String): TDatasetItem;
var
  lItem: TDatasetItem;
  lDrawItem: TMapDrawItem;
  lCursor: TCursor;
  i: Integer;
begin
  lCursor := HourglassCursor;
  try
    if Assigned(AItem) then
      lItem := AItem
    else begin
      lItem := TDatasetItem.Create;
      lItem.TableName := ATableName;
      lItem.OnNeedRefresh := MapDatasetItemRefresh;
    end;

    // Comming from Recorder.
    if Assigned(ARecordset) then
      with ARecordset do begin
        while not Eof do begin
          lDrawItem := TMapDrawItem.Create;
          lDrawItem.InitFromRecord(Fields, AKeyField);
          lDrawItem.Displayed := PointDisplayedStatus(lDrawItem.SpatialRef, lDrawItem.SpatialSys);
          lItem.DistributionPoints.Add(lDrawItem);
          MoveNext;
        end;
        Close;
      end
    else
      // Comming from addin.
      for i := 0 to AMapPoints.MapPointCount - 1 do begin
        lDrawItem := TMapDrawItem.Create;
        lDrawItem.InitFromInterface(AMapPoints.MapPoint[i]);
        lDrawItem.Displayed := ((lDrawItem.Accuracy <= DistributionScaleX) and
                                (lDrawItem.Accuracy <= DistributionScaleY)) or
                               (DistributionScaleX = 1);
        lItem.DistributionPoints.Add(lDrawItem);
      end;

    // Clean up if nothing happened
    if lItem.DistributionPoints.Count = 0 then begin
      // free the created event list
      lItem.Free;
      Result := nil;
    end else
    // Add the dataset item to list only if it's a new one. Otherwise, it means we just
    // wanted to add stuff to an existing one.
    if Assigned(AItem) then
      // DatasetItem already exists.
      Result := lItem
    else
    if AddDatasetItem(lItem, ATitle) then begin
      // Managed to add a new dataset item to the legend.
      Result := lItem;
      RefreshMap;
    end else begin
      // Legend didn't want new item. Full or only 1 item allowed.
      lItem.Free;
      Result := nil;
    end;
  finally
    DefaultCursor(lCursor);
  end;
end;  // TfrmMap.CreateDistributionList

{-------------------------------------------------------------------------------
  Creates a MapUser sheet and attaches it to the dataset. Deletes any old MapUser sheet before 
      doing so. The MapUser sheet is the only way we have to form a 'Canvas' on which to draw
      gridlines and distributions.
  Side Effects: Adds to the SheetTotal property. 
}
procedure TfrmMap.CreateMapUserSheet;
begin
  if not Assigned(FMapServerLink) then Exit;

  { Delete any old MapUser sheets }
  DeleteMapUserSheets;
  MapCheck(MapUserCreateNewSheet(FMapServerLink.MapHandle,
                                 PChar(AppSettings.MapFilePath + MAPUSER_FILENAME),
                                 MAPUSER_SHEET_NAME, MAPUSER_SHEET_DESCRIPTION),
                                 AppSettings.MapFilePath + MAPUSER_FILENAME);

  MapCheck(MapUserRegisterHwnd(FMapServerLink.MapHandle, Handle),
           AppSettings.MapFilePath + MAPUSER_FILENAME);
  ChangeMapUserExtents;
end;  // TfrmMap.CreateMapUserSheet

{-------------------------------------------------------------------------------
  A wrapper for the MapServer.dll call and error handling.
  Pass in an object type and this will create a new Edit object.
}
procedure TfrmMap.CreateNewMapObject(iObjectType: byte);
var
  coords: integer;
begin
  coords := 0;
  // dont' mapcheck this, as it fails if no object
  MapEditGetNumCoords(FMapServerLink.MapHandle, coords);
  if coords>0 then
    MapCheck(MapEditClearObject(FMapServerLink.MapHandle));
  MapCheck(MapEditCreateNewObject(FMapServerLink.MapHandle, SelectedSheetID));
  MapCheck(MapEditSetObjectType(FMapServerLink.MapHandle, iObjectType));
end;  // TfrmMap.CreateNewMapObject

{-------------------------------------------------------------------------------
  General purpose function to return the TPolygonLayer currently selected in the combo box  
}
function TfrmMap.CurrentDrawingLayer: TObject;
begin
  if cmbCurrentLayer.ItemIndex <> -1 then
    Result := TObject(cmbCurrentLayer.Items.Objects[cmbCurrentLayer.ItemIndex])
  else
    Result := nil; // no current layer selected
  mnuMapExportPolygonLayer.Enabled := Assigned(Result);
end;  // TfrmMap.CurrentDrawingLayer 

{-------------------------------------------------------------------------------
  Means that querying for details about objects can only be done for the Object Sheet.  Saves 
      confusion. 
}
procedure TfrmMap.DefineSheetQueryStatus;
var
  lRet: Integer;
  i: Integer;
  lLayer: TLayerItem;
  lSheetID: Integer;
begin
  for i := 0 to FLayerLegend.Count - 1 do begin
    lLayer := FLayerLegend[i];
    lSheetID := FMapServerLink.SheetIDByFileName(lLayer.FileName);
    if lSheetID = -1 then
      raise EMapBrowserError.Create(Format(ResStr_CantFindSheet, [lLayer.FileName]));
    if lLayer is TPolygonLayerItem then
      lRet := MapSetQuerySheet(FMapServerLink.MapHandle, lSheetID, True)
    else
      lRet := MapSetQuerySheet(FMapServerLink.MapHandle, lSheetID, False);
    MapCheck(lRet, Format(ResStr_ErrorSheetQryStatus, [lLayer.FileName]));
  end;
end;  // TfrmMap.DefineSheetQueryStatus 

{-------------------------------------------------------------------------------
  Calls the methof to detach the sheet from the dataset and then removes the file created when
      the MapUser sheet was created.  A new map user sheet is created each time that the map
      window is created because its the only way we could get it to work. 
}
procedure TfrmMap.DeleteMapUserSheets;
var
  i: Integer;
begin
  if not Assigned(FMapServerLink) then Exit;

  {Select none on GridLines menu and default on Survey data}
  GridLineScaleX := 0;
  GridLineScaleY := 0;
  GridOffsetX    := 0;
  GridOffsetY    := 0;
  DistributionScaleX := 1;
  DistributionScaleY := 1;
  for i := FMapServerLink.SheetTotal - 1 downto 0 do
    if CompareText(FMapServerLink.SheetName(i), MAPUSER_SHEET_NAME) = 0 then
      MapDetachSheet(FMapServerLink.MapHandle, i);
  
  { Delete Old GridlineSheet .gsf file }
  if FileExists(AppSettings.MapFilePath + MAPUSER_FILENAME) then
    DeleteFile(AppSettings.MapFilePath + MAPUSER_FILENAME);
end;  // TfrmMap.DeleteMapUserSheets

{-------------------------------------------------------------------------------
  Deletes any currently selected region and any associated boundary held in the database 
  Forces the map to refresh.
}
procedure TfrmMap.DeletePolygon;
var
  lKey: String;
  lStaticID, liResult: Integer;
begin
  if SelectedRegion.ObjectID > -1 then begin
    if MessageDlg(ResStr_DeleteSelRegion, mtConfirmation ,[mbYes, mbNo],0) = mrNo then
      Exit
    else begin
      lStaticID := FMapServerLink.StaticIDforObject(SelectedRegion.SheetID,
                                                    SelectedRegion.ObjectID);
      lKey := ReadMapDatabase(SelectedRegion.SheetID, SelectedRegion.ObjectID,
                              MS_FIELD_LOCATION_KEY);

      if (lKey <> '') and dmGeneralData.CheckKeyExists(TN_LOCATION, PK_LOCATION, lKey) then begin
        if MessageDlg(ResStr_DeleteBoundary, mtConfirmation ,[mbYes,mbNo],0) = mrNo then
          Exit
        else
          with FMapServerLink do
            FdmMap.DeleteBoundary(lStaticID, SheetMapSheetKey(SelectedRegion.SheetID));
      end; // if there is a location associated
      liResult := MapDeleteObject(FMapServerLink.MapHandle,
                                  SelectedRegion.SheetID, SelectedRegion.ObjectID);
      // ignore if object already deleted by another user.
      if liResult <> -33 then MapCheck(liResult);
      RefreshMap;
      RefreshLocationBoundaries;
      SelectedRegion := PolygonID(0,-1);
      eLocation.Text := '';

      // Force changes to be written to map dataset file.
      FMapServerLink.SaveChanges(BaseMapKey);
    end; // if sure that you want to delete
  end;
  AppSettings.UpdateMapWindowSelectors;
end;  // TfrmMap.DeletePolygon

{-------------------------------------------------------------------------------
  Private proc that sets the polygon drawing + import toolbutton and menu item checked state 
}
procedure TfrmMap.DepressPolygonDropdown(itfState: Boolean);
var
  i: Integer;
begin
  try
    for i := 0 to frmMain.tbContext.ButtonCount-1 do
      if frmMain.tbContext.Buttons[i].DropDownMenu = pmBoundary then
        frmMain.tbContext.Buttons[i].Down := actDraw.Checked;
    mnuMapAddBoundary.Checked := itfState;
  except
    on E:Exception do
      raise EMapBrowserError.Create(ResStr_DepressPolygonDropDownError, E);
  end; // try
end;  // TfrmMap.DepressPolygonDropdown

{-------------------------------------------------------------------------------
}
procedure TfrmMap.DetachSheetByFileName(const iFileName: String);
begin
  MapDetachSheet(FMapServerLink.MapHandle, FMapServerLink.SheetIDByFileName(iFileName));
end;  // TfrmMap.DetachSheetByFileName

{-------------------------------------------------------------------------------
}
procedure TfrmMap.DoShowHint(var HintStr: string; var CanShow: Boolean; var HintInfo:
    THintInfo);
begin
  if (HintInfo.HintControl = pnlMapPanel) then
  begin
    if HintStr = ResStr_OutsideSystem then
    begin
      CanShow := true;
      HintInfo.HideTimeout := -1;
    end else
    begin
      if FSpecificHintPos then
        HintInfo.HintPos := FHintPos
      else begin
        // centre the hint if no hint pos specified
        HintInfo.HintPos.X := pnlMapPanel.Left + pnlMapPanel.Width div 2 - 7;
        HintInfo.HintPos.Y := pnlMapPanel.Top + pnlMapPanel.Height div 2 - 7;
      end;
      HintInfo.HintPos   := ClientToScreen(HintInfo.HintPos);
      HintInfo.HintMaxWidth    := pnlMapPanel.Width div 3;
      HintInfo.HintWindowClass := THintPointer;
      CanShow := not ShownHint;
      // reset the hint position for the next hint
      FSpecificHintPos := false;
      ShownHint := true;
    end;
  end else
    // Show hint as normal
    inherited;
end;  // TfrmMap.DoShowHint

{-------------------------------------------------------------------------------
  An alternative to DrawPixel which is used to draw distribution points at the default point
      size
}
procedure TfrmMap.DrawBigPixel(Coords: TPoint; Color: Cardinal; iBrush: HDc);
begin
  SelectObject(FDevice, iBrush);
  Ellipse(FDevice, Coords.x - 2, Coords.y - 2, Coords.x + 2, Coords.y + 2);
end;  // TfrmMap.DrawBigPixel

{-------------------------------------------------------------------------------
  Draws distribution point as transparent square.
}
procedure TfrmMap.DrawBigSquare(const spatialRef: String; square: TGridSquare; graphics: TGPGraphics;
  brush: TGPSolidBrush; pen: TGPPen; gridExtents: MSExtent; paintedSquares, graySquares: TStringList);
var
  spatialRef10k: String;
  brushColour, penColour: Cardinal;
  polyPoints: Array[0..3] of TGPPoint;

  function GetGPPoint(latLong: TLatLong): TGPPoint;
  var
    eastNorth: TMapCoord;
    mapPoint: TPoint;
  begin
    eastNorth := LatLongToSpecificEN(latLong, DatasetSpatialSystem);
    mapPoint := MapToScreen(eastNorth.x, eastNorth.y, gridExtents);
    Result := MakePoint(mapPoint.x, mapPoint.y);
  end;

begin
  if paintedSquares.IndexOf(spatialRef) <> -1 then Exit;

  // Draw a gray 10k square over?
  if square.Precision < 10000 then begin
    spatialRef10k := spatialRef;
    ReduceGridPrecision(spatialRef10k, 10000, square.System);
    if graySquares.IndexOf(spatialRef10k) = -1 then begin
      graySquares.Add(spatialRef10k);
      // Change colour to some gray
      brush.GetColor(brushColour);
      brush.SetColor(MakeColor(50, 180, 180, 180));
      pen.GetColor(penColour);
      pen.SetColor(MakeColor(180, 180, 180));
      // Reuse method :-)
      DrawBigSquare(
          spatialRef10k, SpatialRefToGridSquare(spatialRef10k, square.System, -1),
          graphics, brush, pen, gridExtents, paintedSquares, graySquares);
      // Change back to original colour
      pen.SetColor(penColour);
      brush.SetColor(brushColour);
    end;
  end;

  paintedSquares.Add(spatialRef);

  // Need 4 points, converted to map's system. Go clockwise.
  polyPoints[0] := GetGPPoint(square.BottomLeft);
  polyPoints[1] := GetGPPoint(square.TopLeft);
  polyPoints[2] := GetGPPoint(square.TopRight);
  polyPoints[3] := GetGPPoint(square.BottomRight);
  graphics.FillPolygon(brush, PGPPoint(@polyPoints), 4);
  graphics.DrawPolygon(pen, PGPPoint(@polyPoints), 4);
end;

{-------------------------------------------------------------------------------
}
procedure TfrmMap.DrawDistributionPoints(AGridExtents: MSExtent; AItem: TDatasetItem;
  AGraySquares: TStringList);
var
  lPen, lSolidBrush, lHollowBrush, lCurrentBrush: HDc;
  lBotLeftCoord, lTopRightCoord, lCurrentCoord: MSCoord;
  lWidthOfCircle, lTopLeft, lBotRight, lPixelPoint: TPoint;
  lLatLong: TLatLong;
  lCursor: TCursor;
  i: Integer;
  lDSX, lDSY: Double;
  paintedSquares: TStringList;
  graphics: TGPGraphics;
  brush: TGPSolidBrush;
  pen: TGPPen;
  r, g, b: Integer;
begin
  lDSX := DistributionScaleX;
  lDSY := DistributionScaleY;
  ChangeMapUserExtents;
  lCursor := MapCursor;
  MapCursor := crHourglass;
  try
    { size of circle in screen pixels }
    lWidthOfCircle.x := Round(lDSX * CoordtoPixelFactor.x);
    lWidthOfCircle.y := Round(lDSY * CoordtoPixelFactor.y);

    lPen := CreatePen(PS_SOLID, 1, AItem.Colour);
    SelectObject(FDevice, lPen);
    lHollowBrush := GetStockObject(HOLLOW_BRUSH);
    lSolidBrush  := CreateSolidBrush(AItem.Colour);
    with AItem.DistributionPoints do begin
      // indicate menu options should be enabled
      if Count > 0 then FPointsDrawn := True;

      // Will help to remember NOT to keep painting squares if already done once.
      paintedSquares := TStringList.Create;

      // MakeColor wants separate RGB.
      r := AItem.Colour and $FF;
      g := (AItem.Colour and $FF00) shr 8;
      b := (AItem.Colour and $FF0000) shr 16;
      graphics := TGPGraphics.Create(FDevice);
      brush := TGPSolidBrush.Create(MakeColor(50, r, g, b));
      pen := TGPPen.Create(MakeColor(r, g, b));

      try
        for i := 0  to Count - 1 do
          if TMapDrawItem(Items[i]).Displayed then begin
            lLatLong := TMapDrawItem(Items[i]).LatLong;

            // Check for zero values
            if (lLatLong.Lat <> NULL_LATLONG) and (lLatLong.Long <> NULL_LATLONG) then begin
              // Find Easting and Northing for the particular map Projection
              lCurrentCoord := MSCoord(LatLongToSpecificEN(lLatLong, DatasetSpatialSystem));
              if TMapDrawItem(Items[i]).Date > AppSettings.CutOffDate then
                lCurrentBrush := lSolidBrush
              else
                lCurrentBrush := lHollowBrush;
              SelectObject(FDevice, lCurrentBrush);
              if DistributionScaleX = 1 then begin
                // Draw a transparent square if item has one.
                if AppSettings.GridRefsAsSquares and TMapDrawItem(Items[i]).HasGridSquare then
                  DrawBigSquare(
                      TMapDrawItem(Items[i]).SpatialRef,
                      TMapDrawItem(Items[i]).GridSquare,
                      graphics,
                      brush,
                      pen,
                      AGridExtents,
                      paintedSquares,
                      AGraySquares)
                else begin
                  // Draw as a single pixel
                  lPixelPoint := MapToScreen(lCurrentCoord.x, lCurrentCoord.y, AGridExtents);
                  DrawBigPixel(lPixelPoint, AItem.Colour, lSolidBrush);
                end
              end else begin
                { lTopLeftCoord / lBotRightCoord are the corners of the appropriate
                  bounding box within which the data points will fall }
                RoundCoords(lCurrentCoord, -1);
                GetCoBoundingBox(lBotLeftCoord, lTopRightCoord, lCurrentCoord, lDSX, lDSY);
                lTopLeft  := MapToScreen(lBotLeftCoord.x,  lTopRightCoord.y, AGridExtents);
                lBotRight := MapToScreen(lTopRightCoord.x, lBotLeftCoord.y,  AGridExtents);

                if (lBotRight.x > 0) and (lTopLeft.x < MapCanvasSize.x) and
                   (lBotRight.y > 0) and (lTopLeft.y < MapCanvasSize.y) then
                  Ellipse(FDevice, lTopLeft.x, lTopLeft.y, lBotRight.x, lBotRight.y);
              end; // of else draw as a region;
            end; // of checking for zero values
          end;
      finally
        pen.Free;
        brush.Free;
        graphics.Free;

        MapCursor := lCursor;
        DeleteObject(lHollowBrush);
        DeleteObject(lSolidBrush);
        DeleteObject(lPen);
        paintedSquares.Free;
      end;// try-finally
    end;
  finally
    MapCursor := lCursor;
  end;
end;  // TfrmMap.DrawDistributionPoints

{-------------------------------------------------------------------------------
  The draw polygon pop-up menu is only required to be active when the map is in Draw Polygon
      mode. 
}
procedure TfrmMap.EnableDrawMenu(iState: Boolean);
begin
  pmPolygonDrawing.AutoPopup        := iState;
  // Finish ... always False until we have enough points
  actFinishPolygonCurrent.Enabled   := False;
  mnuEditFinishPolygonAny.Enabled   := False;
  actFinishLineCurrent.Enabled      := False;
  mnuEditFinishLineAny.Enabled      := False;
  pmDrawingFinishLineAny.Enabled    := False;
  pmDrawingFinishPolygonAny.Enabled := False;
  actCancelDraw.Enabled             := iState;
end;  // TfrmMap.EnableDrawMenu

{-------------------------------------------------------------------------------
  Adjusts coordinates at the centre of the map.
}
procedure TfrmMap.EndPan;
var
  lOriginMove: msCoord;
begin
  lOriginMove.x := MouseDownMapPos.x - MouseUpMapPos.x;
  lOriginMove.y := MouseDownMapPos.y - MouseUpMapPos.y;
  MapCheck(MapSetViewOrigin(FMapServerLink.MapHandle, FOriginStartPan.x + lOriginMove.X,
                                                      FOriginStartPan.y + lOriginMove.y));
end;  // TfrmMap.EndPan

{-------------------------------------------------------------------------------
  When in select area mode, Clears the drag rectangle, draws a fake and populates the bounding
      box property 
}
procedure TfrmMap.EndSelectArea;
var
  lTopLeft, lBotRight: LongInt;
  lExtent: msExtent;
  SouthWestEN, NorthEastEN, TempEN: TMapCoord;
begin
  // Finish the selection dragging process
  MapCheck(MapDragRectEnd(FMapServerLink.MapHandle, lTopLeft, lBotRight,
      lExtent));

  try
    MouseUpPixPos :=
        NearestPointInReferenceSystem(MouseUpPixPos.X, MouseUpPixPos.Y);

    { End the MapServer drag rectangle only if the drag has been > 10 pixels }
    if (Abs(MouseDownPixPos.x - MouseUpPixPos.x) > 10) and
       (Abs(MouseDownPixPos.y - MouseUpPixPos.y) > 10) then
    begin
      case FBoundingBoxResizeDragElement of
        NoSideOrCorner:
          begin
            SouthWestEN := TMapCoord(ScreenToMap(FMouseDownPixPos.X,
                FMouseDownPixPos.Y));
            NorthEastEN := TMapCoord(ScreenToMap(FMouseUpPixPos.X,
                FMouseUpPixPos.Y));

            // If a new bounding box was drawn, the drag must have STARTED
            // within the system limits, too.
            MouseDownPixPos := NearestPointInReferenceSystem(MouseDownPixPos.X,
                MouseDownPixPos.Y);
          end;
        BottomLeftCorner:
          begin
            SouthWestEN := TMapCoord(ScreenToMap(FMouseUpPixPos.X,
                FMouseUpPixPos.Y));
            NorthEastEN := LatLongToSpecificEN(BoundingBox.NorthEast,
                DatasetSpatialSystem);
          end;
        TopLeftCorner:
          begin
            SouthWestEN.Y := LatLongToSpecificEN(BoundingBox.SouthWest,
                DatasetSpatialSystem).y;
            NorthEastEN.X := LatLongToSpecificEN(BoundingBox.NorthEast,
                DatasetSpatialSystem).x;
            SouthWestEN.X := TMapCoord(ScreenToMap(FMouseUpPixPos.X,
                FMouseUpPixPos.Y)).X;
            NorthEastEN.Y := TMapCoord(ScreenToMap(FMouseUpPixPos.X,
               FMouseUpPixPos.Y)).Y;
          end;
        TopRightCorner:
          begin
            NorthEastEN := TMapCoord(ScreenToMap(FMouseUpPixPos.X,
                FMouseUpPixPos.Y));
            SouthWestEN := LatLongToSpecificEN(BoundingBox.SouthWest,
                DatasetSpatialSystem);
          end;
        BottomRightCorner:
          begin
            SouthWestEN.X := LatLongToSpecificEN(BoundingBox.SouthWest,
                DatasetSpatialSystem).x;
            NorthEastEN.Y := LatLongToSpecificEN(BoundingBox.NorthEast,
                DatasetSpatialSystem).y;
            SouthWestEN.Y := TMapCoord(ScreenToMap(FMouseUpPixPos.X,
                FMouseUpPixPos.Y)).Y;
            NorthEastEN.X := TMapCoord(ScreenToMap(FMouseUpPixPos.X,
                FMouseUpPixPos.Y)).X;
          end;
        LeftSide:
          begin
            NorthEastEN := LatLongToSpecificEN(BoundingBox.NorthEast,
                DatasetSpatialSystem);
            SouthWestEN.Y := LatLongToSpecificEN(BoundingBox.SouthWest,
                DatasetSpatialSystem).Y;
            SouthWestEN.X := TMapCoord(ScreenToMap(FMouseUpPixPos.X,
                FMouseUpPixPos.Y)).X;
          end;
        TopSide:
          begin
            SouthWestEN := LatLongToSpecificEN(BoundingBox.SouthWest,
                DatasetSpatialSystem);
            NorthEastEN.X := LatLongToSpecificEN(BoundingBox.NorthEast,
                DatasetSpatialSystem).X;
            NorthEastEN.Y := TMapCoord(ScreenToMap(FMouseUpPixPos.X,
                FMouseUpPixPos.Y)).Y;
          end;
        RightSide:
          begin
            SouthWestEN := LatLongToSpecificEN(BoundingBox.SouthWest,
                DatasetSpatialSystem);
            NorthEastEN.Y := LatLongToSpecificEN(BoundingBox.NorthEast,
                DatasetSpatialSystem).Y;
            NorthEastEN.X := TMapCoord(ScreenToMap(FMouseUpPixPos.X,
                FMouseUpPixPos.Y)).X;
          end;
        BottomSide:
          begin
            NorthEastEN := LatLongToSpecificEN(BoundingBox.NorthEast,
                DatasetSpatialSystem);
            SouthWestEN.X := LatLongToSpecificEN(BoundingBox.SouthWest,
                DatasetSpatialSystem).X;
            SouthWestEN.Y := TMapCoord(ScreenToMap(FMouseUpPixPos.X,
                FMouseUpPixPos.Y)).Y;
          end;
      end;

      with BoundingBox do
      begin
        { Convert start and end points to TLatLongs. Use "SpecificENToLatLong"
        to take account of the dataset base sheet type. }

        TempEN.X := Max(NorthEastEN.X, SouthWestEN.X);
        TempEN.Y := Max(NorthEastEN.Y, SouthWestEN.Y);

        NorthEast := SpecificENToLatLong(TempEN,
            DatasetSpatialSystem);

        TempEN.X := Min(NorthEastEN.X, SouthWestEN.X);
        TempEN.Y := Min(NorthEastEN.Y, SouthWestEN.Y);

        SouthWest := SpecificENToLatLong(TempEN,
            DatasetSpatialSystem);
      end;
      { Replace the DragRect with a rect drawn onto the Canvas of the MapUser sheet }
      DrawBoundingBox;
    end;
  except
    raise EMapBrowserError.CreateNonCritical(ResStr_BBOutsideSystem);
  end;

  // Indicates that resizing is no longer in progress.
  FBoundingBoxResizeDragElement := NoSideOrCorner;

  // If a side was dragged to resize the selection, the cursor may not still be
  // over the rectangle when the mouse button is released, so update it.
  UpdateAreaSelectionToolCursor(GetPossibleBoundingBoxResizeDragElement(
      MouseUpPixPos));

  InvalidateRect(FMapServerLink.MapHandle, nil, True);
end;  // TfrmMap.EndSelectArea

{-------------------------------------------------------------------------------
  To rescale the map.  Ends the drag rectangle. Determines the new scale and new central
      coordinates at which to display the map.  if movement has been less than 10 pixels then
      the map scale is modified by a factor or two otherwise scale is modified according to
      the size of the bounding box.  In both cases the centre of the map is moved according to
      the centre of the zoom clicking.
  Side Effects: WMU_MAPUSERDRAW triggered which causes the map to refresh
}
procedure TfrmMap.EndZoom;
var
  lTopLeft: LongInt;
  lBotRight: LongInt;
  lExtent: msExtent;
  lDifference: TPoint;
  lRatio: msCoord;
  lCoCentre: msCoord;
  lCoord: msCoord;
  lMapOrigin: msCoord;
  lNewOrigin: TPoint;
begin
  lMapOrigin.X := -VisibleExtents.dWest;
  lMapOrigin.Y := -VisibleExtents.dSouth;
  
  lTopLeft  := MakeLong(Max(FMouseDownPixPos.x, 0), Max(FMouseDownPixPos.y, 0));
  lBotRight := MakeLong(Max(MouseUpPixPos.x, 0), Max(MouseUpPixPos.y, 0));
  MapCheck(MapDragRectEnd(FMapServerLink.MapHandle, lTopLeft, lBotRight, lExtent));
  
  lDifference.x := MouseUpPixPos.x - FMouseDownPixPos.x;
  lDifference.y := MouseUpPixPos.y - FMouseDownPixPos.y;

  { Assume a rect has been drawn if the box is > 10 pixels in both dimensions }
  if (Abs(lDifference.x) > 10) and (Abs(lDifference.y) > 10) then begin
    { Determine the neew scale according to the rectangle }
    lRatio.x := Abs(lDifference.x / MapCanvasSize.x);
    lRatio.y := Abs(lDifference.y / MapCanvasSize.y);
  
    { Determine the centre of the bounding box for centre of the new view }
    if MouseUpPixPos.x < MouseDownPixPos.x then
      lNewOrigin.X := MouseUpPixPos.x + Abs(lDifference.x div 2)
    else
      lNewOrigin.x := MouseDownPixPos.x + Abs(lDifference.x div 2);
  
    if MouseUpPixPos.y < MouseDownPixPos.y then
      lNewOrigin.y := MouseUpPixPos.y + Abs(lDifference.y div 2)
    else
      lNewOrigin.y := MouseDownPixPos.y + Abs(lDifference.y div 2);
  
    { Convert this to a real MapServer coordinate }
    lCoord := ScreenToMap(lNewOrigin.x, lNewOrigin.y);
  
    { SCALE }
    if Tool = TOOL_ZOOM_IN then begin
      if lRatio.x > lRatio.y then
      { zoom according to the x-coord }
        MapScale := Abs(FMapScale * (lDifference.x / MapCanvasSize.x))
      else { zoom to y-coord }
        MapScale := Abs(FMapScale * (lDifference.y / MapCanvasSize.y));
    end; // if Tool := TOOL_ZOOM_IN
  
    if Tool = TOOL_ZOOM_OUT then begin
      if lRatio.x > lRatio.y then
      { zoom according to the x-coord }
        MapScale := Abs(FMapScale * (MapCanvasSize.x / lDifference.x))
      else { zoom to y-coord }
        MapScale := Abs(FMapScale * (MapCanvasSize.y / lDifference.y));
    end; // if Tool := TOOL_ZOOM_OUT
  
    MapCheck(MapSetViewOrigin (FMapServerLink.MapHandle, lCoord.x, lCoord.y));
  end // of if rectangle is significant
  else begin
  { if not significant movement, then double the scale }
    lCoCentre := ScreenToMap(Round(MouseDownPixPos.x), Round(MouseDownPixPos.y));
    if Tool = TOOL_ZOOM_IN then MapScale := FMapScale / 2.0
                           else MapScale := FMapScale * 2.0;

    MapCheck(MapSetViewOrigin(FMapServerLink.MapHandle, lCoCentre.x, lCoCentre.y));
  end; // of if clicked
  RefreshMap;
end;  // TfrmMap.EndZoom

{-------------------------------------------------------------------------------
  On dropping a survey event onto the map, this calls methods to create an FEventList and an
      FSampleList of Event Keys dropped and of the samples contained within those events 
  Dependencies: FMapHandle, FdmMap must exist
  Special Notes: Triggers a WMU_MAPUSER message and redraws the map. 
}
procedure TfrmMap.EventDropped(ASourceData: TKeyList);
var
  i: Integer;
  lKeys: String;
  lRS: _Recordset;
begin
  lKeys := '''' + ASourceData.Items[0].KeyField1 + '''';
  for i := 1 to ASourceData.Header.ItemCount - 1 do
    lKeys := lKeys + ',''' + ASourceData.Items[i].KeyField1 + '''';

  // Asks whether want the event or the samples displayed
  with TdlgSurveyDataOptions.Create(nil) do
    try
      rgOptions.Items.Strings[0] := ResStr_SurveyEvent;
      if ShowModal = mrOK then
        case rgOptions.ItemIndex of
          0: begin // Plot survey event
               lRS := dmDatabase.ExecuteSQL('SELECT * FROM Survey_Event WHERE Spatial_Ref <> '''' ' +
                                            'AND Survey_Event_Key IN (' + lKeys + ')', True);
               if CreateDistributionList(nil, lRS, nil, 'Events', TN_SURVEY_EVENT, PK_SURVEY_EVENT) = nil then
                 MessageDlg(ResStr_NoRecordings, mtInformation, [mbOk], 0);
             end;
          1: begin //plot samples
               lRS := dmDatabase.ExecuteSQL('SELECT * FROM Sample WHERE Spatial_Ref <> '''' ' +
                                            'AND Survey_Event_Key IN (' + lKeys + ')', True);
               if CreateDistributionList(nil, lRS, nil, 'Samples', TN_SAMPLE, PK_SAMPLE) = nil then
                 MessageDlg(ResStr_NoRecordings, mtInformation, [mbOk], 0);
             end;
        end;
    finally
      Free;
    end;
end;  // TfrmMap.EventDropped 

{-------------------------------------------------------------------------------
  Using the #REPORT_OUTPUT table, determine the list of points to plot on the
       map from the Report Wizard output.
}
procedure TfrmMap.FilterDropped(AConnection: TADOConnection);
var
  lCursor: TCursor;
  lCurrentType: string;
  lDataset: TDatasetItem;
  lFields: TStringList;
  i: integer;
  lDrawItem: TMapDrawItem;
  lKeyField: string;
  lRecordset: _Recordset;

    // Return the description of a dataset
    function Description: string;
    begin
      Result := ResStr_Local_ReportOutput;
      if CompareText(lDataset.TableName, TN_TAXON_OCCURRENCE) = 0 then
        Result := Result + ResStr_Local_Taxa
      else if CompareText(lDataset.TableName, TN_BIOTOPE_OCCURRENCE) = 0 then
        Result := Result + ResStr_Local_Biotope;
    end;

    // create a new dataset, and loads the old one if there already was one
    procedure CreateDataset;
    begin
      if Assigned(lDataset) then
        AddDatasetItem(lDataset, Description);
      lDataset := TDatasetItem.Create;
      if (lFields.IndexOf('Type')>-1) and (lFields.IndexOf(PK_OCCURRENCE)>-1) then
        lKeyField := PK_OCCURRENCE
      else if lFields.IndexOf(PK_SAMPLE)>-1 then
        lKeyField := PK_SAMPLE
      else if lFields.IndexOf(PK_LOCATION)>-1 then
        lKeyField := PK_LOCATION;
    end;

    // checks that a dataset exists, also creates new one if switching between
    // biotope and taxon records
    procedure CheckDataset;
    begin
      if (lFields.IndexOf('Type')>-1) and
          (lFields.IndexOf(PK_OCCURRENCE)>-1) then begin
        if VarToStr(lRecordset.Fields['Type'].Value)<>lCurrentType then begin
          lCurrentType := VarToStr(lRecordset.Fields['Type'].Value);
          CreateDataset;
          if CompareText(VarToStr(lRecordset.Fields['Type'].Value), 'T')=0 then
            lDataset.TableName := TN_TAXON_OCCURRENCE
          else
            lDataset.TableName := TN_BIOTOPE_OCCURRENCE;
        end;
      end;
      if not Assigned(lDataset) then begin
        CreateDataset;
        if lFields.IndexOf(PK_SAMPLE)>-1 then
          lDataset.TableName := TN_SAMPLE
        else if lFields.IndexOf(PK_LOCATION)>-1 then
          lDataset.TableName := TN_LOCATION
        else
          lDataset.TableName := ''; // no known table - cannot find data
      end;
    end;

begin
  lCursor := MapCursor;
  MapCursor := crHourGlass;
  frmMain.SetStatus(ResStr_LoadingDataset);
  lFields := TStringList.Create;
  lFields.Sorted := True;
  try
    lCurrentType := '';
    lDataset := nil;
    lRecordset := AConnection.Execute('EXEC usp_SpatialRefsForReport_Select');
    if Assigned(lRecordset) then
      if lRecordset.State>0 then
        with lRecordset do begin
          // Prepare a list of fields so we can keep checking them
          for i := 0 to Fields.Count-1 do
            lFields.Add(Fields[i].Name);
          while not EOF do begin
            CheckDataset;
            lDrawItem := TMapDrawItem.Create;
            lDrawItem.InitFromRecord(Fields, lKeyField);
            lDrawItem.Displayed := PointDisplayedStatus(lDrawItem.SpatialRef, lDrawItem.SpatialSys);
            lDataset.DistributionPoints.Add(lDrawItem);
            MoveNext;
            frmMain.SetProgress(Integer(AbsolutePosition) * 100 div RecordCount);
          end;
          AddDatasetItem(lDataset, Description);
        end
      else
        ShowInformation(ResStr_NoDataToPlot);
  finally
    lFields.Free;
    RefreshMap;
    MapCursor := lCursor;
    frmMain.SetProgress(0);
    frmMain.SetStatus('');
  end; // try finally
end;  // TfrmMap.FilterDropped

{-------------------------------------------------------------------------------
  Given a point on the map, this creates a bounding box around it. Any points in that bounding
      box are considered clicked.
}
procedure TfrmMap.FindLatLongBoundingBox(const iPosition: TMapCursorPos; out oNELatLong,
    oSWLatLong: TLatLong);
var
  lMapCoord, lNEMapCoord, lSWMapCoord: msCoord;
  i, j: Integer;
  lItem: TMapDrawItem;
  lRgn: HRgn;
  lMapPoint: TPoint;
begin
  lMapCoord.X := iPosition.MapPos.X;
  lMapCoord.Y := iPosition.MapPos.Y;
  //Find bounding box
  if DistributionScaleX <> 1 then begin
    GetCoBoundingBox(lSWMapCoord, lNEMapCoord, lMapCoord, DistributionScaleX, DistributionScaleY);

    //Reuse lMapCoord to obtain SW coord
    lMapCoord.X := lSWMapCoord.X;
    lMapCoord.Y := lSWMapCoord.Y;
    oSWLatLong := SpecificENToLatLong(TMapCoord(lMapCoord), DatasetSpatialSystem);

    //Reuse lMapCoord to obtain NE coord
    lMapCoord.X := lNEMapCoord.X;
    lMapCoord.Y := lNEMapCoord.Y;
    oNELatLong := SpecificENToLatLong(TMapCoord(lMapCoord), DatasetSpatialSystem);
  end else begin
    //Bounding box is a circle round the point
    //Find the NorthEast
    lNEMapCoord.X := lMapCoord.X + 2 * PixelToCoordFactor.X;
    lNEMapCoord.Y := lMapCoord.Y + 2 * PixelToCoordFactor.Y;

    //Find the SouthWest
    lSWMapCoord.X := lMapCoord.X - 2 * PixelToCoordFactor.X;
    lSWMapCoord.Y := lMapCoord.Y - 2 * PixelToCoordFactor.Y;

    // Use LatLong from now on.
    oNELatLong := SpecificENToLatLong(MapCoord(lNEMapCoord.x, lNEMapCoord.y), DatasetSpatialSystem);
    oSWLatLong := SpecificENToLatLong(MapCoord(lSWMapCoord.x, lSWMapCoord.y), DatasetSpatialSystem);

    // Now expand the bounding box to include everything that overlap that point.
    lMapPoint := Point(Round(lMapCoord.x), Round(lMapCoord.y));
    for i := 0 to DatasetLegend.Count - 1 do
      if DatasetLegend[i].Visible then
        with DatasetLegend[i].DistributionPoints do
          for j := 0 to Count - 1 do begin
            lItem := TMapDrawItem(Items[j]);
            if lItem.HasGridSquare then begin
              lRgn := GridSquareToRegion(lItem.GridSquare, DatasetSpatialSystem);
              try
                if PtInRegion(lRgn, lMapPoint.x, lMapPoint.y) then begin
                  oSWLatLong.Lat  := Min(oSWLatLong.Lat , lItem.GridSquare.BottomLeft.Lat);
                  oSWLatLong.Long := Min(oSWLatLong.Long, lItem.GridSquare.BottomLeft.Long);
                  oNELatLong.Lat  := Max(oNELatLong.Lat , lItem.GridSquare.TopRight.Lat);
                  oNELatLong.Long := Max(oNELatLong.Long, lItem.GridSquare.TopRight.Long);
                end;
              finally
                DeleteObject(lRgn);
              end;
            end;
          end;
  end;
end;  // TfrmMap.FindLatLongBoundingBox

{-------------------------------------------------------------------------------
  Given the list of possible objects in the FMultiLocations array, this function cycles 
      through them in order so that clicking on overlapping object cycles through them.
}
function TfrmMap.FindNextFArrayObject: TPolygonID;
var
  lCount: Integer;
  lListItems: Integer;
begin
  lListItems := High(FMultiLocations) + 1;
  { Go to beginning of array }
  { Find the position of the selected object }
  for lCount := 0 to lListItems -1 do
    if (FMultiLocations[lCount].SheetId = SelectedRegion.SheetID) and
       (FMultiLocations[lCount].ObjectID = SelectedRegion.ObjectID) then
      Break;
  { lCount now holds the index of the selected item }
  if lCount >= lListItems -1 then
    lCount := 0
  else
    Inc(lCount);
  Result := FMultiLocations[lCount];
end;  // TfrmMap.FindNextFArrayObject

{-------------------------------------------------------------------------------
  This function determines if gridlines can be drawn on the map and disables or enables menu
      items and toolbar buttons accordingly. It also finds out what the ratio
      MapCoordinates:Metres is and puts it in the field FMapCoordsPerMetre.
  Called from DoCreateStuff
}
procedure TfrmMap.FindSettingsBySRSystem;
var
  lCanDisplayGrids: Boolean;
  i: Integer;
begin
  if CompareText(DatasetSpatialSystem, LAT_LONG) = 0 then begin
    lCanDisplayGrids := False;
    FMapCoordsPerMetre := 1; {Though is unusable}
  end else
  if CompareText(DatasetSpatialSystem, UTM) = 0 then begin
    lCanDisplayGrids := False;
    FMapCoordsPerMetre := 1; {though unusable}
  end else
  if CompareText(DatasetSpatialSystem, OS_GB) = 0 then begin
    lCanDisplayGrids := True;
    FMapCoordsPerMetre := 1;
  end else
  if CompareText(DatasetSpatialSystem, OS_NI) = 0 then begin
    lCanDisplayGrids := True;
    FMapCoordsPerMetre := 1;
  end else
  if GetComBaseMapSystem then begin
    lCanDisplayGrids   := FComMapFormat.CanDisplayGrids;
    FMapCoordsPerMetre := FComMapFormat.MapCoordsPerMetre;
  end else
    raise EMapBrowserError.CreateNonCritical(Format(ResStr_UnrecognisedBaseMapSystem, [DatasetSpatialSystem]));

  mnuMapDistributionPoints.Enabled := lCanDisplayGrids;
  mnuMapGridLines.Enabled          := lCanDisplayGrids;
  // Keep buttons in synch.
  with frmMain.tbContext do
    for i := 0 to ButtonCount - 1 do begin
      if Buttons[i].DropDownMenu = pmDistributionPoints then
        Buttons[i].Enabled := lCanDisplayGrids;
      if Buttons[i].DropDownMenu = pmGridLines then
        Buttons[i].Enabled := lCanDisplayGrids;
    end;
end;  // TfrmMap.FindSettingsBySRSystem

{-------------------------------------------------------------------------------
  When using the Find Source Data tool, locates all data items displayed under the click, and
      displays a dialog allowing the user to select which bit of data to find in the
      observations hierarchy
}
procedure TfrmMap.FindSourceDataUnderCursor(const iPosition: TMapCursorPos);
var
  lEventKeys: TStringList;
  lSampleKeys: TStringList;
  lResults: TObjectList;
  lNELatLong, lSWLatLong: TLatLong;
  lKeys: TEditableKeyList;
  lSelectedPoint: TPointOnMap;
  lfrmObs: TfrmObservations;
begin
  MapCursor := crHourglass;
  lEventKeys := TStringList.Create;
  lSampleKeys := TStringList.Create;
  lResults := TObjectList.Create;
  lSelectedPoint := nil;
  try
    //Find out what lat Long we have to
    //search for and in what range.
    FindLatLongBoundingBox(iPosition, lNELatLong, lSWLatLong);

    //Search database for survey events and samples
    GetDataWithinBoundingBox(lNELatLong, lSWLatLong, lEventKeys, lSampleKeys, iPosition.MapPos);
    MakeTaxonAndBiotopeStrings(lEventKeys, lSampleKeys, lResults);
    case lResults.Count of
      0: raise EMapBrowserError.CreateNonCritical(ResStr_NoDistributionPoints);
      1: lSelectedPoint := TPointOnMap(lResults[0]);
    else  // >1 observations - display selection list
        with TdlgSelectDistributionPoint.Create(nil) do
          try
            PopulateGrid(lResults);
            if ShowModal = mrOK then
              lSelectedPoint := TPointOnMap(lResults[sgPoints.Row - sgPoints.FixedRows]);
          finally
            Free;
          end;
    end; // case

    if Assigned(lSelectedPoint) then begin
      // Get Obs form up BEFORE applying filter, so we don't get the message about data on other forms.
      lfrmObs := TFrmObservations(dmFormActions.DisplayForm(TfrmObservations));
      lKeys := TEditableKeyList.Create;
      try
        lKeys.SetTable(lSelectedPoint.DataTable);
        lKeys.AddItem(lSelectedPoint.Key, lSelectedPoint.DataTable);
        with TListFilter.Create do
          try
            LoadFilter(lKeys);
            Applyfilter;
          finally
            Free;
          end;
      finally
        lKeys.Free;
      end;
      lfrmObs.SelectNode(lSelectedPoint.DataTable, lSelectedPoint.Key);
    end;
  finally
    lResults.Free;
    lEventKeys.Free;
    lSampleKeys.Free;
    MapCursor := crFindSourceData;
  end;  //try..finally
end;  // TfrmMap.FindSourceDataUnderCursor

{-------------------------------------------------------------------------------
}
procedure TfrmMap.FinishLineClick(Sender: TObject);
begin
  FinishLineOntoSheet(FLayerLegend[TMenuItem(Sender).Tag]);
end;  // TfrmMap.FinishLineClick 

{-------------------------------------------------------------------------------
}
procedure TfrmMap.FinishLineOntoSheet(ALayer: TObject);
var
  lNumCoords: Integer;
begin
  if ALayer is TPolygonLayerItem then begin
    if not FFirstCoord then begin
      MapCheck(MapEditGetNumCoords(FMapServerLink.MapHandle, lNumCoords));
      if lNumCoords < 2 then
        MessageDlg(ResStr_CannotCompLine, mtInformation, [mbOK], 0)
      else
        FinishObjectOntoSheet(ALayer, MS_LINE_OBJECT);
    end;
  end else
    raise EMapBrowserError.Create(ResStr_CannotAddLine);
end;  // TfrmMap.FinishLineOntoSheet

{-------------------------------------------------------------------------------
}
procedure TfrmMap.FinishObjectOntoSheet(ALayer: TObject; AType: Byte);
var
  lSheetID: Integer;
  lLayer: TPolygonLayerItem;
begin
  if ALayer is TPolygonLayerItem then begin
    lLayer := TPolygonLayerItem(ALayer);

    lSheetID := FMapServerLink.SheetIDByFileName(lLayer.FileName);
    // Set object to correct display format, and place onto sheet
    MapCheck(MapEditSetSheet(FMapServerLink.MapHandle, lSheetID));
    MapCheck(MapEditSetID(FMapServerLink.MapHandle, FMapServerLink.ObjectTotal[lSheetID]));
    SetMapObjectDetails(AType, True, lLayer.UnselectedColour,
                        lLayer.MSPattern, False, nil, nil);
    FFirstCoord := True;
    EnableDrawMenu(False);
    lLayer.LastNumObjects := lLayer.LastNumObjects + 1;
    AppSettings.UpdateMapWindowSelectors;

    // Force line to be written to map dataset file.
    FMapServerLink.SaveChanges(BaseMapKey);
  end;
end;  // TfrmMap.FinishObjectOntoSheet

{-------------------------------------------------------------------------------
}
procedure TfrmMap.FinishPolygonClick(Sender: TObject);
begin
  FinishPolygonOntoSheet(FLayerLegend[TMenuItem(Sender).Tag]);
end;  // TfrmMap.FinishPolygonClick

{-------------------------------------------------------------------------------
}
procedure TfrmMap.FinishPolygonOntoSheet(ALayer: TObject);
var
  lNumCoords: Integer;
begin
  if ALayer is TPolygonLayerItem then begin
    if not FFirstCoord then begin
      MapCheck(MapEditGetNumCoords(FMapServerLink.MapHandle, lNumCoords));
      if lNumCoords < 3 then
        MessageDlg(ResStr_PolygonCoordinates, mtInformation, [mbOK], 0)
      else
        FinishObjectOntoSheet(ALayer, MS_REGION_OBJECT);
    end;
  end else
    raise EMapBrowserError.Create(ResStr_CannotAddPolygon);
end;  // TfrmMap.FinishPolygonOntoSheet

{-------------------------------------------------------------------------------
  Sets up the tool bar
  Calls the CheckLastLocationBoundary method which queries the Location_Boundary table to check whether the last boundary has a location associated, and if so writes the Key_Value of that boundary to the map's internal database. 
}
procedure TfrmMap.FormActivate(Sender: TObject);
var
  lGridScaleX, lGridScaleY: Double;
  lGridOffsetX, lGridOffsetY: Double;
  lDistScaleX, lDistScaleY: Double;
  lGridState: Boolean;
begin
  inherited;
  if IsBoundaryImportInProgress then
  begin
    WindowState := wsMinimized;
  end
  else
  begin
    if not (csDestroying in ComponentState) then // safety
      if not FActivated then begin
        with frmMain do begin
          dmFormActions.actPrint.Enabled := True;
          ClearContextToolbar(True);
          { Set up toolbar with ReturnData tool button }
          SetContextToolbar(Self, mnuEdit, 0, 0, [nil]);
          { Add the Copy to Clipboard tool button }
          SetContextToolbar(Self, mnuEdit, 8, 8, [nil]);
          { Add separator }
          SetContextToolbar(Self, mnuEdit, 1, 1, [nil]);
          { Set up the tool bar with the menu items and popup menus }
          SetContextToolbar(Self, mnuChildMap, 4,
                            [nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, pmBoundary, nil, nil,
                             nil, nil, pmPolygonToLayer, nil, nil, nil, nil, nil, nil, nil, nil, nil,
                             pmDistributionPoints, pmGridLines, nil, pmSampleTypes, nil]);
          FInitialisedToolbar := True; // flag to inform other parts of map
          SetTool(Tool);
          // Enable save functionality in main menu
          mnuFileSave.Enabled   := True;
          mnuFileSaveAs.Enabled := True;
        end;  // with frmMain

        if FMapCursor = 0 then
          FMapCursor := crArrow;
        dmFormActions.actOccurrencesForPlacesReport.Enabled := (FGridReference <> '') and
                                              (ObjectDetails.ObjectLocationKey<>'')
                                              and (not ObjectDetails.IsAdminArea);
        dmFormActions.actPlacesForOccurrencesReport.Enabled := (FGridReference <> '') and
                                              (ObjectDetails.ObjectLocationKey<>'')
                                              and (not ObjectDetails.IsAdminArea);
        FActivated := True;
      end;  //if not FActivated

    // This is to fix an intriguing, and rather upsetting problem that sprang up between
    // two builds. And obviously it shouldn't have.
    // Dropping and re-creating the user sheet is harmless and fixes the problem.
    // It IS a hack but it works. :-)
    // Update: Fix was thought to be harmless, but it actually buggered the display of the grid!
    lGridState := FGridActive;
    lGridScaleX := GridLineScaleX;
    lGridScaleY := GridLineScaleY;
    lGridOffsetX := GridOffsetX;
    lGridOffsetY := GridOffsetY;
    lDistScaleX := DistributionScaleX;
    lDistScaleY := DistributionScaleY;
    CreateMapUserSheet;  // Calls DeleteMapUserSheets too.
    // Restore grid and distriution to what they were before dropping user sheet.
    FGridActive       := lGridState;
    GridLineScaleX    := lGridScaleX;
    GridLineScaleY    := lGridScaleY;
    GridOffsetX       := lGridOffsetX;
    GridOffsetY       := lGridOffsetY;
    DistributionScaleX := lDistScaleX;
    DistributionScaleY := lDistScaleY;
  end;
end;  // TfrmMap.FormActivate

{-------------------------------------------------------------------------------
  Retuns the scale as an easily read formatted string... ie. pass in 5403221, returns 
      '1 : 5,403,221'
}
function TfrmMap.FormatScale(AScale: double): String;
var
  lCount: Integer;
  lLength: Integer;
  lScale: String;
  lFirst, lSecond: String;
  lFormatSettings: TFormatSettings;
  lSep: Char;
begin
  GetLocaleFormatSettings(LOCALE_USER_DEFAULT, lFormatSettings);
  if lFormatSettings.DecimalSeparator = '.' then lSep := ',' else lSep := ' ';
  lScale := IntToStr(Round(AScale));
  lLength := Length(lScale);
  if lLength > 3 then begin
    lCount := 3;
    lFirst := Copy(lScale, 1, lLength - lCount);
    lSecond:= Copy(lScale, (lLength - lCount + 1), lLength);
    while Length(lFirst) > 0 do begin
      lScale  := lFirst + lSep + lSecond;
      lLength := length(lScale);
      lCount  := lCOunt + 4;
      lFirst  := Copy(lScale, 1, lLength - lCount);
      lSecond := Copy(lScale, (lLength - lCount + 1), lLength);
    end;
  end; // if length > 3
  Result := lScale;
end;  // TfrmMap.FormatScale 

{-------------------------------------------------------------------------------
}
procedure TfrmMap.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  inherited;
  { Reactivate the Configure menu option }
  dmFormActions.actPrint.Enabled:=False;
  Action := caFree;
  // Disable save items in main menu
  frmMain.mnuFileSave.Enabled    := False;
  frmMain.mnuFileSaveAs.Enabled  := False;
end;  // TfrmMap.FormClose

{-------------------------------------------------------------------------------
  if the map is open, then it is used to remove the MapUser sheet from the dataset and delete
      the file that was created and also repaints the selected object in red again.
}
procedure TfrmMap.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if IsBoundaryImportInProgress then
  begin
    ShowInformation(ResStr_BoundaryImportInProgress);
    CanClose := False;
  end
  else
  begin
    if FMapWindowOpen then // FMapHandle <> 0 then
      try
        DeleteMapUserSheets;
      except
        on EMapBrowserError do
        CanClose := True;
      end;

    CanClose := True;
  end;
end;  // TfrmMap.FormCloseQuery

{-------------------------------------------------------------------------------
}
procedure TfrmMap.FormDeactivate(Sender: TObject);
begin
  dmFormActions.actprint.Enabled := False;
  frmMain.mnuFileSave.Enabled    := False;
  frmMain.mnuFileSaveAs.Enabled  := False;
  { Note here we access panel text directly - we mustn't trigger a processmessages
     during the deactivate procedure otherwise things go wrong! }
  frmMain.Status.Panels[0].Text:='';
  FActivated := False;
  inherited;
end;  // TfrmMap.FormDeactivate 

{-------------------------------------------------------------------------------
  Make sure that the legend fits nicely into the map 
}
procedure TfrmMap.FormResize(Sender: TObject);
begin
  inherited;
  if not (csDestroying in ComponentState) then begin
    pnlDockableLegend.Height := pnlDockSiteleft.Height;
    if Assigned(FDatasetLegend) then FDatasetLegend.CheckColumnWidths;
    if Assigned(FLayerLegend) then FLayerLegend.CheckColumnWidths;
    pnlDrag.SetBounds(2, 2, pnlBottom.Width - 4, pnlBottom.Height - 4);
  end;
end;  // TfrmMap.FormResize

{-------------------------------------------------------------------------------
  Work out the bounding box according to a distribution scale for a given point
}
procedure TfrmMap.GetCoBoundingBox(var oCoBotLeft, oCoTopRight: msCoord; iCoCurrentPoint:
    msCoord; iDistScaleX, iDistScaleY: double);
begin
  // Get Bottom-Left corner. Use RoundTo to avoid issues with doubles storing ints as values
  // that are just below the actual int, so truncate to the lower number.
  oCoBotLeft.x := ((Trunc(RoundTo(iCoCurrentPoint.x / (iDistScaleX * FMapCoordsPerMetre), -8))) *
                   iDistScaleX * FMapCoordsPerMetre);
  oCoBotLeft.y := ((Trunc(RoundTo(iCoCurrentPoint.y / (iDistScaleY * FMapCoordsPerMetre), -8))) *
                   iDistScaleY * FMapCoordsPerMetre);
  // Shift to get Top-Right corner.
  oCoTopRight.x := oCoBotLeft.x + iDistScaleX * FMapCoordsPerMetre;
  oCoTopRight.y := oCoBotLeft.y + iDistScaleY * FMapCoordsPerMetre;
end;  // TfrmMap.GetCoBoundingBox

{-------------------------------------------------------------------------------
  Locates the COM Base Map system and stores it in FComMapFormat, if the base map is a COM
      system.
  Returns true if the Appsettings.MapDatasetSystem is a ComMapSystem: false otherwise
}
function TfrmMap.GetComBaseMapSystem: Boolean;
var
  i: Integer;
  lName: String;
begin
  Result:=False;
  for i := 0 to AppSettings.ComAddins.SpatialSystemInterfaces.Count - 1 do begin
    lName := (AppSettings.ComAddins.SpatialSystemInterfaces[i]
              as ISpatialReference).SpatialRefSystem;
    if lName = DatasetSpatialSystem then
    begin
      Result := True;
      try
        FComMapFormat := AppSettings.ComAddins.SpatialSystemInterfaces[i] as IBaseMapFormat;
      except
        on E:EIntfCastError do begin
          raise EMapBrowserError.Create(ResStr_ComMapNotBaseMap, E);
          FComMapFormat := nil;
        end;
      end;
      Exit;
    end;
  end;
end;  // TfrmMap.GetComBaseMapSystem 

{-------------------------------------------------------------------------------
  Get mthod for the property which gives the number of pixels represented by an equivalent 
      distance in metres (the main map unit) on the map. 
}
function TfrmMap.GetCoordToPixelFactor: msCoord;
begin
  Result.x := Abs (MapCanvasSize.x  / (VisibleExtents.dWest - VisibleExtents.dEast));
  Result.y := Abs (MapCanvasSize.y / (VisibleExtents.dNorth - VisibleExtents.dSouth));
end;  // TfrmMap.GetCoordToPixelFactor 

{-------------------------------------------------------------------------------
  This procedure makes a list of all the data that is within the bounding box regardless of 
      whether it is on the map or not. Called from pnlMapPanelClick 
}
procedure TfrmMap.GetDataWithinBoundingBox(const iNELatLong, iSWLatLong: TLatLong; out
    oSurveyEventKeys, oSampleKeys: TStringList; const iMapPos: msCoord);
var
  lPosition: TLatLong;
  lCoBotLeft, lCoTopRight: MSCoord;
  lNELat, lNELong, lSWLat, lSWLong, lSQL: String;
  lFormatSettings: TFormatSettings;
  lRoundMode: TFPURoundingMode;

  function IsUnderSelection(const spatialRef, system: String; lat, long: Double): Boolean;
  var
    rgn: HRgn;
  begin
    if DistributionScaleX = 1 then
      try
        // Bounding box determined for grid systems used to find items, therefore they ARE within.
        rgn := GridSquareToRegion(SpatialRefToGridSquare(spatialRef, system, -1), DatasetSpatialSystem);
        try
          Result := PtInRegion(rgn, Round(iMapPos.x), Round(iMapPos.y));
        finally
          DeleteObject(rgn);
        end;
      except
        // Grid squares not supported.
        on ESpatialRefError do
          Result := CheckPointInCircle(lat, long, lPosition);
      end
    else
      Result := CheckPointInBoundingBox(lat, long, lCoBotLeft, lCoTopRight);
  end;

begin
  // For queries. SQL doesn't like ',' a decimal separator!
  GetLocaleFormatSettings(LOCALE_USER_DEFAULT, lFormatSettings);
  lFormatSettings.DecimalSeparator := '.';

  oSurveyEventKeys.Clear;
  oSampleKeys.Clear;
  //Find point on map as a latlong.
  lPosition := SpatialRefFuncs.SpecificENToLatLong(TMapCoord(iMapPos), DatasetSpatialSystem);
  GetCoBoundingBox(lCoBotLeft, lCoTopRight, iMapPos, DistributionScaleX, DistributionScaleY);

  // Round the Lat/Long appropriately to hopefully get all relevant records.
  lRoundMode := GetRoundMode;
  SetRoundMode(rmUp);    // Force rounding up for NE corner
  lNELat  := FloatToStr(RoundTo(iNELatLong.Lat, -10), lFormatSettings);
  lNELong := FloatToStr(RoundTo(iNELatLong.Long, -10), lFormatSettings);
  SetRoundMode(rmDown);  // Force rounding down for SW corner
  lSWLat  := FloatToStr(RoundTo(iSWLatLong.Lat, -10), lFormatSettings);
  lSWLong := FloatToStr(RoundTo(iSWLatLong.Long, -10), lFormatSettings);
  SetRoundMode(lRoundMode);
  try
    lSQL :=
        'SELECT %s, Spatial_Ref, Spatial_Ref_System, Lat, Long FROM %s'
        + ' WHERE Lat>=' + lSWLat + ' AND Lat<=' + lNELat
        + ' AND Long>=' + lSWLong + ' AND Long<=' + lNELong;
    // Get events.
    with dmDatabase.ExecuteSQL(Format(lSQL, [PK_SURVEY_EVENT, TN_SURVEY_EVENT]), True) do
    begin
      while not Eof do begin
        if IsUnderSelection(
            Fields['Spatial_Ref'].Value,
            Fields['Spatial_Ref_System'].Value,
            Fields['Lat'].Value,
            Fields['Long'].Value) then
          oSurveyEventKeys.Add('''' + Fields[PK_SURVEY_EVENT].Value + '''');
        MoveNext;
      end;
      Close;
    end;

    // Get samples.
    with dmDatabase.ExecuteSQL(Format(lSQL, [PK_SAMPLE, TN_SAMPLE]), True) do
    begin
      while not Eof do begin
        if IsUnderSelection(
            Fields['Spatial_Ref'].Value,
            Fields['Spatial_Ref_System'].Value,
            Fields['Lat'].Value,
            Fields['Long'].Value) then
          oSampleKeys.Add('''' + Fields[PK_SAMPLE].Value + '''');
        MoveNext;
      end;
      Close;
    end;
  except
    on EDatabaseError do
      raise EMapDatabaseError.CreateNonCritical(ResStr_DBWriteFail);
  end;//try..Except
end;  // TfrmMap.GetDataWithinBoundingBox

{-------------------------------------------------------------------------------
}
function TfrmMap.GetGridReference: String;
begin
  if FGridReference = '' then
    raise EMapBrowserError.CreateNonCritical(ResStr_NoSpatialRef)
  else
    Result := FGridReference;
end;  // TfrmMap.GetGridReference 

{-------------------------------------------------------------------------------
  Returning data to other forms when the magnifier is clicked 
}
function TfrmMap.GetKeyList: TKeyList;
var
  lNewKeyList: TEditableKeyList;
  lSpatialRefString: TStringCoord;
  lValidSR: TValidSpatialRef;
  lWantPolygon: Boolean;

  procedure AddSelectedObjectToKeyList(AKeyList: TEditableKeyList);
  begin
    if ObjectDetails.SheetID > -1 then begin  // something selected
      // Add Object's Map sheet and static id to list
      AKeyList.SetTable('POLYGON');
      AKeyList.AddItem(IntToStr(ObjectDetails.SheetID),
                       IntToStr(ObjectDetails.ObjectStaticID));
      // Add Location/Admin Area key, if attached to object
      if CompareText(ObjectDetails.ObjectName, ResStr_NoName) = 0 then
        if ObjectDetails.IsAdminArea then
          AKeyList.AddItem(ObjectDetails.ObjectLocationKey, TN_ADMIN_AREA)
        else
          AKeyList.AddItem(ObjectDetails.ObjectLocationKey, TN_LOCATION);
    end;
  end;
  
begin
  lWantPolygon := not FSelectingArea;
  FSelectingArea := False;
  { Return an editable key list }
  lNewKeyList := TEditableKeyList.Create;
  lNewKeyList.SetTable('SPATIAL_REF');

  try
    { Sometimes necessary to return BOUNDING BOX data }
    if Tool = TOOL_SELECT_AREA then begin
      // Bounding box
      if not lWantPolygon then begin
        { Return data as follows :
                      Table name: 'SPATIAL_REF' as text
                Item1, KeyField2: BottomLeft SpatialReference
                Item2, KeyField2: TopRight SpatialReference }
        lSpatialRefString := BoundingBoxLLtoSpatialRef(BoundingBox);
        lNewKeyList.AddItem('', AllowForAccuracy(lSpatialRefString.x,
                                Appsettings.SpatialRefSystem));
        lNewKeyList.AddItem('', AllowForAccuracy(lSpatialRefString.y,
                                Appsettings.SpatialRefSystem));
      end else
        // Polygon
        AddSelectedObjectToKeyList(lNewKeyList);
    end else
    { for Location Details... need to return (1) a central spatial ref,
                                             (2) an object ID
                                             (3) the object sheet name }
    if (RequestorForm is TfrmLocationDetails) and
       (SelectedRegion.SheetID <> -1) and
       (not TfrmLocationDetails(RequestorForm).ExpectingSpatialRef) then
    begin
      lNewKeyList.AddItem(ReadMapDatabase(SelectedRegion.SheetID, SelectedRegion.ObjectID,
                                          MS_FIELD_STATICID),
                                          GetObjectCentroid(SelectedRegion));
      // Sheet name returned in table name of keylist
      lNewKeyList.SetTable(LayerLegend[SelectedRegion.SheetID].MapSheetKey);
    end else
    // Want a polygon
    if (RequestorForm is TdlgWizard) or (RequestorForm is TFrmParameters)then
      AddSelectedObjectToKeyList(lNewKeyList)
    else begin
      { Sometimes necessary to return just a LOCATION KEY and SPATIAL REF
        ie. SpeciesCard, PlaceCard, LocationDetails, EventDetails, SampleDetails }
      { Return data as follows:  Item1, KeyField1: LocationKey
                                 Item1, KeyField2: SpatialReference }
      { Dont return invalid data }
      if GridReference = '' then
        Raise EMapBrowserError.CreateNonCritical(ResStr_CantDrag);
      lValidSR := ValidSPatialRef(GridReference, AppSettings.SpatialRefSystem);
      if not lValidSR.Valid then
        Raise EMapBrowserError.CreateNonCritical(lValidSR.Error);
      lNewKeyList.AddItem(ObjectDetails.ObjectLocationKey, lValidSR.FormattedSR);
    end;
  finally
    Result := lNewKeyList;
    Tool   := TOOL_POINTER;
  end; // try finally
end;  // TfrmMap.GetKeyList

{-------------------------------------------------------------------------------
  Sets up the SPATIAL_REF data to be dragged and dropped. if there is a location then the key 
      is passed in the KeyField1, otherwise this is left blank.  KeyField2 is populated by the 
      SpatialReference. 
}
procedure TfrmMap.GetLocationKey(const Sender: TObject; var oDropSource: TJNCCDropSource);
begin
  if not validSpatialRef(GridReference, DatasetSpatialSystem).Valid then
    raise EMapBrowserError.CreateNonCritical(validSpatialRef(GridReference,
                    DatasetSpatialSystem).Error);
  if GridReference = '' then
    raise EMapBrowserError.CreateNonCritical(ResStr_CantDrag);
  
  oDropSource.DropData.SetTable('SPATIAL_REF');
  oDropSource.DropData.AddItem(ReadMapDatabase(SelectedRegion.SheetID, SelectedRegion.ObjectID,
                                               MS_FIELD_LOCATION_KEY),
                               GridReference);
end;  // TfrmMap.GetLocationKey

{-------------------------------------------------------------------------------
  Accessor method for the MapUserSheet property to return the index of the MapUser sheet, 
      which is required for drawing gridlines, distributions and other temporary visual stuff.
  Dependencies: Map dataset has to be open, MapUser sheet has to be created each time the map 
      is opened. 
}
function TfrmMap.GetMapUserSheetID: Integer;
var
  lSheet: Integer;
begin
  Result := -1;
  for lSheet := 0 to FMapServerLink.SheetTotal-1 do begin
    if FMapServerLink.SheetName(lSheet) = MAPUSER_SHEET_NAME then begin
      Result := lSheet;
      Break;
    end;
  end;
end;  // TfrmMap.GetMapUserSheetID 

{-------------------------------------------------------------------------------
  Returns the maximimum zoom scale for the base map.  Defaults to 100 (=1 to 100) but 
      different depending on the size of a point on the map
}
function TfrmMap.GetMaxZoomScale: Double;
begin
  Result := 100; // actual scale allowed
  if DatasetSpatialSystem = 'LTLN' then
    Result := 0.2 // arbitrary value since scale not accurate in latlong
  else
  if Assigned(FComMapFormat) then begin
    if FComMapFormat.CanDisplayGrids then
      if FComMapFormat.MapCoordsPerMetre < 1 then
        Result := 100
      else
        Result := 100 / FComMapFormat.MapCoordsPerMetre
    else
      Result := 0.2; // arbitrary
  end;
end;  // TfrmMap.GetMaxZoomScale

{-------------------------------------------------------------------------------
  Gives a spatial reference which represents the centroid of the passed in object ID. 
}
function TfrmMap.GetObjectCentroid(const iObject: TPolygonID): String;
var
  lRet: Integer;
  lCentroid: msCoord;
  lPointEN: TMapCoord;
begin
  if iObject.ObjectID > -1 then begin
    lRet := MapGetObjectCentroid(FMapServerLink.MapHandle,
                                 iObject.SheetID, iObject.ObjectID, lCentroid);
    if lRet <> MS_SUCCESS then
      raise EMapBrowserError.CreateNonCritical(ResStr_Centroid);
    lPointEN.x := lCentroid.X;
    lPointEN.y := lCentroid.Y;
    Result := EastingNorthingToSpatialRef(lPointEN, DatasetSpatialSystem, DatasetSpatialSystem);
  end else
    Result := GridReference;
end;  // TfrmMap.GetObjectCentroid 

{-------------------------------------------------------------------------------
  Seperated from QueryObjectAtPos for the benefit of the CenterOnObject method.
  The ObjectId and SheetID of the details object must be already populated - the rest is 
      filled in by this method  
}
procedure TfrmMap.GetObjectInfo(var ObInf: TQueryDetails);
var
  lCentroid: msCoord;
  lSheetName: Array[0..255] of Char;
  lLocorAdminKey: String;
begin
  { Read the Static ID }
  ObInf.ObjectStaticID := FMapServerLink.StaticIDForObject(ObInf.SheetId, ObInf.ObjectId);

  { Pass in ObjectID NOT static ID }
  lLocOrAdminKey := ReadMapDatabase(ObInf.SheetID, ObInf.ObjectID, MS_FIELD_LOCATION_KEY);
  if lLocOrAdminKey = '' then
    ObInf.ObjectName := ResStr_NoName
  else
  if lLocOrAdminKey[1] = '#' then begin // is admin area
    ObInf.ObjectLocationKey := Copy(lLocOrAdminKey, 2, 16);
    ObInf.IsAdminArea := True;
    ObInf.ObjectName := QueryForAdminName(ObInf.ObjectLocationKey);
  end else begin
    ObInf.ObjectLocationKey := lLocOrAdminKey;
    ObInf.IsAdminArea := False;
    ObInf.ObjectName := QueryForLocationName(ObInf.ObjectLocationKey);
  end;

  { Object Centroid.x }
  MapCheck(MapGetObjectCentroid(FMapServerLink.MapHandle,
                                ObInf.SheetID, ObInf.ObjectID, lCentroid),
           ResStr_CannotDetermineObjCent);
  ObInf.ObjectCentroid := lCentroid;

  { Read the sheet name }
  FillChar(lSheetName, Length(lSheetName), #0);
  MapCheck(MapGetSheetName(FMapServerLink.MapHandle, ObInf.SheetId, lSheetName));
  ObInf.SheetName := lSheetName;
end;  // TfrmMap.GetObjectInfo

{-------------------------------------------------------------------------------
  Get method for the property which gives the number of map units (metres) represented by an 
      equivalent number of screen pixels. 
}
function TfrmMap.GetPixelToCoordFactor: msCoord;
begin
  Result.x := Abs((VisibleExtents.dWest - VisibleExtents.dEast) / MapCanvasSize.x);
  Result.y := Abs((VisibleExtents.dNorth - VisibleExtents.dSouth) / MapCanvasSize.y);
end;  // TfrmMap.GetPixelToCoordFactor

{-------------------------------------------------------------------------------
  Combines values in the PCanvas Property for the size of the Printer Canvas and the offsets 
      which are HARD CODED as constants, to determine the global coordinates which the Printer
      Canvas will print. 
}
function TfrmMap.GetPrinterExtents: MsExtent;
var
  lPrintExt: msExtent;
  lCoOrigin: msCoord;
  lPageSize: msCoord;
  lScale: Double;
  lcoSize: msCoord;
begin
  MapCheck(MapGetViewOrigin(FMapServerLink.MapHandle, lCoOrigin.x, lCoOrigin.y));
  lScale := MapScale;
  
  { Set size of Canvas }
  lPageSize.x := FPCanvas.Rect.Right - FPCanvas.LeftGap -
                 FPCanvas.Rect.Left - FPCanvas.RightGap;
  lPageSize.y := FPCanvas.Rect.Bottom - FPCanvas.Rect.Top -
                 FPCanvas.BotGap - FPCanvas.TopGap;
  
  { Real MapServer Width shown on Printed map }
  if lPageSize.x<lPageSize.y then begin // portrait
    lCoSize.x := PAPER_SIZE_X * lScale;
    lCoSize.y := PAPER_SIZE_Y * lScale;
  end else begin // landscape
    lCoSize.x := PAPER_SIZE_Y * lScale;
    lCoSize.y := PAPER_SIZE_X * lScale;
  end;
  
  { Extents..  }
  lPrintExt.dNorth:= lcoOrigin.y + (lCoSize.y / 2);
  lPrintExt.dSouth:= lcoOrigin.y - (lCoSize.y / 2);
  lPrintExt.dEast := lcoOrigin.x + (lCoSize.x / 2);
  lPrintExt.dWest := lcoOrigin.x - (lCoSize.x / 2);
  
  Result := lPrintExt;
end;  // TfrmMap.GetPrinterExtents

{-------------------------------------------------------------------------------
  Takes the SampleType Key Value fromt the SampleType TMenuItem and returns the file name of 
      the recording card 
}
function TfrmMap.GetRecordCardPathFromDB(const iSampleTypeKey: String): String;
begin
  with dmDatabase.ExecuteSQL('SELECT Recording_Card FROM Sample_Type ' +
                             'WHERE Sample_Type_Key = ''' + iSampleTypeKey + '''', True) do
    try
      if not Eof then
        Result := Fields['Recording_Card'].Value
      else
        Result := '';
      Close;
    except
      on EDatabaseError do begin
        Close;
        raise EMapBrowserError.CreateNonCritical(ResStr_DBWriteFail);
      end;
    end;
end;  // TfrmMap.GetRecordCardPathFromDB 

{-------------------------------------------------------------------------------
  Return the polygon sheet ID as selected in the combo box on the map legend panel
}
function TfrmMap.GetSelectedSheetID: Integer;
begin
  if CurrentDrawingLayer <> nil then
    Result := FMapServerLink.SheetIDByFileName(TLayerItem(CurrentDrawingLayer).FileName)
  else
    Result := -1;
end;  // TfrmMap.GetSelectedSheetID 

{-------------------------------------------------------------------------------
  Return the size of four pixels on the map, as a coordinate.  Used for scaling  
}
function TfrmMap.GetSizeOfNPixels: Double;
begin
  Result := NUM_ACCURACY_PIX * PixelToCoordFactor.x;
end;  // TfrmMap.GetSizeOfNPixels 

{-------------------------------------------------------------------------------
  This is not intended for accurate calculations: it is only intended to give a rough idea 
}
function TfrmMap.GetSystemCoordsOverMapCoordsFactor: msCoord;
var
  lNELatLong, lSWLatLong: TLatLong;
  lNEMapCoord, lSWMapCoord: TMapCoord;
begin
  // Default
  Result.x := 1;
  Result.y := 1;
  // Should defaults change?
  if CompareText(DatasetSpatialSystem, LAT_LONG) = 0 then begin
    lNEMapCoord.x := VisibleExtents.dEast;
    lNEMapCoord.y := VisibleExtents.dNorth;
    lSWMapCoord.x := VisibleExtents.dWest;
    lSWMapCoord.y := VisibleExtents.dSouth;
    lNELatLong := SpecificENToLatLong(lNEMapCoord, DatasetSpatialSystem);
    lSWLatLong := SpecificENToLatLong(lSWMapCoord, DatasetSpatialSystem);
    Result.x := (lNELatLong.Long - lSWLatLong.Long) / (lNEMapCoord.x - lSWMapCoord.x);
    Result.y := (lNELatLong.Lat - lSWLatLong.Lat) / (lNEMapCoord.y - lSWMapCoord.y);
  end;
end;  // TfrmMap.GetSystemCoordsOverMapCoordsFactor 

{-------------------------------------------------------------------------------
}
procedure TfrmMap.HorizontalSplitterMoved(Sender: TObject);
begin
  inherited;
  if Assigned(FDatasetLegend) then FDatasetLegend.CheckColumnWidths;
  if Assigned(FLayerLegend) then FLayerLegend.CheckColumnWidths;
end;  // TfrmMap.HorizontalSplitterMoved 

{-------------------------------------------------------------------------------
  To allow the Map Browser to import objects (lines and regions) from other vector sheets, by
      importing the sheet and copying, one at a time, details from the imported sheet to the 
      Object Sheet.
  Dependencies: FMapHandle, FMapServerLink.dmMap must exist
  Special Notes: Changes the SheetTotal and ObjectTotal Properties.
  if not MapServer sheets then will also temporarily create some files in the MapFilePath
      folder. 
}
procedure TfrmMap.ImportBoundary;
var
  lImportObjectTotal: Integer;
  i: Integer;
  lSheetIndex: Integer;
  lDestSheetID: Integer;
  checkFile: Integer;
  boundaryImport: TdlgBoundaryImportDialog;
  lPolygonIndices: Array of Integer;
  lContinue : boolean;
begin
  // add a warning here
  lContinue := true;
  if mBoundaryImportInProgress then begin
    ShowInformation(ResStr_OnlyOneBoundaryImport);
    lContinue := false;
  end
  else if
    MessageDlg(ResStr_UseNewPolygon, mtWarning, [mbYes,MbNo],0) = MrNo then
    lContinue := false;

  if lContinue = true then
  begin
    if TPolygonLayerItem(CurrentDrawingLayer) = nil then
      ShowInformation(ResStr_NoPolygonLayer)
    else begin
      Tool := TOOL_POINTER; // prevents user doing other stuff
      FDeleteTemporaryMapFile := True;
      FPreBoundaryImportMapCursor := MapCursor;
      { Set the browsing directory in the Open dialog }
      if FLastFilePath <> '' then
        dlgOpen.InitialDir := FLastFilePath;

      if dlgOpen.Execute then
      begin
        FLocationKeys := TStringList.Create;
        try
          MapCursor := crHourGlass;
          FCurrentWindowState := WindowState;
          FFileName := dlgOpen.FileName;
          FLastFilePath := ExtractFilePath(FFileName);
          FShortName := ExtractFileName(FFileName);
          { if it is not MapServer sheet... }
          if CompareText(ExtractFileExt(FFileName), '.gsf') = 0 then
            FDeleteTemporaryMapFile := False;

          { Import it - lSheetIndex is either Sheet ID or error code}
          lSheetIndex := ImportMapFile(FFileName);

          if lSheetIndex < MS_SUCCESS then
          begin
            if lSheetIndex = -58 then
              raise EMapBrowserError.CreateNonCritical(ResStr_ImportCancelled)
            else
              raise EMapBrowserError.CreateNonCritical(ResStr_ImportFail + ' ' +
                                                       FFileName + #13 +
                                                       GetErrorString(lSheetIndex));
          end
          else
          begin
            try
              // Run this procedure not for its result, but so we can get the return code
              // to see if the file is valid.
              checkFile := MapDataGetNumAttributeFields(FMapServerLink.MapHandle, lSheetIndex, i);
              if checkFile <> MS_SUCCESS then
                ShowInformation(GetErrorString(checkFile))
              else begin
                { Now we have an extra sheet...
                  We want to take the objects one at a time and copy them onto the
                  object sheet before before detaching the object sheet and deleting
                  the import created files }
                MapCheck(MapGetNumObjects(FMapServerLink.MapHandle,
                                          lSheetIndex,
                                          lImportObjectTotal),
                         FFileName);

                lDestSheetID := FMapServerLink.SheetIDByFileName(
                    TPolygonLayerItem(CurrentDrawingLayer).FileName);

                boundaryImport := TdlgBoundaryImportDialog.Create(
                        nil, FMapServerLink.MapHandle, lSheetIndex);
                try
                  if boundaryImport.ShowModal() = mrOK then
                  begin
                    if boundaryImport.MatchExisting then
                    begin
                      FImportObjectTotal := lImportObjectTotal;
                      FBoundaryLocationMatch := TdlgBoundaryLocationMatch.Create(
                          Self,
                          boundaryImport.Attributes,
                          FMapServerLink.MapHandle,
                          lSheetIndex,
                          lDestSheetId,
                          FMapServerLink,
                          boundaryImport.PrimaryAttribute,
                          boundaryImport.MatchField,
                          FLocationKeys);
                      WindowState := wsMinimized;
                      mBoundaryImportInProgress := True;
                      FBoundaryLocationMatch.Show();
                     end
                    else
                    begin
                      lPolygonIndices := nil;
                      CompleteBoundaryImport(lSheetIndex, lImportObjectTotal,
                          lDestSheetID, FLocationKeys, lPolygonIndices);
                    end;
                  end
                finally
                  boundaryImport.Free;
                end;
              end;
            finally
              if not mBoundaryImportInProgress then
              begin
                CleanUpBoundaryImport();
              end;
            end;
          end; // if imported the file successfully
        finally
          if not mBoundaryImportInProgress then
          begin
            FreeAndNil(FLocationKeys);
          end;
        end; // try.. finally
      end;

    end;
  end;
end;  // TfrmMap.ImportBoundary

{-------------------------------------------------------------------------------
  Adds a sheet to the map dataset. if this is a MapServer.gsf file then by calling
      MapAttachSheet or if another format then by calling MapImport.
  Dependencies: FMapHandle, FMapServerLink.dmMap
  Side Effects: Returns the sheet index of the new sheet (or negative for MS Error code)
  Also modifies the QueryStatus of the sheet
}
function TfrmMap.ImportMapFile(const AFileName: String): Integer;
var
  lResult: Integer;
  lExtension: String;
  lDatasetFileName: String;
  lSheetIndex: Integer;
begin
  ImportSheetID := 0;
  lExtension := ExtractFileExt(AFileName);
  
  { if it is a MapServer file it only has to be attached }
  if CompareText(lExtension, '.gsf') = 0 then begin
    lDatasetFileName := AFileName;
    // If file did not exists in <MapFilePath>, move it there first.
    if not FileExists(AppSettings.MapFilePath + ExtractFileName(AFileName)) then begin
      CopyFilesLike(AFileName, AppSettings.MapFilePath);
      lDatasetFileName := AppSettings.MapFilePath + ExtractFileName(AFileName);
    end;
    // Then attach to map dataset.
    lResult := MapAttachSheet(FMapServerLink.MapHandle, PChar(lDatasetFileName));
  end else
    // Otherwise, import the file
    lResult := MapImport(FMapServerLink.MapHandle,
                         PChar(AFileName), PChar(AppSettings.MapFilePath), 0, True);

  if lResult <> MS_SUCCESS then begin
    MapShowError(lResult);
    Result := -1; //lResult;
    Exit;
  end else begin
    // Now, new sheet is last one, whether from MapImport or AttachSheet.
    lSheetIndex := FMapServerLink.SheetTotal - 1;

    lDatasetFileName := FMapServerLink.SheetFileName(lSheetIndex);
    { Set the query status of the new sheet }
    MapCheck(MapSetQuerySheet(FMapServerLink.MapHandle, lSheetIndex, False),
             Format(ResStr_ErrorSheetQryStatus, [lDatasetFileName]));
    { ensure sheet is visible }
    MapCheck(MapSelectSheet(FMapServerLink.MapHandle, lSheetIndex, True));
    Result := lSheetIndex;

    // Force changes to be written to map dataset file.
    FMapServerLink.SaveChanges(BaseMapKey);
  end;
end;  // TfrmMap.ImportMapFile

{-------------------------------------------------------------------------------
}
function TfrmMap.LayerBySheetID(ASheetID: Integer): TLayerItem;
var
  i: Integer;
begin
  Result := nil;
  if ASheetID = -1 then Exit;
  
  with FLayerLegend do
    for i := 0 to Count - 1 do
      if (FMapServerLink.SheetIDByFilename(LegendItems[i].FileName) = ASheetID) then begin
        Result := LegendItems[i];
        Break;
      end;
end;  // TfrmMap.LayerBySheetID 

{-------------------------------------------------------------------------------
  Create an ADMIN_BOUNDARY record and associate the polygon with a given admin area
}
procedure TfrmMap.LinkSelectedBoundaryToAdmin(const iAdminAreaKey: TKeyString);
var
  lNewKey: TKeyString;
begin
  lNewKey := dmGeneralData.IDGenerator.GetNextKey(TN_ADMIN_BOUNDARY);
  dmGeneralData.ExecuteSQL('INSERT INTO Admin_Boundary(Admin_Boundary_Key, Object_ID, ' +
                           'Admin_Area_Key, Entered_By, Map_Sheet_Key) VALUES (''' +
                           lNewKey + ''', ' +
                           IntToStr(ObjectDetails.ObjectStaticID) + ', ''' +
                           iAdminAreaKey + ''', ''' +
                           Appsettings.UserID + ''', ''' +
                           FMapServerLink.SheetMapSheetKey(ObjectDetails.SheetID) + ''')',
                           ResStr_ErrInsAdminBoundary);
  { Store the key's in MS's database.  We prefix the key with a hash to indicate it
      is admin area, not location related }
  FMapServerLink.WriteToMapDatabase(MS_FIELD_LOCATION_KEY, ObjectDetails.SheetID,
                                    ObjectDetails.ObjectID, '#' + iAdminAreaKey);
  FMapServerLink.WriteToMapDatabase(MS_FIELD_LOCATION_BOUNDARY_KEY, ObjectDetails.SheetID,
                                    ObjectDetails.ObjectID, '#' + lNewKey);
  AppSettings.UpdateMapWindowSelectors;
end;  // TfrmMap.LinkSelectedBoundaryToAdmin 

{-------------------------------------------------------------------------------
}
procedure TfrmMap.LinkToBoundary(iSheetID, iObjectID: Integer);
var
  lfrmLocations: TfrmLocations;
  lfrmAdminDict: TfrmAdminAreaDictBrowser;
  spRootType: TADOStoredProc;
begin
  if ReadMapDatabase(iSheetID, iObjectID, MS_FIELD_LOCATION_KEY) = '' then
    NewLocation
  else
  { if a region WITH a Location Key...
    Go to that boundary in the locations form }
  if ObjectDetails.IsAdminArea then begin
    dmFormActions.actAdminAreaDiction.Execute;
    lfrmAdminDict := frmMain.GetForm(TfrmAdminAreaDictBrowser) as TfrmAdminAreaDictBrowser;
    if (lfrmAdminDict <> nil) then begin
      spRootType := FdmMap.spAdminAreaGetRootType;
      spRootType.Parameters.ParamValues['AdminAreaKey'] := ObjectDetails.ObjectLocationKey;
      spRootType.ExecProc;
      if not VarIsNull(spRootType.Parameters.ParamValues['RootTypeKey']) then begin
        lfrmAdminDict.SelectList(spRootType.Parameters.ParamValues['RootTypeKey']);
        lfrmAdminDict.LocateDictionaryItem(ObjectDetails.ObjectLocationKey, 'Admin Area');
      end;
    end; // iFormOn <> -1
  end else begin
    dmFormActions.actLocations.Execute;
    lfrmLocations := frmMain.GetForm(TfrmLocations) as TfrmLocations;
    if (lfrmLocations <> nil) then
      if not lfrmLocations.GotoSiteFromMapObject(ObjectDetails) then NewLocation;
  end;
end;  // TfrmMap.LinkToBoundary

{-------------------------------------------------------------------------------
  In order to link a region to a boundary, this take the object ID of the currently selected 
      region and queries to see if there is or is not a boundary associated. if there is not 
      then it calls to create a new one, otherwise it call the method to show the location 
      record.
  Side Effects: Takes in ObjectID.. NOT a staticID
}
procedure TfrmMap.LinkToBoundary(iMapSheetKey: TKeyString; iObjectID: Integer);
var
  lLayer: TLayerItem;
begin
  lLayer := FLayerLegend.LayerByKey(iMapSheetKey);
  if Assigned(lLayer) then
    LinkToBoundary(FMapServerLink.SheetIDByFileName(lLayer.FileName), iObjectID);
end;  // TfrmMap.LinkToBoundary 

{-------------------------------------------------------------------------------
  When the global coordinates of the cursor position are less than 6 digits long, to add zeros
      to the front of the string i.e. 563, 123 converts to 000563, 000123 
  JVB- this function now rounds the return result
}
function TfrmMap.MakeSixDigits(const iString: String): String;
var
  lAbsoluteInt: String;
begin
  lAbsoluteInt := IntToStr(Abs(Round(StrToFloat(iString))));
  Result := Copy('000000', 1, 6 - Length(lAbsoluteInt)) + lAbsoluteInt;
  // show negative if necessary
  if StrToFloat(iString)<0 then
    Result := '-' + Result;
end;  // TfrmMap.MakeSixDigits 

{-------------------------------------------------------------------------------
}
procedure TfrmMap.MakeTaxonAndBiotopeStrings(AEventKeys, ASamplekeys: TStringList; out
    AResults: TObjectList);
var
  i, j: Integer;
  lItemKeys: TStringList;
  lDatasetKeys, lSamples, lEvents: TObjectList;
  lTableName: String;
  
  { This function splits a long stringlist up into smaller ones with lengths
       MAXITEMS.  All items have commas at the end, except the last one }
  procedure SplitStringList(const AListToCut: TStringList; ALists: TObjectList);
  const
    MAXITEMS = 1000;//maximum number of items allowed in the lists;
  var
    lIdx: Integer;
    lListPart: TStringList;
  begin
    ALists.Clear;
    lIdx := 0;
    repeat
      lListPart := TStringList.Create;
      ALists.Add(lListPart);
      while (lIdx < AListToCut.Count) and (lListPart.Count < MAXITEMS) do begin
        lListPart.Add(AListToCut[lIdx]);
        Inc(lIdx);
      end;
    until lIdx = AListToCut.Count;
  end;

  procedure FindIntersection(AList1, AList2: TObjectList; const ASQL: String;
            AFormatRepeats: Integer; const AItemType: String);
  var
    lIdx1, lIdx2: Integer;
    lKeys1, lKeys2: String;
    lSql: string;
  begin
    for lIdx1 := 0 to AList1.Count -1 do
      for lIdx2 := 0 to AList2.Count -1 do begin
        // format input SQL with IN clauses
        lKeys1 := TStringList(AList1[lIdx1]).CommaText;
        if lKeys1 = '' then lKeys1 := #39#39;
        lKeys2 := TStringList(AList2[lIdx2]).CommaText;
        if lKeys2 = '' then lKeys2 := #39#39;
        case AFormatRepeats of
          1: lSQL := Format(ASQL, [lKeys1, lKeys2]);
          2: lSQL := Format(ASQL, [lKeys1, lKeys2, lKeys1, lKeys2]);
          3: lSQL := Format(ASQL, [lKeys1, lKeys2, lKeys1, lKeys2, lKeys1, lKeys2]);
        end;
        with dmDatabase.ExecuteSQL(lSql, true) do
          while not Eof do begin
            if not VarIsNull(Fields['ItemKey'].Value) then
              AResults.Add(TPointOnMap.Create(AItemType, Fields['ItemKey'].Value));
            MoveNext;
          end;
      end;
  end;

begin
  lDatasetKeys := TObjectList.Create;
  lSamples     := TObjectList.Create;
  lEvents      := TObjectList.Create;
  lItemKeys    := TStringList.Create;
  try
    //Add commas to the end of the keys in iSampleKeys
    SplitStringList(ASamplekeys, lSamples);
    SplitStringList(AEventKeys, lEvents);
    for i := 0 to DatasetLegend.Count - 1 do
      if DatasetLegend[i].Visible then begin
        lItemKeys.Clear;
        with DatasetLegend[i].DistributionPoints do
          for j := 0 to Count - 1 do
            if TMapDrawItem(Items[j]).Key <> '' then
              lItemKeys.Add(#39 + TMapDrawItem(Items[j]).Key + #39);

        // Turn the data in each resultset displayed into a stringlist suitable to
        // be part of a SQL String
        SplitStringList(lItemKeys, lDatasetKeys);
        lTableName := DatasetLegend[i].TableName;

        //Only look for survey events if there are survey events within the bounding box
        if AEventKeys.Count > 0 then
          if CompareText(lTableName, TN_SURVEY_EVENT) = 0 then
            FindIntersection(lEvents, lDatasetKeys,
                             SQL_EVENTS_FOR_EVENTS_INTERSECTION, 3,
                             TN_SURVEY_EVENT);

        //Only look for taxa, biotopes & Samples if there are samples within the bounding box
        if ASamplekeys.Count > 0 then begin
          if CompareText(lTableName, TN_SAMPLE) = 0 then
            FindIntersection(lSamples, lDatasetKeys,
                             SQL_SAMPLES_FOR_SAMPLES_INTERSECTION, 1,
                             TN_SAMPLE)
          else
          if CompareText(lTableName, TN_TAXON_LIST_ITEM) = 0 then
            FindIntersection(lSamples, lDatasetKeys,
                             SQL_TAXA_FOR_LIST_ITEM_AND_SAMPLE_INTERSECTION, 1,
                             TN_TAXON_OCCURRENCE)
          else
          if CompareText(lTableName, TN_TAXON_GROUP) = 0 then
            FindIntersection(lSamples, lDatasetKeys,
                             SQL_TAXA_FOR_GROUP_AND_SAMPLE_INTERSECTION, 1,
                             TN_TAXON_OCCURRENCE)
          else
          if CompareText(lTableName, TN_TAXON_OCCURRENCE) = 0 then
            FindIntersection(lSamples, lDatasetKeys,
                             SQL_TAXA_FOR_OCCURRENCE_AND_SAMPLE_INTERSECTION, 1,
                             TN_TAXON_OCCURRENCE)
          else
          if CompareText(lTableName, TN_BIOTOPE_LIST_ITEM) = 0 then
            FindIntersection(lSamples, lDatasetKeys,
                             SQL_BIOTOPES_FOR_LIST_ITEM_AND_SAMPLE_INTERSECTION, 1,
                             TN_BIOTOPE_OCCURRENCE)
          else
          if CompareText(lTableName, TN_BIOTOPE_OCCURRENCE) = 0 then
            FindIntersection(lSamples, lDatasetKeys,
                             SQL_BIOTOPES_FOR_OCCURRENCE_AND_SAMPLE_INTERSECTION, 1,
                             TN_BIOTOPE_OCCURRENCE);
        end;
        lDatasetKeys.Clear;
      end;
  finally
    lItemKeys.Free;
    lDatasetKeys.Free;
    lSamples.Free;
    lEvents.Free;
  end;
end;  // TfrmMap.MakeTaxonAndBiotopeStrings

{-------------------------------------------------------------------------------
  IMPORTANT ..
  Drag and Drop Master Code #2 
}
procedure TfrmMap.MapDataDropped(const Sender: TObject; const AFormat: Integer; const
    ASourceData: TKeyList; const ATextStrings: TStringList; var AHandled: Boolean);
var
  lTableName: String;
begin
  // Disconnect handler because of ProcessMessages that will can cause another form
  // to start handling drag&drop when it shouldn't.
  DatasetLegend.OnChange := nil;
  try
    if AFormat = CF_JNCCDATA then begin
      lTableName := Asourcedata.Header.TableName;
      { if dropping SURVEYS from the Observations screen...
        Whether you want to plot Survey Events or Samples }
      if CompareText(lTableName, TN_SURVEY) = 0 then
        SurveyDropped(ASourceData)
      else
      { if dropping SURVEY_EVENTS from the Observations screen... }
      if CompareText(lTableName, TN_SURVEY_EVENT) = 0 then
        EventDropped(ASourceData)
      else
      if CompareText(lTableName, TN_TAXON_LIST_ITEM) = 0 then
        TaxonDropped(ASourceData)
      else
      if CompareText(lTableName, TN_BIOTOPE_LIST_ITEM) = 0 then
        BiotopeDropped(ASourceData)
      else
      if FAddinTables.IndexOfName(lTableName) > -1 then
        AddinDataDropped(StringToGUID(FAddinTables.Values[lTableName]), ASourceData);
      AHandled := True;
    end;
  finally
    // All done, reconnect the event.
    DatasetLegend.OnChange := MapDatasetChange;
  end;
end;  // TfrmMap.MapDataDropped 

{-------------------------------------------------------------------------------
  Clear and rebuild the Export Dataset sub-menu.
}
procedure TfrmMap.MapDatasetMenuRefresh;
var
  i: Integer;
  item: TMenuItem;
begin
  mnuMapExportDataset.Clear;
  for i := 0 to FDatasetLegend.Count - 1 do begin
    item := TMenuItem.Create(mnuMapExportDataset);
    item.Caption := FDatasetLegend[i].Title;
    item.OnClick := MapDatasetExportDistributionPoints;
    mnuMapExportDataset.Add(item);
  end;
  mnuMapExportDataset.Enabled := mnuMapExportDataset.Count > 0;
  RefreshXPMenu;
end;  // TfrmMap.MapDatasetMenuRefresh

{-------------------------------------------------------------------------------
}
procedure TfrmMap.MapDatasetChange(Sender: TObject);
begin
  RefreshMap;
  Application.ProcessMessages;
end;  // TfrmMap.MapDatasetChange

{-------------------------------------------------------------------------------
}
procedure TfrmMap.MapDatasetDelete(Sender: TObject);
begin
  // The item is still in the DatasetLegend at this point.
  // So remove menu item another way.
  mnuMapExportDataset.Delete(sgDatasets.Row);
  mnuMapExportDataset.Enabled := mnuMapExportDataset.Count > 0;
  MapDatasetChange(Sender);
end;  // TfrmMapDatasetDelete

{-------------------------------------------------------------------------------
}
procedure TfrmMap.MapDatasetItemRefresh(Sender: TObject);
begin
  if TDatasetItem(Sender).TitleChanged then
    MapDatasetMenuRefresh;
end;  // TfrmMap.MapDatasetItemRefresh

{-------------------------------------------------------------------------------
}
procedure TfrmMap.MapLayerBackgroundRefresh(Sender: TObject);
var
  lSheetID: Integer;
begin
  with TBackgroundLayerItem(Sender) do begin
    // Get the Sheet ID, for MapServer.
    lSheetID := FMapServerLink.SheetIDByFileName(FileName);

    if VisibilityChanged and (lSheetID <> -1) then
      MapCheck(MapSelectSheet(FMapServerLink.MapHandle, lSheetID, Visible));
  end;
  RefreshMap;
  Application.ProcessMessages;
end;  // TfrmMap.MapLayerBackgroundRefresh

{-------------------------------------------------------------------------------
}
procedure TfrmMap.MapLayerBaseMapRefresh(Sender: TObject);
begin
  if TBaseMapLayerItem(Sender).TitleChanged then
    AppSettings.UpdateMapWindowSelectors;
  RefreshMapSheets;
  Application.ProcessMessages;
end;  // TfrmMap.MapLayerBaseMapRefresh 

{-------------------------------------------------------------------------------
}
procedure TfrmMap.MapLayerChange(Sender: TObject);
var
  i: Integer;
  lItem: TLayerItem;
begin
  // Always make sure each layer is linked to its proper event.
  for i := 0 to LayerLegend.Count - 1 do begin
    lItem := LayerLegend[i];
    if lItem is TBaseMapLayerItem then lItem.OnNeedRefresh := MapLayerBaseMapRefresh
    else
    if lItem is TBackgroundLayerItem then lItem.OnNeedRefresh := MapLayerBackgroundRefresh
    else
    if lItem is TPolygonLayerItem then lItem.OnNeedRefresh := MapLayerPolygonRefresh;
  end
end;  // TfrmMap.MapLayerChange

{-------------------------------------------------------------------------------
}
procedure TfrmMap.MapLayerDelete(Sender: TObject);
var
  lSheetID: Integer;
begin 
  if Sender is TLayerItem then begin
    lSheetID := FMapServerLink.SheetIDByFileName(TLayerItem(Sender).FileName);
    if Sender is TPolygonLayerItem then begin
      // Unselect the selection if on the deleted sheet
      if SelectedRegion.SheetID = lSheetID then
        SelectedRegion := PolygonID(-1, 0);
    end;
    MapCheck(MapDetachSheet(FMapServerLink.MapHandle, lSheetID));  // Keep synchronised
    ActivePolygonLayersChange;
    RefreshMap;
  end;
  AppSettings.UpdateMapWindowSelectors;
end;  // TfrmMap.MapLayerDelete

{-------------------------------------------------------------------------------
}
procedure TfrmMap.MapLayerMove(ALayer: TLayerItem; AFromPos, AToPos: integer);
begin
  MapMoveSheetToPos(FMapServerLink.MapHandle,
                    FMapServerLink.SheetIDByFileName(ALayer.FileName), AToPos);
  RefreshMap;
  ActivePolygonLayersChange;
  Application.ProcessMessages;
end;  // TfrmMap.MapLayerMove

{-------------------------------------------------------------------------------
}
procedure TfrmMap.MapLayerUpdatePopup(Sender: TObject);
begin
  // Disable the Export Polygon Layer menu for background layers. 
  if Sender is TMenuItem then
    with TMenuItem(Sender) do
      if MenuIndex = 2 then
        Enabled := not (FLayerLegend.LegendItems[sgLayers.Row] is TBackgroundLayerItem);
end;  // TfrmMap.MapLayerUpdatePopup

{-------------------------------------------------------------------------------
}
procedure TfrmMap.MapLayerPolygonRefresh(Sender: TObject);
var
  lSheetID: Integer;
begin
  with TPolygonLayerItem(Sender) do begin
    // Get the Sheet ID, for MapServer.
    lSheetID := FMapServerLink.SheetIDByFileName(FileName);

    if TitleChanged then
      AppSettings.UpdateMapWindowSelectors;

    if VisibilityChanged then begin
      // Only need to deselect if layer becomes invisible.
      if not Visible then
        if (ObjectDetails.SheetID <> -1) and (ObjectDetails.SheetID = lSheetID) then
          SelectedRegion := PolygonID(-1, 0);

      if lSheetID <> -1 then
        MapCheck(MapSelectSheet(FMapServerLink.MapHandle, lSheetID, Visible));
    end;

    // Need to call SetUnselectedPopygonColour, and SelectedRegion (for selected colour)
    if ColoursChanged or PatternChanged then begin
      SetUnselectedPolygonColour(TPolygonLayerItem(Sender), ColoursChanged, PatternChanged);
      Appsettings.UpdateMapWindowSelectors;
    end;
  end;
  ActivePolygonLayersChange;
  RefreshMap;
  Application.ProcessMessages;
end;  // TfrmMap.MapLayerPolygonRefresh

{-------------------------------------------------------------------------------
  Converts MapServer global coordinate into Printer Canvas coordinates. 
  Dependencies: PrinterExtents property 
}
function TfrmMap.MapToPrinterCanvas(iEast, iNorth: double; iFactor: msCoord): TPoint;
begin
  Result.x := Round((iEast - PrinterExtents.dWest) * iFactor.x);
  Result.y := Round((PrinterExtents.dNorth - iNorth)* iFactor.y);
end;  // TfrmMap.MapToPrinterCanvas 

{-------------------------------------------------------------------------------
  To be able to convert distances in terms of global map coordinates to distances in terms of 
      pixels.
}
function TfrmMap.MapToScreen(lat, long: double; Extents: MsExtent): TPoint;
begin
  Result.x := Round((lat - Extents.dWest) * CoordtoPixelFactor.x);
  Result.y := Round((Extents.dNorth - long) * CoordtoPixelFactor.y);
end;  // TfrmMap.MapToScreen

{-------------------------------------------------------------------------------
  Allows the Importing of objects from an external sheet onto the users Object drawing sheet  
}
procedure TfrmMap.mnuMapBoundaryImportClick(Sender: TObject);
begin
  inherited;
  if actCancelDraw.Enabled  then
    raise EMapBrowserError.CreateNonCritical(ResStr_CannotImportWhileDrawing);
  MapEditClearObject(FMapServerLink.MapHandle);

  ImportBoundary;
end;  // TfrmMap.mnuMapBoundaryImportClick

{-------------------------------------------------------------------------------
  Clears any plotted distributions from the map browser and clears the "Key" box 
}
procedure TfrmMap.mnuMapClearClick(Sender: TObject);
begin
  DatasetLegend.Clear;
  RefreshMap;
end;  // TfrmMap.mnuMapClearClick

{-------------------------------------------------------------------------------
}
procedure TfrmMap.mnuMapDeletePolygonClick(Sender: TObject);
begin
  Tool := TOOL_DEL_POLY;
  if SelectedRegion.ObjectID > -1 then
    DeletePolygon
  else
    MessageDlg(ResStr_SelectToDelete, mtInformation, [mbOK], 0);
  Tool := TOOL_POINTER;
  SelectedRegion := PolygonID(0, -1);
end;  // TfrmMap.mnuMapDeletePolygonClick

{-------------------------------------------------------------------------------
  Handles all clicks to the DISTRIBUTION POINTS submenu sets the distribution points on the
      MapUSER sheet  
}
procedure TfrmMap.mnuMapDistributionPointsClick(Sender: TObject);
var
  i: Byte;
  lMenuItem: TMenuItem;
  lScaleList: IBaseMapScaleList;
begin
  inherited;
  lMenuItem := nil;
  if Sender is TMenuItem then
    lMenuItem:=TMenuItem(Sender)
  else
  if Sender is TToolButton then  // Toolbutton triggers default menu item
  begin
    lMenuItem := pmDistributionPoints.Items.Find(ResStr_Cap_Default);
  end;

  if lMenuItem <> nil then
    with lMenuItem do begin
      for i:= 0 to Parent.Count - 1 do Parent.Items[i].Checked := False;
      Checked := True;
      if Parent.Parent <> nil then UpdatePopups
                              else UpdateMenus;
      if GetComBaseMapSystem and
         Supports(FComMapFormat, IID_IBaseMapScaleList, lScaleList) then
      begin
        if (lMenuItem <> mnuMapDistributionPoints) and (lMenuItem.Caption <> ResStr_Cap_Default) then
        begin
          lScaleList := (FComMapFormat as IBaseMapScaleList);
          DistributionScaleX := lScaleList.PointSizeX[Parent.IndexOf(lMenuItem)];
          DistributionScaleY := lScaleList.PointSizeY[Parent.IndexOf(lMenuItem)];
        end
        else if (lMenuItem.Caption = ResStr_Cap_Default) then
        begin
          DistributionScaleX := 1;
          DistributionScaleY := 1;
        end;
      end else begin
        case Parent.IndexOf(lMenuItem) of
          DP_10KM_GRID:
              begin
                DistributionScaleX := 10000;
                DistributionScaleY := 10000;
              end;
          DP_5KM_GRID :
              begin
                DistributionScaleX := 5000;
                DistributionScaleY := 5000;
              end;
          DP_2KM_GRID :
              begin
                DistributionScaleX := 2000;
                DistributionScaleY := 2000;
              end;
          DP_1KM_GRID :
              begin
                DistributionScaleX := 1000;
                DistributionScaleY := 1000;
              end;
          DP_100M     :
              begin
                DistributionScaleX := 100;
                DistributionScaleY := 100;
              end;
          DP_DEFAULT  :
              begin
                DistributionScaleX := 1;
                DistributionScaleY := 1;
              end;
        end;// of case
      end;// of if GetComBaseMapSystem
      RefreshMap;
    end; // if Sender is TMenuItem
end;  // TfrmMap.mnuMapDistributionPointsClick 

{-------------------------------------------------------------------------------
  Handles all clicks to the GRIDLINES submenu sets the gridline spacing  
}
procedure TfrmMap.mnuMapGridLinesClick(Sender: TObject);
var
  i: Byte;
  menuIdx: Integer;
  lMenuItem: TMenuItem;
  lScaleList: IBaseMapScaleList;
  lScaleList614: IBaseMapScaleList_614;
begin
  inherited;
  lMenuItem := nil;
  if Sender is TMenuItem then
    lMenuItem := TMenuItem(Sender)
  else
  if Sender is TToolButton then  // Toolbutton triggers default menu item
    lMenuItem := pmGridLines.Items.Find(ResStr_Cap_None);

  if lMenuItem <> nil then begin
    FGridActive := True;
    GridOffsetX := 0;
    GridOffsetY := 0;

    with lMenuItem do begin
      menuIdx := Parent.IndexOf(lMenuItem);

      for i:= 0 to Parent.Count - 1 do Parent.Items[i].Checked := False;
      Checked := True;
      if Parent.Parent <> nil then UpdatePopups
                              else UpdateMenus;

      if GetComBaseMapSystem and
         Supports(FComMapFormat, IID_IBaseMapScaleList, lScaleList) then
      begin
        if (lMenuItem <> mnuMapGridLines) and (lMenuItem.Caption <> ResStr_Cap_None) then
        begin
          GridLineScaleX := lScaleList.GridScaleX[menuIdx];
          GridLineScaleY := lScaleList.GridScaleY[menuIdx];

          if Supports(FComMapFormat, IID_IBaseMapScaleList_614, lScaleList614) then begin
            GridOffsetX := lScaleList614.GridOffsetX[menuIdx];
            GridOffsetY := lScaleList614.GridOffsetY[menuIdx];
          end;
        end else
        if lMenuItem.Caption = ResStr_Cap_None then
        begin
          FGridActive := False;
          GridLineScaleX := 0;
          GridLineScaleY := 0;
        end;
      end else begin
        case menuIdx of
          { Grid Scale is in metres }
          GS_100KM_GRID :
              begin
                GridLineScaleX := 100000;
                GridLineScaleY := 100000;
              end;
          GS_50KM_GRID :
              begin
                GridLineScaleX := 50000;
                GridLineScaleY := 50000;
              end;
          GS_10KM_GRID :
              begin
                GridLineScaleX := 10000;
                GridLineScaleY := 10000;
              end;
          GS_1KM_GRID :
              begin
                GridLineScaleX := 1000;
                GridLineScaleY := 1000;
              end;
          GS_NONE :
              begin
                FGridActive := False;
                GridLineScaleX := 0;
                GridLineScaleY := 0;
              end;
        end;// of case
      end;//of if GetComBaseMapSystem
    end; // with TMenuItem(Sender)
    RefreshMap;
  end; // if Sender is TMenuItem
end;  // TfrmMap.mnuMapGridLinesClick

{-------------------------------------------------------------------------------
}
procedure TfrmMap.mnuMapZoomExtentsClick(Sender: TObject);
begin
  Tool := TOOL_ZOOM_TO_EXT;
  MapScale := 0;
  ShownHint := True;
  RefreshMap;
   // turn off impending location hint, as it will now be wrong!
end;  // TfrmMap.mnuMapZoomExtentsClick 

{-------------------------------------------------------------------------------
  Depending on the tag of the calling menu item, moves the selected polygon between layers  
}
procedure TfrmMap.MovePolygonClick(Sender: TObject);
var
  lNewLayer: TPolygonLayerItem;
  lOldSheetKey: String;
  lOldStaticID: Integer;
  pKeyValue: Array[0..20] of char;
  pBoundaryKey: Array[0..20] of char;
begin
  inherited;
  if (SelectedRegion.SheetID <> -1) and (SelectedRegion.ObjectID <> -1) then begin
    { Read original settings so we can relink boundaries }
    lOldSheetKey := FMapServerLink.SheetMapSheetKey(SelectedRegion.SheetID);
    lOldStaticID := FMapServerLink.StaticIDForObject(SelectedRegion.SheetID,
                                                     SelectedRegion.ObjectID);
    MapCheck(MapDataGetAttribute(FMapServerLink.MapHandle, SelectedRegion.SheetID,
                                 MS_FIELD_LOCATION_KEY,
                                 SelectedRegion.ObjectID, pKeyValue, 20));
    MapCheck(MapDataGetAttribute(FMapServerLink.MapHandle, SelectedRegion.SheetID,
                                 MS_FIELD_LOCATION_BOUNDARY_KEY,
                                 SelectedRegion.ObjectID, pBoundaryKey, 20));
    lNewLayer := TPolygonLayerItem(FLayerLegend[TComponent(Sender).Tag]);
    MapCheck(MapDeleteObject(FMapServerLink.MapHandle,
                             SelectedRegion.SheetID, SelectedRegion.ObjectID));
    with TPolygonLayerItem(LayerBySheetID(SelectedRegion.SheetID)) do
      LastNumObjects := LastNumObjects - 1;

    FSelectedRegion.SheetID := FMapServerLink.SheetIDByFileName(lNewLayer.FileName);
    MapCheck(MapEditSetSheet(FMapServerLink.MapHandle, SelectedRegion.SheetID));
    lNewLayer.LastNumObjects := lNewLayer.LastNumObjects + 1;

    MapCheck(MapEditSetFillPattern(FMapServerLink.MapHandle, lNewLayer.MSPattern));
    MapCheck(MapEditSetColor(FMapServerLink.MapHandle, lNewLayer.UnSelectedColour));
    MapCheck(MapEditSetEdgeColor(FMapServerLink.MapHandle, lNewLayer.UnSelectedColour));
    { Commit the object in unselected state }
    MapCheck(MapEditCommitObject(FMapServerLink.MapHandle));
    FSelectedRegion.ObjectID := FMapServerLink.ObjectTotal[SelectedRegion.SheetID]-1;
    { Then select it }
    MapCheck(MapEditSelectObject(FMapServerLink.MapHandle,
                                 SelectedRegion.SheetID, SelectedRegion.ObjectID));
    MapCheck(MapEditSetColor(FMapServerLink.MapHandle, lNewLayer.UnSelectedColour));
    MapCheck(MapEditSetEdgeColor(FMapServerLink.MapHandle, lNewLayer.UnSelectedColour));
    MapCheck(MapEditUpdate(FMapServerLink.MapHandle));
    MapCheck(MapDataSetAttribute(FMapServerLink.MapHandle, SelectedRegion.SheetID,
                                 MS_FIELD_LOCATION_KEY, SelectedRegion.ObjectID, pKeyValue));
    MapCheck(MapDataSetAttribute(FMapServerLink.MapHandle, SelectedRegion.SheetID,
                                 MS_FIELD_LOCATION_BOUNDARY_KEY, SelectedRegion.ObjectID,
                                 pBoundaryKey));
    RefreshMap;
    RelinkMovedBoundary(lOlDSheetKey, lOldStaticID, lNewLayer.MapSheetKey,
                         FMapServerLink.StaticIDForObject(SelectedRegion.SheetID,
                                                          SelectedRegion.ObjectID));
    { Rehighlight the selected region }
    SelectedRegion := PolygonID(SelectedRegion.SheetID, SelectedRegion.ObjectID);

    // Force changes to be written to map dataset file.
    FMapServerLink.SaveChanges(BaseMapKey);
  end;
end;  // TfrmMap.MovePolygonClick 

{-------------------------------------------------------------------------------
  Creates a diaog box to ask whether to create a new location or to add the current details (
      from the ObjectDetails property) to an existing location and calls the appropriate 
      method in frmLocations.  NEW - now supports adding boundaries to admin areas 
}
procedure TfrmMap.NewLocation;
var
  lTempStrCoord: TStringCoord;
  lLocWindow: TfrmLocations;
  lValidSR: TValidSpatialRef;
  lDialog: TdlgMapLocationOptions;
begin
  lTempStrCoord.x := IntToStr(Round(ObjectDetails.ObjectCentroid.x));
  lTempStrCoord.y := IntToStr(Round(ObjectDetails.ObjectCentroid.y));
  
  { Validate Spatial Reference }
  lValidSR := ValidSpatialRef(GridReference, DatasetSpatialSystem);
  if lValidSR.Valid = False then
    raise EMapBrowserError.CreateNonCritical(lValidSR.Error + #13+
                            ResStr_ChangeSRSystem);
  lDialog := TdlgMapLocationOptions.Create(nil);
  try
    if lDialog.ShowModal = mrOK then begin
      case lDialog.RadioIndex of
        0: begin { New Location }
             lLocWindow := TfrmLocations(dmFormActions.DisplayForm(TfrmLocations));
             if Assigned(lLocWindow) then
               lLocWindow.AddSiteFromMapObject(BaseMapKey, FMapServerLink, ObjectDetails);
           end; // 0
  
        1: begin  { Link to current location }
             { Call the locations form }
             lLocWindow := TfrmLocations(dmFormActions.DisplayForm(TfrmLocations));
             if Assigned(lLocWindow) then
               if lLocWindow.EditMode = emView then
                 lLocWindow.EditSiteFromMap(BaseMapKey, FMapServerLink, ObjectDetails)
               else
                 ShowInformation(ResStr_LinkLocationWhileEditing);
           end; // 1:

        2: with TdlgFind.CreateSearchAdminType(Self, ResStr_LinkBoundToAdminArea,
                                               lDialog.AdminTypeKey) do
           begin
             SetSearchText('');
             if ShowModal = mrOk then LinkSelectedBoundaryToAdmin(ItemKey);
             Free;
           end;
      end; // case
    end; // if mrOK
  finally
    lDialog.Free;
  end; // try..finally
end;  // TfrmMap.NewLocation

{-------------------------------------------------------------------------------
  Ensure that sheets are displayed in the correct drawing z order, and that their query status 
      is correct. 
}
procedure TfrmMap.OrganiseSheets;
var
  lCount: Integer;
  lSheetID: Integer;
  lLayer: TLayerItem;
begin
  // Refresh the layers.
  ActivePolygonLayersChange;

  with dmDatabase.ExecuteSQL('SELECT Map_Sheet_Key, Sheet_Type, Dataset_Sheet_FileName '+
                             'FROM Map_Sheet WHERE Base_Map_Key = ''' + BaseMapKey +
                             ''' AND (Computer_ID =Host_Name() OR Sheet_Type=3) ' +
                             'AND Remove_Sheet=0 AND Sheet_Displayed=1 ' +
                             'ORDER BY Dataset_Sheet_Order ASC',
                             True) do
    try
      lCount := 0;
      while not Eof do begin
        lSheetID := FMapServerLink.SheetIDByFileName(
            DatasetSheetFileName(Fields['Sheet_Type'].Value,
                                 Fields['Dataset_Sheet_FileName'].Value));
  
        // If layer is not found, add it.
        lLayer := FLayerLegend.LayerByKey(Fields[PK_MAP_SHEET].Value);
        if not Assigned(lLayer) then begin
          FLayerLegend.AddItemFromFields(Fields);
          lLayer := FLayerLegend.LayerByKey(Fields[PK_MAP_SHEET].Value);
        end;
  
        // Initialise starting number of objects on polygon sheet
        if Fields['Sheet_Type'].Value = 3 then
          TPolygonLayerItem(lLayer).LastNumObjects := FMapServerLink.ObjectTotal[lSheetID];

        // Move to correct z order
        if lSheetID <> lCount then
          MapMoveSheetToPos(FMapServerLink.MapHandle, lSheetID, lCount);
        Inc(lCount);
        MoveNext;
      end;
      Close;
      // Make sure that only the polygon layers are queried
      DefineSheetQueryStatus;
    except
      on E:EDatabaseError do begin
        Close;
        raise EMapBrowserError.Create(ResStr_MapDatabaseWR);
      end; // try.. finally
    end;
end;  // TfrmMap.OrganiseSheets

{-------------------------------------------------------------------------------
  Set the list box width fixed when it is dropped
}
procedure TfrmMap.pnlDockSiteDockDrop(Sender: TObject; Source: TDragDockObject; X, Y: Integer);
begin
  Source.Control.Width := 177;
end;  // TfrmMap.pnlDockSiteDockDrop

{-------------------------------------------------------------------------------
  Only accept the legend list box
}
procedure TfrmMap.pnlDockSiteLeftDockOver(Sender: TObject; Source: TDragDockObject; X, Y:
    Integer; State: TDragState; var Accept: Boolean);
begin
  Accept := (Source.Control is TPanel) and (Source.Control.Name = 'pnlDockableLegend');
end;  // TfrmMap.pnlDockSiteLeftDockOver 

{-------------------------------------------------------------------------------
  Set the rectangle of influence so if you drop the panel near the dock site it still docks. 
}
procedure TfrmMap.pnlDockSiteLeftGetSiteInfo(Sender: TObject; DockClient: TControl; var 
    InfluenceRect: TRect; MousePos: TPoint; var CanDock: Boolean);
begin
  with InfluenceRect do begin
    Left := Left - 100;
    Right := Right + 100;
  end;
end;  // TfrmMap.pnlDockSiteLeftGetSiteInfo 

//==============================================================================
// Procedure Name: UpdateAreaSelectionToolCursor
//        Purpose: Shows the appropriate mouse cursor, when using the area
//                 selection tool, for either drawing a new bounding box (the
//                 cross-shaped cursor) or resizing the existing one if it has
//                 been drawn or populated already and the mouse pointer is
//                 hovering over or near it.
//     Parameters: X: Integer - The X coordinate relative to the map panel.
//                 Y: Integer - The Y coordinate relative to the map panel.
//------------------------------------------------------------------------------
procedure TfrmMap.UpdateAreaSelectionToolCursor(
    ResizeDragElement: TRectangleElement);
var
  Cursor: TCursor;
begin
  Cursor := MapCursor;

  case ResizeDragElement of
    TopLeftCorner, BottomRightCorner:
      Cursor := crSizeNWSE;
    TopRightCorner, BottomLeftCorner:
      Cursor := crSizeNESW;
    LeftSide, RightSide:
      Cursor := crSizeWE;
    TopSide, BottomSide:
      Cursor := crSizeNS;
    NoSideOrCorner:
      Cursor := crCross
  end;

  MapCursor := Cursor;
end;


{-------------------------------------------------------------------------------
  Calculates the Spatial Ref, Location name and various other details about the click by
      calling QueryObjectAtPos to return a TLocationDetails object. Also selectes a region and
      deselects the last.
  Clicking in delete mode, initiates the call to delete either any selected polygon or sample.
}
procedure TfrmMap.pnlMapPanelClick(Sender: TObject);
var
  lPosition: TMapCursorPos;
begin
  inherited;
  pnlMapPanel.SetFocus;
  if (Tool in [TOOL_POINTER, TOOL_DISPLAY_RECORDCARD,
               TOOL_FIND_SOURCE_DATA, TOOL_SUBTRACT_BOUNDARY]) then
  begin
    // grab the mouse cursor pos immediately, so there is no inaccuracy if the mouse is moved.
    lPosition := QueryCursorPosition;
    // give immediate visual feedback
    if Tool in [TOOL_POINTER, TOOL_SUBTRACT_BOUNDARY] then begin
      QuerySpatialReference(lPosition.MapPos);
      if dmFormActions.actTransferData.Enabled and (not CalledFromWizard) and (not CalledFromCustom) then begin
        // draw a point or square to show where the user clicked to return data, if doing data entry
        ShowClickPoint(lPosition);
      end;
    end;
    { Update the ObjectDetails properties }
    ObjectDetails := QueryObjectAtPos(lPosition);
    case Tool of
      TOOL_POINTER, TOOL_SUBTRACT_BOUNDARY:
          begin
            SelectedRegion := PolygonID(ObjectDetails.SheetID, ObjectDetails.ObjectID);
            if dmFormActions.actTransferData.Enabled and (not CalledFromWizard) and (not CalledFromCustom) then
              // reshow the click point as the selected region removed it
              ShowClickPoint(lPosition);
            if (SelectedRegion.SheetID >= 0) and (SelectedRegion.ObjectID >= 0) then begin
              //If a polygon is selected we need to enable the Quick Report menu
              pmQuickReports.Enabled := True;
              pmBatchUpdate.Enabled := True;
              pmAssociateBoundary.Enabled := True;
              pmDeletePolygon.Enabled := True;
              pmExtractGridSquares.Enabled := True;
            end else begin
              pmQuickReports.Enabled := False;
              pmBatchUpdate.Enabled := False;
              pmAssociateBoundary.Enabled := False;
              pmDeletePolygon.Enabled := False;
              pmExtractGridSquares.Enabled := False;
            end;
          end;

      TOOL_DISPLAY_RECORDCARD :
          begin
            SelectedRegion := PolygonID(ObjectDetails.SheetID, ObjectDetails.ObjectID);
            QuerySpatialReference;
            if GridReference = ResStr_BeyondSystemLimits then
              raise EMapBrowserError.CreateNonCritical(ResStr_SampleOutsideLimits);
            Tool := TOOL_POINTER;
            if (CompareText(ObjectDetails.ObjectName, 'None') = 0) or
               (CompareText(ObjectDetails.ObjectName, ResStr_NoName) = 0) then
              frmMain.DisplayRecordingCard(GetRecordCardPathFromDB(SampleTypeKeyValue),
                                           SampleTypeKeyValue, GridReference,'')
            else
              frmMain.DisplayRecordingCard(GetRecordCardPathFromDB(SampleTypeKeyValue),
                                           SampleTypeKeyValue, GridReference,
                                           ObjectDetails.ObjectName);
          end;

      TOOL_FIND_SOURCE_DATA: FindSourceDataUnderCursor(lPosition);
    end; // case
  end;
end;  // TfrmMap.pnlMapPanelClick

(**
 * When returning data from the map, if you click on a point the point or square you clicked
 * on must be shown, to make it clear what you clicked on.
 * See Mantis 264.
 *)
procedure TfrmMap.ShowClickPoint(position: TMapCursorPos);
var
  sqr: TGridSquare;
  point: TLatLong;
  gotSqr, gotPoint: boolean;
  // corners of the square or point
  bl, tr, br, tl, dot: msCoord;
  extent: msExtent;
begin
  // check bounding boxes still OK.
  gotSqr := false;
  gotPoint := false;
  if (SameText(AppSettings.SpatialRefSystem, OS_GB) or SameText(AppSettings.SpatialRefSystem, OS_NI))
      and (Copy(eSpatialRef.Text, 1, 6)<>LAT_LONG + ': ') then begin
    sqr := SpatialRefToGridSquare(FGridReference, AppSettings.SpatialRefSystem, -1);
    gotSqr := true;
  end else if Copy(eSpatialRef.Text, 1, 6)=LAT_LONG + ': ' then begin
    // this means we have clicked outside the range of the grid system. So can only display
    // a point. Handle this first, because in this case the current system is not actually
    // the one we have in FGridRerence
    point := ConvertToLatLong(FGridReference, LAT_LONG);
    gotPoint := true;
  end else begin
    // A point reference which should be the same system as the current selected one.
    point := ConvertToLatLong(FGridReference, AppSettings.SpatialRefSystem);
    gotPoint := true;
  end;
  // show a popup hint
  pnlMapPanel.Hint := eSpatialRef.Text;
  FHintPos := position.MousePos;
  FHintPos.X := FHintPos.X + pnlMapPanel.Left;
  FHintPos.Y := FHintPos.Y + pnlMapPanel.Top;
  FSpecificHintPos := true;
  ShownHint := False;
  if gotSqr then begin
    // Create an object on the map and ensure it is visible
    CreateNewMapObject(MS_LINE_OBJECT);
    MapCheck(MapEditSetColor(FMapServerLink.MapHandle, clRed));
    MapCheck(MapEditSetVisible(FMapServerLink.MapHandle, True));
    // Find the square's corners using the same system as the base map
    tl := msCoord(LatLongToSpecificEN(sqr.TopLeft, FDatasetSpatialSystem));
    tr := msCoord(LatLongToSpecificEN(sqr.TopRight, FDatasetSpatialSystem));
    bl := msCoord(LatLongToSpecificEN(sqr.BottomLeft, FDatasetSpatialSystem));
    br := msCoord(LatLongToSpecificEN(sqr.BottomRight, FDatasetSpatialSystem));
    // Build an object on the map
    MapCheck(MapEditInsertObjectCoord(FMapServerLink.MapHandle, MS_LAST_COORD, tl));
    MapCheck(MapEditInsertObjectCoord(FMapServerLink.MapHandle, MS_LAST_COORD, tr));
    MapCheck(MapEditInsertObjectCoord(FMapServerLink.MapHandle, MS_LAST_COORD, br));
    MapCheck(MapEditInsertObjectCoord(FMapServerLink.MapHandle, MS_LAST_COORD, bl));
    MapCheck(MapEditInsertObjectCoord(FMapServerLink.MapHandle, MS_LAST_COORD, tl));
    RefreshMap;
  end else if gotPoint then begin
     CreateNewMapObject(MS_CIRCLE_OBJECT);
     MapCheck(MapGetVisibleExtent(FMapServerLink.MapHandle, extent));
     // calculate a 3 pixel radius in real world coordinates
     MapCheck(MapEditSetSize(FMapServerLink.MapHandle,
         ((extent.dEast - extent.dWest) / pnlMapPanel.Width) * 3
     ));
     MapCheck(MapEditSetColor(FMapServerLink.MapHandle, clRed));
     MapCheck(MapEditSetVisible(FMapServerLink.MapHandle, True));
     dot := msCoord(LatLongToSpecificEN(point, FDatasetSpatialSystem));
     MapCheck(MapEditInsertObjectCoord(FMapServerLink.MapHandle, MS_LAST_COORD, dot));
     RefreshMap;
  end;
  pnlMapPanel.Repaint;
end;

{-------------------------------------------------------------------------------
  Either adds a point during polygon drawing, or starts drawing a drag rectangle.
}
procedure TfrmMap.pnlMapPanelMouseDown(Sender: TObject; Button: TMouseButton; Shift:
    TShiftState; X, Y: Integer);
var
  lParam: LongInt;
  DragStartPixel: TPoint;
begin
  inherited;
  if Button = mbLeft then
  begin
    MouseDownPixPos := Point(x, y);

    case Tool of
      TOOL_ZOOM_IN, TOOL_ZOOM_OUT:
        begin
          lParam := MakeLong(x, y);
          MapCheck(MapDragRectNew(FMapServerLink.MapHandle, lParam, 1));
          ShownHint := True; // turn off impending location hint, as it will now be wrong!
        end;

      TOOL_SELECT_AREA :
        begin
          Application.ProcessMessages;

          DragStartPixel := Point(Max(0, X), Max(0, Y));
          lParam := MakeLong(Max(MouseDownPixPos.X, 0) + 1,
              Max(MouseDownPixPos.Y, 0) + 1);

          // Determine which side or corner, if any, the user wants to drag.
          FBoundingBoxResizeDragElement :=
              GetPossibleBoundingBoxResizeDragElement(Point(X, Y));

          // Now generate the appropriate lParam to be used for the drag
          // rectangle, depending on which part of the existing rectangle is
          // being dragged. Max is used to ensure that the rectangle does not
          // start being drawn beyond the limits of the map panel, as this
          // would cause an error, and visually this makes no difference.
          case FBoundingBoxResizeDragElement of
            TopLeftCorner:
              begin
                DragStartPixel := BoundingBoxFake.BottomRight;
                lParam := MakeLong(BoundingBoxFake.Right + 1,
                      BoundingBoxFake.Bottom + 1);
              end;
            BottomLeftCorner:
              begin
                DragStartPixel.X := BoundingBoxFake.Right;
                DragStartPixel.Y := BoundingBoxFake.Top;
                lParam := MakeLong(BoundingBoxFake.Right + 1,
                    Max(BoundingBoxFake.Top, 0));
              end;
            LeftSide:
              begin
                DragStartPixel := BoundingBoxFake.BottomRight;
                lParam := MakeLong(BoundingBoxFake.Right + 1,
                    Max(BoundingBoxFake.Top, 0));
              end;
            TopRightCorner:
              begin
                DragStartPixel.X := BoundingBoxFake.Left;
                DragStartPixel.Y := BoundingBoxFake.Bottom;
                lParam := MakeLong(Max(BoundingBoxFake.Left, 0),
                    BoundingBoxFake.Bottom + 1);
              end;
            BottomRightCorner:
              begin
                DragStartPixel := BoundingBoxFake.TopLeft;
                lParam := MakeLong(Max(BoundingBoxFake.Left, 0),
                    Max(BoundingBoxFake.Top, 0));
              end;
            RightSide:
              begin
                DragStartPixel := BoundingBoxFake.TopLeft;
                lParam := MakeLong(Max(BoundingBoxFake.Left, 0),
                    Max(BoundingBoxFake.Top, 0));
              end;
            TopSide:
              begin
                DragStartPixel := BoundingBoxFake.BottomRight;
                lParam := MakeLong(Max(BoundingBoxFake.Left, 0),
                    BoundingBoxFake.Bottom + 1);
              end;
            BottomSide:
              begin
                DragStartPixel := BoundingBoxFake.TopLeft;
                lParam := MakeLong(Max(BoundingBoxFake.Left, 0),
                    Max(BoundingBoxFake.Top, 0));
              end;
            NoSideOrCorner:
              begin

              end;
          end;

          MouseDownPixPos := DragStartPixel;
          MapCheck(MapDragRectNew(FMapServerLink.MapHandle, lParam, 1));
        end;
      TOOL_PAN      : StartPan;
      TOOL_DRAW_POLY: StartDrawPoly;
    end; // of case
  end;
end;  // TfrmMap.pnlMapPanelMouseDown

//==============================================================================
// Procedure Name: pnlMapPanelMouseMove
//        Purpose: Event handler for movement of the mouse over the map surface.
//                 Updates the map pointer coordinates shown on the status bar.
//                 Handles resizing of the selection bounding box, or shows the
//                 appropriate resizing cursor when hovering over a corner or
//                 side of an existing selection boundary.
//------------------------------------------------------------------------------
procedure TfrmMap.pnlMapPanelMouseMove(Sender: TObject; Shift: TShiftState; X,
    Y: Integer);
var
  lText: String;
  lCoord: TStringCoord;
  llParam: LongInt;
  SnapPixel: TPoint;
begin
  inherited;
  try
    lCoord.x := FloatToStr(QueryMapPos.X);
    lCoord.y := FloatToStr(QueryMapPos.Y);
  except
    on E:Exception do
      raise EMapBrowserError.Create(Format(ResStr_MoveError,
          [FloatToStr(QueryMapPos.X), FloatToStr(QueryMapPos.Y)]), E);
  end;   // try
  
  // tidy up for display
  lCoord.x := MakeSixDigits(lCoord.x);
  lCoord.y := MakeSixDigits(lCoord.y);
  lCoord.x := MakeSixDigits(lCoord.x);
  lCoord.y := MakeSixDigits(lCoord.y);
  
  lText := Format(ResStr_LegScale, [FormatScale(MapScale)])+ ';  ';
  if DatasetSpatialSystem = 'LTLN' then
    lText := lText + ResStr_LatitudeLongitude
  else
    lText := lText + ResStr_EastingNorthing;
  lText := lText + ': (' + lCoord.x + ListSeparator + ' ' + lCoord.y + ')';
  frmMain.SetStatus(lText, False);  // don't process messages in an event handler
  case Tool of
    TOOL_ZOOM_IN, TOOL_ZOOM_OUT, TOOL_SELECT_AREA:
      begin
        if (ssLeft in Shift) then// If the left mouse button is held down...
        begin
          { Use zero if mouse pos outside range }
          SnapPixel := Point(Max(X, 0), Max(Y, 0));

          if Tool = TOOL_SELECT_AREA then
          begin
            // The 1-pixel offsets below are to get the drag rectangle to align
            // with the existing bounding box so that it looks like the same box
            // is being resized.
            // lParam specifies an offset between the drag rectangle and the
            // actual mouse pointer. When the mouse moves, we change this offset
            // to keep the drag rectangle aligned so that only the chosen side
            // is affected.
            case FBoundingBoxResizeDragElement of
              LeftSide:
                begin
                  llParam := MakeLong(
                      Min(SnapPixel.X, FBoundingBoxFake.Right - 10),
                      SnapPixel.Y + (FBoundingBoxFake.Bottom - SnapPixel.Y) + 1);
                end;
              TopSide:
                begin
                  llParam := MakeLong(SnapPixel.X +
                      (FBoundingBoxFake.Right - SnapPixel.X) + 1,
                      Min(SnapPixel.Y, FBoundingBoxFake.Bottom - 10));
                end;
              RightSide:
                begin
                  llParam := MakeLong(
                      Max(SnapPixel.X, FBoundingBoxFake.Left + 10),
                      SnapPixel.Y +
                      (FBoundingBoxFake.Bottom - SnapPixel.Y) + 1);
                end;
              BottomSide:
                begin
                  llParam := MakeLong(SnapPixel.X +
                      (FBoundingBoxFake.Right - SnapPixel.X) + 1,
                      Max(SnapPixel.Y, FBoundingBoxFake.Top + 10));

                end else
                begin
                  // Either a corner is being dragged and no special offset is
                  // needed, or the rectangle is first being drawn, not resized.
                  llParam := MakeLong(SnapPixel.X, SnapPixel.Y);
                end;
            end;
          end else
            llParam := MakeLong(Max(X, 0), Max(Y, 0));// Zoom tool, don't snap.

          // Don't worry the user if this fails (not calling MapCheck).
          MapDragRectUpdate(FMapServerLink.MapHandle, llParam);
        end else
        begin
          // Cursor doesn't change during drawing or resizing, so update it when
          // the left mouse button is NOT held down.
          if Tool = TOOL_SELECT_AREA then
          begin
            UpdateAreaSelectionToolCursor(
                GetPossibleBoundingBoxResizeDragElement(Point(X, Y)));
          end;
        end;
      end;
  end;
end;  // TfrmMap.pnlMapPanelMouseMove

{-------------------------------------------------------------------------------
  Ends either a pan, zoom or a select area operation, or opens the polygon drawing sub-menu
}
procedure TfrmMap.pnlMapPanelMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
    X, Y: Integer);
var
  lPoint: TPoint;
  lScreenPoint: TPoint;
begin
  inherited;
  lPoint.x := X;
  lPoint.y := Y;
  MouseUpPixPos := lPoint;
  GetCursorPos(lScreenPoint);

  case  Tool of
    TOOL_DRAW_POLY:
      if (Button = mbRight) and (FFirstCoord = False) then
        pmPolygonDrawing.popup(lScreenPoint.x, lScreenPoint.y);
    TOOL_PAN                   : if Button = mbLeft then EndPan;
    TOOL_ZOOM_IN..TOOL_ZOOM_OUT: if Button = mbLeft then EndZoom;
    TOOL_SELECT_AREA           : if Button = mbLeft then EndSelectArea;
  end; // case
  // need to manually popup the menu, as MapServer has a "double popup" bug
  if Assigned(FMapPopupMenu) and (Button=mbRight) then
    FMapPopupMenu.Popup(lScreenPoint.x, lScreenPoint.y);
end;  // TfrmMap.pnlMapPanelMouseUp

{-------------------------------------------------------------------------------
}
procedure TfrmMap.pnlMapPanelResize(Sender: TObject);
begin
  inherited;
  if Assigned(FMapServerLink) then
    MoveWindow(FMapServerLink.MapHandle, 0, 0, pnlMapPanel.Width, pnlMapPanel.Height, True);
end;  // TfrmMap.pnlMapPanelResize 

{-------------------------------------------------------------------------------
  When plotting distribution points on a map, whether they can be displayed to varying 
      different levels of accuracy (i.e.
  distribution spacing) is dependant on the accuracy of the entered spatial reference.
  A TMapDrawItem has a display property which determines whether the Distribution scale is too
      highly defined or not, given the accuracy of the entered spatial reference. This is set 
      initially on populating the list of TMapDraw items in
  - CreateEventList and
  - CreateSampleList.
  The displayed status is also updated on setting the DistributionScale to a new value 
}
function TfrmMap.PointDisplayedStatus(const ASpatialRef, ASpatialSys: String): Boolean;
var
  lCompactSR: String;
  lLength: Integer;
begin
  if (CompareText(ASpatialSys, OS_GB) = 0) or (CompareText(ASpatialSys, OS_NI) = 0) then begin
  { for UK and Irish spatial references then points have a cut-in and a cut out }
    lCompactSR :=  SpatialRefFuncs.RemoveSubstrings(ASpatialRef, ' ');
    { Convert Spatial Ref to numbers }
    if ASpatialSys = OS_GB then begin
      { Check for a TETRAD }
      if Tetrad(lCompactSR) then begin
        if (DistributionScaleX <= 1) or (DistributionScaleX >= 2000) then
          Result := True
        else
          Result := False;
        Exit;
      end; // tetrad
    end else
    if ASpatialSys = OS_NI then
      { Add a supurious 'Z' so that it now has a 2 figure prefix, so its
        length is equivalent to a UK Spatial Ref of the same accuracy }
       lCompactSR := 'Z' + lCompactSR;

    { Make all visible }
    Result := False;

    lLength := Length(lCompactSR);
    if (DistributionScaleX = 1) or
        ((DistributionScaleX = 100) and (lLength >= 8)) or
        ((DistributionScaleX >= 1000) and (DistributionScaleX <= 5000) and (lLength >= 6)) or
        ((DistributionScaleX = 10000) and (lLength >= 4)) then
      Result := True;
    //end; // case
  end else
  { for other grid systems, the accuracy of a point cannot be assessed,
    so Result is always set to True }
    Result := True;
end;  // TfrmMap.PointDisplayedStatus 

{-------------------------------------------------------------------------------
}
procedure TfrmMap.PreviewScreen;
begin
  // not implemented in maps
end;  // TfrmMap.PreviewScreen 

{-------------------------------------------------------------------------------
  Given a set of coordinates, prints a Big Dot, 10 pixels in size, centred at those 
      coordinates as the DEFAULT distribution size. 
}
procedure TfrmMap.PrintBigPixel(ACoords: TPoint; AColor: Cardinal);
begin
  Printer.Canvas.Brush.Color := AColor;
  Printer.Canvas.Ellipse(ACoords.x - 10 + FPCanvas.LeftGap,
                         ACoords.y - 10 + FPCanvas.TopGap,
                         ACoords.x + 10 + FPCanvas.LeftGap,
                         ACoords.y + 10 + FPCanvas.TopGap);
end;  // TfrmMap.PrintBigPixel

{-------------------------------------------------------------------------------
  Draws distribution point as transparent square.
}
procedure TfrmMap.PrintBigSquare(const spatialRef: String; square: TGridSquare; graphics: TGPGraphics;
  brush: TGPSolidBrush; pen: TGPPen; factor: MSCoord; paintedSquares, graySquares: TStringList);
var
  spatialRef10k: String;
  brushColour, penColour: Cardinal;
  polyPoints: Array[0..3] of TGPPoint;

  function GetGPPoint(latLong: TLatLong): TGPPoint;
  var
    eastNorth: TMapCoord;
    mapPoint: TPoint;
  begin
    eastNorth := LatLongToSpecificEN(latLong, DatasetSpatialSystem);
    mapPoint := MapToPrinterCanvas(eastNorth.x, eastNorth.y, factor);
    Result := MakePoint(mapPoint.x, mapPoint.y);
  end;

begin
  if paintedSquares.IndexOf(spatialRef) <> -1 then Exit;

  // Draw a gray 10k square over?
  if square.Precision < 10000 then begin
    spatialRef10k := spatialRef;
    ReduceGridPrecision(spatialRef10k, 10000, square.System);
    if graySquares.IndexOf(spatialRef10k) = -1 then begin
      graySquares.Add(spatialRef10k);
      // Change colour to some gray
      brush.GetColor(brushColour);
      brush.SetColor(MakeColor(50, 180, 180, 180));
      pen.GetColor(penColour);
      pen.SetColor(MakeColor(180, 180, 180));
      // Reuse method :-)
      PrintBigSquare(
          spatialRef10k, SpatialRefToGridSquare(spatialRef10k, square.System, -1),
          graphics, brush, pen, factor, paintedSquares, graySquares);
      // Change back to original colour
      pen.SetColor(penColour);
      brush.SetColor(brushColour);
    end;
  end;

  paintedSquares.Add(spatialRef);

  // Need 4 points, converted to map's system. Go clockwise.
  polyPoints[0] := GetGPPoint(square.BottomLeft);
  polyPoints[1] := GetGPPoint(square.TopLeft);
  polyPoints[2] := GetGPPoint(square.TopRight);
  polyPoints[3] := GetGPPoint(square.BottomRight);
  graphics.FillPolygon(brush, PGPPoint(@polyPoints), 4);
  graphics.DrawPolygon(pen, PGPPoint(@polyPoints), 4);
end;

{-------------------------------------------------------------------------------
  When passed a TList (To indicate what is being printed) and a style, reads TMapDrawItems 
      from the appropriate TLists of distribution data and plots it on the Printer Canvas. 
}
procedure TfrmMap.PrintDataPoints(AItem: TDatasetItem; AGraySquares: TStringList);
var
  PxWidthOfCircle: TPoint;
  coBotLeft: msCoord;
  coTopRight: msCoord;
  pxTopLeft: Tpoint;
  pxBotRight: TPoint;
  CoCurrentPoint: msCoord;
  pxPixelPoint: TPoint;
  lLatLong: TLatLong;
  lPageSize: Tpoint;
  lFactor: msCoord;
  i: Integer;
  lCursor: TCursor;
  lDSX, lDSY: Double;
  lItem: TMapDrawItem;
  paintedSquares: TStringList;
  graphics: TGPGraphics;
  brush: TGPSolidBrush;
  pen: TGPPen;
  r, g, b: Integer;
begin
  lDSX := DistributionScaleX;
  lDSY := DistributionScaleY;
  lCursor := MapCursor;
  try
    MapCursor := crHourGlass;
  
    { Set size of Canvas }
    lPageSize.x := FPCanvas.Rect.Right - FPCanvas.LeftGap -
                   FPCanvas.Rect.Left - FPCanvas.RightGap;
    lPageSize.y := FPCanvas.Rect.Bottom - FPCanvas.Rect.Top -
                   FPCanvas.BotGap - FPCanvas.TopGap;

    lFactor.x := Abs(lPageSize.x / (PrinterExtents.dEast - PrinterExtents.dWest));
    lFactor.y := Abs(lPageSize.y / (PrinterExtents.dNorth - PrinterExtents.dSouth));
  
    { size of circle in screen pixels }
    PxWidthOfCircle.x := Round(lDSX * lFactor.x);
    PxWidthOfCircle.y := Round(lDSY * lFactor.y);

    paintedSquares := TStringList.Create;
    // MakeColor wants separate RGB.
    r := AItem.Colour and $FF;
    g := (AItem.Colour and $FF00) shr 8;
    b := (AItem.Colour and $FF0000) shr 16;
    graphics := TGPGraphics.Create(Printer.Handle, Printer.Canvas.Handle);
    // Prepare for printing
    graphics.SetPageUnit(UnitPixel);
    graphics.TranslateTransform(FPCanvas.LeftGap, FPCanvas.TopGap);
    graphics.SetClip(MakeRect(FPCanvas.Rect.Left, FPCanvas.Rect.Top, lPageSize.x, lPageSize.y));

    brush := TGPSolidBrush.Create(MakeColor(50, r, g, b));
    pen := TGPPen.Create(MakeColor(r, g, b));

    try
      for i := 0  to AItem.DistributionPoints.Count - 1 do begin
        lItem := TMapDrawItem(AItem.DistributionPoints[i]);
        { Read the TMapDrawItem from the TList and see if it is to be displayed }
        if lItem.Displayed then begin
          lLatLong := lItem.LatLong;
          { Check for zero values }
          if (lLatLong.Lat <> NULL_LATLONG) and (lLatLong.Long <> NULL_LATLONG) then begin
            { Find Easting and northing for the particular map Projection }
            CoCurrentPoint := msCoord(LatLongToSpecificEN(lLatLong, DatasetSpatialSystem));
            // Round to 2 decimal places to remove maths errors
            if DistributionScaleX = 1 then begin
              // Draw a transparent square if item has one.
              if AppSettings.GridRefsAsSquares and lItem.HasGridSquare then
                PrintBigSquare(
                    lItem.SpatialRef,
                    lItem.GridSquare,
                    graphics,
                    brush,
                    pen,
                    lFactor,
                    paintedSquares,
                    AGraySquares)
              else begin
                { draw as a single pixel; }
                pxPixelPoint := MapToPrinterCanvas(CoCurrentPoint.x,
                                                   CoCurrentPoint.y,
                                                   lFactor);
                PrintBigPixel(pxPixelPoint, AItem.Colour);
              end;
            end else begin
              { coTopLeft / coBotRight are the corners of the appropriate
                bounding box within which the data points will fall }
              RoundCoords(CoCurrentPoint, -1);
              GetCoBoundingBox(coBotLeft, coTopRight, coCurrentPoint, lDSX, lDSY);

              { convert to pixels }
              pxTopLeft := MapToPrinterCanvas(coBotLeft.x, coTopRight.y, lFactor);
              pxBotRight:= MapToPrinterCanvas(coTopRight.x, coBotLeft.y, lFactor);

              if lItem.Date > AppSettings.CutOffDate then
              begin
                Printer.Canvas.Brush.Style := bsSolid;
                Printer.Canvas.Brush.Color := AItem.Colour;
              end else
                Printer.Canvas.Brush.Style := bsClear;

              { Should It Be Drawn ? }
              with FPCanvas do
                if (pxTopLeft.x  > (Rect.Left + LeftGap)) and
                   (pxBotRight.x < (Rect.Right - RightGap - LeftGap)) and
                   (pxTopLeft.y  > (Rect.Top + TopGap)) and
                   (pxBotRight.y < (Rect.Bottom - TopGap - BotGap)) then
                  { print }
                  Printer.Canvas.Ellipse(pxTopLeft.x + LeftGap, pxTopLeft.y + TopGap,
                                         pxBotRight.x + LeftGap, pxBotRight.y + TopGap);
            end; // of else draw as a region;
          end;
        end;
      end;
    finally
      paintedSquares.Free;
      pen.Free;
      brush.Free;
      graphics.Free;
    end;
  finally
    MapCursor := lCursor;
  end;// try-finally
end;  // TfrmMap.PrintDataPoints

{-------------------------------------------------------------------------------
  Draws gridlines onto the Printer Canvas.
}
procedure TfrmMap.PrintGridLines;
var
  lWest, lNorth, least, lSouth: Double;
  lPixToOrigin: TPoint;
  lPixCurrPos: TPoint;
  lCount: Integer;
  lFactor: msCoord;
  lPageSize: TPoint;
begin
  { Read the position of the screen extents }
  lWest  := PrinterExtents.dWest - GridOffsetX;
  lNorth := PrinterExtents.dNorth - GridOffsetY;
  lSouth := PrinterExtents.dSouth - GridOffsetY;
  lEast  := PrinterExtents.dEast - GridOffsetX;
  
  { Set size of Canvas }
  lPageSize.x := FPCanvas.Rect.Right - FPCanvas.LeftGap - FPCanvas.Rect.Left - FPCanvas.RightGap;
  lPageSize.y := FPCanvas.Rect.Bottom - FPCanvas.Rect.Top - FPCanvas.BotGap - FPCanvas.TopGap;

  { lFactor is the fac }
  lFactor.x := Abs(lPageSize.x / (lEast - lWest));
  lFactor.y := Abs(lPageSize.y / (lNorth - lSouth));

  { Drawing the main Cross hairs }
  lPixToOrigin.x := Round((0 - lWest) * lFactor.x);
  lPixToOrigin.y := Round(lNorth * lFactor.y);

  { Vertical Cross Hair }
  Printer.Canvas.MoveTo(FPCanvas.LeftGap + lPixToOrigin.x, FPCanvas.TopGap);
  Printer.Canvas.LineTo(lPixToOrigin.x + FPCanvas.LeftGap, lPageSize.y + FPCanvas.TopGap);

  { Horizontal Cross Hair }
  Printer.Canvas.MoveTo(FPCanvas.LeftGap, lPixToOrigin.y + FPCanvas.TopGap);
  Printer.Canvas.LineTo(lPageSize.x + FPCanvas.LeftGap, lPixToOrigin.y + FPCanvas.TopGap);

  { while between origin and the east }
  lPixCurrPos.x := lPixToOrigin.x;
  lCount := 0;
  lPixCurrPos.x := Round(lFactor.x * ((lCount * GridLineScaleX * FMapCoordsPerMetre) - lWest));

  while lPixCurrPos.x < (FPCanvas.Rect.Right - FPCanvas.LeftGap - FPCanvas.RightGap) do begin
    Printer.Canvas.MoveTo(lPixCurrPos.x + FPCanvas.LeftGap, FPCanvas.TopGap);
    if (lPixCurrPos.x > (FPCanvas.Rect.Left + FPCanvas.LeftGap)) and
       (lPixCurrPos.x < (FPCanvas.Rect.Right + FPCanvas.LeftGap)) then
      Printer.Canvas.LineTo(lPixCurrPos.x + FPCanvas.LeftGap, lPageSize.y + FPCanvas.TopGap);
    Inc(lCount);
    lPixCurrPos.x := Round(lFactor.x *
                           ((lCount * GridLineScaleX * FMapCoordsPerMetre) - lWest));
  end;

  { while between origin and the west }
  lPixCurrPos.x := lPixToOrigin.x;
  lCount := 0;
  lPixCurrPos.x := Round(lFactor.x *
                         ((0 - lWest) - (lCount * GridLineScaleX * FMapCoordsPerMetre)));
  while lPixCurrPos.x > FPCanvas.Rect.Left do begin
    Printer.Canvas.MoveTo(lPixCurrPos.x + FPCanvas.LeftGap, FPCanvas.TopGap);
    if lPixCurrPos.x < (FPCanvas.Rect.Right - FPCanvas.LeftGap - FPCanvas.RightGap) then
      Printer.Canvas.LineTo(lPixCurrPos.x + FPCanvas.LeftGap, lPageSize.y + FPCanvas.TopGap);
    Inc(lCount);
    lPixCurrPos.x := Round(lFactor.x *
                           ((0 - lWest) - (lCount * GridLineScaleX * FMapCoordsPerMetre)));
  end;

  { while between origin and the north }
  lPixCurrPos.y := lPixToOrigin.y;
  lCount := 0;
  lPixCurrPos.y := Round(lFactor.y * (lNorth - (lCount * GridLineScaleY * FMapCoordsPerMetre)));
  while lPixCurrPos.y > FPCanvas.TopGap do begin
    { Pixel Height of line := Distance from North Ext to Origin minus 100km,
                              all of which is factored}
    Printer.Canvas.MoveTo(FPCanvas.LeftGap, lPixCurrPos.y + FPCanvas.TopGap);
    if (lPixCurrPos.y < (FPCanvas.Rect.Bottom - FPCanvas.Rect.Top -
                         FPCanvas.TopGap - FPCanvas.BotGap)) and
       (lPixCurrPos.y > FPCanvas.TopGap) then
      Printer.Canvas.LineTo(lPageSize.x + FPCanvas.LeftGap, lPixCurrPos.y + FPCanvas.TopGap);
    Inc(lCount);
    lPixCurrPos.y := Round(lFactor.y *
                           (lNorth - (lCount * GridLineScaleY * FMapCoordsPerMetre)));
  end;

  { while between origin and the south }
  lPixCurrPos.y := lPixToOrigin.y;
  lCount := 0;
  lPixCurrPos.y := Round(lFactor.y * (lNorth + (lCount * GridLineScaleY * FMapCoordsPerMetre)));
  while lPixCurrPos.y < (FPCanvas.Rect.Bottom - FPCanvas.BotGap - FPCanvas.TopGap) do begin
    Printer.Canvas.MoveTo(FPCanvas.LeftGap, lPixCurrPos.y + FPCanvas.TopGap);
    if lPixCurrPos.y > (FPCanvas.Rect.Top + FPCanvas.TopGap) then
      Printer.Canvas.LineTo(FPCanvas.LeftGap + lPageSize.x, lPixCurrPos.y + FPCanvas.TopGap);
    Inc(lCount);
    lPixCurrPos.y := Round(lFactor.y *
                           (lNorth + (lCount * GridLineScaleY * FMapCoordsPerMetre)));
  end;
end;  // TfrmMap.PrintGridLines

{-------------------------------------------------------------------------------
}
procedure TfrmMap.PrintLegend;
var
  lBorderRect: TRect;
  strTitle: String;
  strScale: String;
  strGridSpacing: String;
  strDistSpacing: String;
  strCutOffYear: String;
  lPrintTitle: TdlgPrintTitle;
  lIndex: Integer;
  lBoxBottom: Integer;
  lNamedSystem: INamedSpatialReference;
  i: Integer;
begin
  // Constants
  lBorderRect := Rect(LEGEND_LEFT, LEGEND_TOP, LEGEND_RIGHT, LEGEND_BOTTOM);

  // Get the Print Title from the user
  lPrintTitle := TdlgPrintTitle.Create(Self);
  try
    if lPrintTitle.ShowModal = mrOk then strTitle := lPrintTitle.ePrintTitle.Text
                                    else Exit;
  finally
    lPrintTitle.Free;
  end; // try.. finally

  // Set up the scale, grid spacing, distribution spacing and cut-off year
  strScale := Format(ResStr_LegScale, [FormatScale(MapScale)]);
  if GetComBaseMapSystem and
      Supports(FComMapFormat, IID_INamedSpatialReference, lNamedSystem) then
  begin
    lNamedSystem := (FComMapFormat as INamedSpatialReference);
    for i := 0 to mnuMapGridLines.Count - 1 do begin
      if mnuMapGridLines.Items[i].Checked then
        strGridSpacing := Format(ResStr_LegGridInterval, [mnuMapGridLines.Items[i].Caption]);
    end;//for
  end else
    strGridSpacing :=Format(ResStr_LegGridInterval, [FloatToStr(GridLineScaleX/1000)]) + 'km';

  if GetComBaseMapSystem and
      Supports(FComMapFormat, IID_INamedSpatialReference, lNamedSystem) then
  begin
    lNamedSystem := (FComMapFormat as INamedSpatialReference);
    for i := 0 to mnuMapDistributionPoints.Count - 1 do begin
      if mnuMapDistributionPoints.Items[i].Checked then
        strDistSpacing := Format(ResStr_DistrSpacing, [mnuMapDistributionPoints.Items[i].Caption]);
    end; //for
  end else begin
    if DistributionScaleX > 1 then begin
      if DistributionScaleX > 999 then
        strDistSpacing := Format(ResStr_DistrSpacing, [FloatToStr(DistributionScaleX/1000)]) + 'km'
      else
        strDistSpacing := Format(ResStr_DistrSpacing, [FloatToStr(DistributionScaleX)]) + 'm';
    end else
      strDistSpacing := ResStr_DefDistrSpacing;
  end;

  strCutOffYear := Format(ResStr_CutOffYearSym, [FormatDateTime('yyyy', AppSettings.CutOffDate)]);
  
  lBoxBottom := PrintSymbol(lBorderRect, lBorderRect.Top, 0, False, bsClear, strTitle, True);
  // Draw Splitter line
  Printer.Canvas.Brush.Color := clWhite;
  Printer.Canvas.Brush.Style := bsSolid;
  Printer.Canvas.Pen.Color   := clBlack;
  Printer.Canvas.MoveTo(lBorderRect.Left, lBoxBottom + 10);
  Printer.Canvas.LineTo(lBorderRect.Right, lBoxBottom + 10);
  lBoxBottom := PrintSymbol(lBorderRect, lBoxBottom, 0, False, bsClear, '', True);
  // blank line where splitter goes
  lBoxBottom := PrintSymbol(lBorderRect, lBoxBottom, 0, False, bsClear, strScale, True);
  // if displaying gridlines
  if GridLineScaleX <> 0 then
    lBoxBottom := PrintSymbol(lBorderRect, lBoxBottom, 0, False, bsClear, strGridSpacing, True);

  // Draw distribution points
  if DatasetLegend.Count > 0 then
    lBoxBottom := PrintSymbol(lBorderRect, lBoxBottom, 0, False, bsClear, '', True);

  // blank line
  for lIndex:=0 to DatasetLegend.Count -1 do
    if DatasetLegend[lIndex].Visible then
      lBoxBottom := PrintSymbol(lBorderRect, lBoxBottom, DatasetLegend[lIndex].Colour,
                               True, bsSolid, DatasetLegend[lIndex].Title, True);

  // Draw polygon layers
  if FLayerLegend.Count > 0 then
    lBoxBottom := PrintSymbol(lBorderRect, lBoxBottom, 0, False, bsClear, '', True);

  // blank line
  with FLayerLegend do
    for lIndex := 0 to Count -1 do
      if (LegendItems[lIndex] is TPolygonLayerItem) and (LegendItems[lIndex].Visible) then
        with TPolygonLayerItem(LegendItems[lIndex]) do
          lBoxBottom := PrintSymbol(lBorderRect, lBoxBottom, UnselectedColour,
                                    True, Pattern, Title,  False);  // draw box

  // Draw Legend Box
  Printer.Canvas.brush.Color := clWhite;
  Printer.Canvas.brush.Style := bsClear;
  Printer.Canvas.Pen.Color   := clBlack;
  Printer.Canvas.RoundRect(lBorderRect.Left, lBorderRect.Top,
                           lBorderRect.Right, lBoxBottom, 10, 10);
end;  // TfrmMap.PrintLegend

{-------------------------------------------------------------------------------
  Prints the cuirrent screen map window and calls functions to draw a legend box and
}
procedure TfrmMap.PrintScreen;
var
  lOptions: MSPRINTINFO;
  lRet: Integer;
  lExtent: msExtent;
  lCursor: TCursor;
  i: Integer;
  graySquares: TStringList;
begin
  lCursor  := MapCursor;
  MapCursor:= crHourGlass;
  try
    frmMain.SetStatus(ResStr_PrintingMap);

    { It is necessary to get the Scale and various dataset details in order
      to allow printing printing }
    MapCheck(MapGetScale(FMapServerLink.MapHandle, lOptions.dScale));
    MapCheck(MapGetDatasetName(FMapServerLink.MapHandle, lOptions.szTitle));
    MapCheck(MapGetDatasetDesc(FMapServerLink.MapHandle, lOptions.szDescription));
    lExtent := ReadMapExtents;

    { Draw only the border and nothing else }
    lOptions.dwFlags := MS_PF_NONE;

    { Can Write to the Canvas from here }
    Printer.BeginDoc;
  except
    on Exception do
    begin
      Printer.abort;
      MapCursor := lCursor;
      raise EMapBrowserError.Create(ResStr_Print);
    end;
  end;
  { Set up the Printer Canvas }
  FPCanvas.Rect    := Printer.Canvas.ClipRect;
  FPCanvas.LeftGap := LEFT_OFFSET;
  FPCanvas.TopGap  := TOP_OFFSET;
  FPCanvas.RightGap:= RIGHT_OFFSET;
  FPCanvas.BotGap  := BOTTOM_OFFSET;

  try
    { Refresh Prior to Printing, but having set the FPrinting flag to True,
      in order that none of the MapUser stuff is redrawn }
    FPrinting := True;
    RefreshMap;

    MapCursor := crHourglass;
    { Mapserver print command }
    lRet := MapPrint(FMapServerLink.MapHandle, Printer.Canvas.Handle, lOptions);
    if lRet <> MS_SUCCESS then
    begin
      Printer.Abort;
      Printer.EndDoc;
      raise EMapBrowserError.Create(ResStr_Print);
    end;

    { Print Gridlines etc onto the Printer Canvas }
    if FGridActive then
      PrintGridLines;

    graySquares := TStringList.Create;
    try
      for i := 0 to DatasetLegend.Count - 1 do
        if DatasetLegend[i].Visible then
          PrintDataPoints(DatasetLegend[i], graySquares);
    finally
      graySquares.Free;
    end;
    PrintLegend;
  finally
    Printer.EndDoc;
    FPrinting := False;
    RefreshMap;
    frmMain.TaskFinished;
    MapCursor := lCursor;
  end;
end;  // TfrmMap.PrintScreen

{-------------------------------------------------------------------------------
}
function TfrmMap.PrintSymbol(iLegendRect: Trect; iTop: Integer; iColor: cardinal; iSymbol: Boolean;
    iStyle: TBrushStyle; const iComment: String; iCircle: Boolean): Integer;
var
  lTextRect: TRect;
  lLineHeight: Integer;
begin
  { Set Up the Text Rect }
  lTextRect := iLegendRect;
  Printer.Canvas.Font.Name := 'Arial';
  lLineHeight   := Printer.Canvas.TextHeight('A') + 5; // set correct heigth for a line
  lTextRect.Top := iTop;
  { Blank out the area we are writing to }
  Printer.Canvas.brush.Color := clWhite;
  Printer.Canvas.brush.Style := bsSolid;
  Printer.Canvas.Pen.Color   := clWhite;
  Printer.Canvas.Rectangle(lTextRect.Left, lTextRect.Top,
                           lTextRect.Right, Round(lTextRect.Top + lLineHeight * 1.25));
                                // each line 1.25 * height of a char
  { Draw our text just inside the box }
  lTextRect.Left  := lTextRect.Left + 10;
  lTextRect.Right := lTextRect.Right - 10;
  lTextRect.Top   := lTextRect.Top + lLineHeight div 4;

  Printer.Canvas.Brush.Color := iColor;
  Printer.Canvas.Brush.Style := iStyle;

  if iSymbol = True then begin
    if iCircle then
      Printer.Canvas.Ellipse(lTextRect.Left, lTextRect.Top + 5,
                             Round(lTextRect.Left + lLineHeight * 0.8),
                             Round(lTextRect.Top + lLineHeight * 0.8 + 5))
    else
      Printer.Canvas.Rectangle(lTextRect.Left, lTextRect.Top + 5,
                               Round(lTextRect.Left + lLineHeight * 0.8),
                               Round(lTextRect.Top + lLineHeight * 0.8 + 5));
    lTextRect.Left := lTextRect.Left + lLineHeight; // move over for text
  end; // if Symbol = True
  { Handle The Text }
  Printer.Canvas.Brush.Style := bsClear;
  Printer.Canvas.TextRect(lTextRect, lTextRect.Left, lTextRect.Top, iComment); // clipped text
  Result := lTextRect.Top + lLineHeight;
end;  // TfrmMap.PrintSymbol 

{-------------------------------------------------------------------------------
  Confirms it is OK to proceed when the selected area has an associated Location or Admin 
      Area.
}
function TfrmMap.ProceedWithSubtraction(super: Boolean): Boolean;
var
  lLocOrAdminKey: String;
  lStaticID: Integer;

  //Confirms it is OK to proceed when the selected area has an associated Location or Admin Area.
  
begin
  lLocOrAdminKey := ReadMapDatabase(SelectedRegion.SheetID, SelectedRegion.ObjectID,
                                    MS_FIELD_LOCATION_KEY);
  if lLocOrAdminKey = '' then
    Result := True
  else
    if MessageDlg(ResStr_BoundaryHaveLocation, mtWarning, mbOKCancel, 0) = idOK then
    begin
      if not super then
        with FMapServerLink do begin
          lStaticID := StaticIDforObject(SelectedRegion.SheetID, SelectedRegion.ObjectID);
          FdmMap.DeleteBoundary(lStaticID, SheetMapSheetKey(SelectedRegion.SheetID));
        end;
      Result := True;
    end else
      Result := False;
end;  // TfrmMap.ProceedWithSubtraction 

{-------------------------------------------------------------------------------
  To return a TMapCursorPosition object containing both the window pixel position and the real 
      world map coordinates of the current cursor position. 
}
function TfrmMap.QueryCursorPosition: TMapCursorPos;
var
  lCoord: msCoord;
  lWindowPos: TPoint;
  lParam: LongInt;
begin
  lWindowPos := QueryWindowXY;
  lParam := MakeLong(lWindowPos.x, lWindowPos.y);
  { Get MapServer map co-ordinates }
  MapCheck(MapPositionFromMouse(FMapServerLink.MapHandle, lParam, lCoord.x, lCoord.y));
  Result.MousePos.x:= lWindowPos.x;
  Result.MousePos.y:= lWindowPos.y;
  Result.MapPos.x  := lCoord.x;
  Result.MapPos.y  := lCoord.y;
end;  // TfrmMap.QueryCursorPosition

{-------------------------------------------------------------------------------
  Finds the name of an object on the map, if it is an admin area name  
}
function TfrmMap.QueryForAdminName(const iAdminAreaKey: String): String;
begin
  if iAdminAreaKey = '' then
    Result := ResStr_NoName
  else
    with dmDatabase.ExecuteSQL('SELECT ITEM_NAME FROM ADMIN_AREA WHERE ADMIN_AREA_KEY=''' +
                               iAdminAreaKey + '''', True) do
    begin
      try
        if RecordCount > 0 then
          Result := Fields['Item_Name'].Value
        else
          Result := ResStr_NoName;
      finally
        Close;
      end; // try
    end; // with
end;  // TfrmMap.QueryForAdminName 

{-------------------------------------------------------------------------------
  If a key value is held in the map's internal database for an object, then this queries the
      main database to return the preferred name for that Location_Key.
}
function TfrmMap.QueryForLocationName(const iLocationKey: String): String;
begin
  if iLocationKey = '' then begin
    Result := ResStr_NoName;
    exit;
  end;
  with FdmMap.qryLocName do
    try
      Parameters.ParamByName('iLocKey').Value := iLocationKey;
      Open;
      First;
      while not Eof do begin
        Result := FieldByName('ITEM_NAME').AsString;
        Next;
      end; // while not Eof
      if RecordCount = 0 then Result := ResStr_NoName;
      Close;
    except
      on EDatabaseError do
      EMapBrowserError.CreateNonCritical(ResStr_DBWriteFail);
    end;
end;  // TfrmMap.QueryForLocationName 

{-------------------------------------------------------------------------------
  Returns an array of objects found at any one give set of map coordinates.
  Objects on invisible layers are not returned. 
}
function TfrmMap.QueryLocationsFound(iCoords: msCoord): TLocationArray;
var
  lFound: Integer;
  lSheetID, lObjectID: Array [0..255] of Integer;
  i: Integer;
  lCurrentIndex: Integer;
  lLayer: TLayerItem;
begin
  MapCheck(MapQueryObjectsAtCoord(FMapServerLink.MapHandle, iCoords, 255, @lSheetID,
                                  @lObjectID, lFound, MS_QUERY_REGION or MS_QUERY_LINE));
  { Copy the object array into the return Result of the function }
  SetLength(Result, lFound);
  lCurrentIndex := 0;
  for i := 0 to lFound - 1 do begin
    lLayer := LayerBySheetID(lSheetID[i]);

    if Assigned(lLayer) then begin // should always be True, but just in case
      if lLayer.Visible then begin // drop invisible layer objects
        Result[lCurrentIndex] := PolygonID(lSheetID[i], lObjectID[i]);
        Inc(lCurrentIndex); // move to next item in Result array
      end else
        SetLength(Result, Length(Result) - 1);  // decrease Result size by one
    end else
      SetLength(Result, Length(Result) - 1);  // decrease Result size by one
  end;
end;  // TfrmMap.QueryLocationsFound

{-------------------------------------------------------------------------------
  Returns the global Map Coordinates of the current cursor position
}
function TfrmMap.QueryMapPos: msCoord;
var
  lCoord: msCoord;
  lWindowPos: TPoint;
  lParam: LongInt;
begin
  lWindowPos := QueryWindowXY;
  if (lWIndowPos.x > 0) and (lWindowPos.y > 0) and
     (lWIndowPos.x < pnlMapPanel.width) and
     (lWindowPos.y < pnlMapPanel.Height) then
  begin
    lParam := MakeLong(lWindowPos.x, lWindowPos.y);
    { Get MapServer map co-ordinates }
    MapCheck(MapPositionFromMouse(FMapServerLink.MapHandle, lParam, lCoord.x, lCoord.y));
    Result :=  lCoord ;
  end else begin
    Result.x := 0;
    Result.y := 0;
  end;
end;  // TfrmMap.QueryMapPos 

{-------------------------------------------------------------------------------
  Pass in a TMapCursorPos object and returns a TQueryDetails object containing :
  MapPosition - gobal map coordinates of the point
  ObjectName - if an object is 'hit' then it queries the
  database to find - if any - the name of the location 'hit'
  ObjectID - if an object is hit
  ObjectStaticID - if an object is hit, the static ID is stored
  in the sheet's internal database
  ObjectCentroid - in coordinates
  ObjectType - a byte correspondung;
  ObjectLocationKey : TKeyString;
  SheetName - Name of the sheet on which the object was found
  SheetID - Index of the sheet
  
  Dependencies: Takes in the cursor coordinates that the query concerns
  Calls: ReadMapDatabase, QueryForNameAndLocationKey etc 
}
function TfrmMap.QueryObjectAtPos(const iCursorPosition: TMapCursorPos): TQueryDetails;
var
  lNextObject: TPolygonID;
  lHits: TLocationArray;
begin
  SetLength(lHits, 0);
  Result.MapPosition.x := iCursorPosition.MapPos.x;
  Result.MapPosition.y := iCursorPosition.MapPos.y;

  { Searching for the array of objects causes a crash if there are zero in total }
  lHits := QueryLocationsFound(iCursorPosition.MapPos);

  { if multiple Objects are Found }
  if High(lHits) > 0 then begin
    { Are we cycling though objects - has the list of objects changed since the
      last click.. if so then assign the new list to FMultiLocations so we can
      check next time, otherwise cycle through. }
    if (not SameArray(lHits, FMultiLocations) and (High(lHits) <> -1)) then
      FMultiLocations := lHits;
    lNextObject := FindNextFArrayObject;
    Result.ObjectID := lNextObject.ObjectID;
    Result.SheetID := lNextObject.SheetID;
  end else
  // if lHits > 1
  if High(lHits) = 0 then begin
    { Just One Object Found }
    Result.ObjectID := lHits[0].ObjectID;
    Result.SheetId := lHits[0].SheetID;
  end else
  { No Objects Found }
  if High(lHits) < 0 then
    Result.ObjectID := -1;

  if Result.ObjectID > -1 then
    GetObjectInfo(Result)
  else
    Result := ZeroObjectsFound(iCursorPosition);
end;  // TfrmMap.QueryObjectAtPos

{-------------------------------------------------------------------------------
  Calculates the spatial reference string (taking into account accuracy) at the
      current cursor position.
}
procedure TfrmMap.QuerySpatialReference;
begin
  QuerySpatialReference(QueryMapPos);
end;

{-------------------------------------------------------------------------------
  Calculates the spatial reference string (taking into account accuracy) at the
      provided cursor position.
}
procedure TfrmMap.QuerySpatialReference(mapPos: msCoord);
var
  lLatLong: TLatLong;
  lFullString, lLatStr, lLongStr: String;
  i: Integer;
begin
  try

    { Convert to LatLong.  Depends on the base map system what we have to do }
    if (DatasetSpatialSystem = 'OSGB') or (DatasetSpatialSystem = 'OSNI') then
      lLatLong := SpecificENToLatLong(TMapCoord(mapPos), DatasetSpatialSystem)
    else
    if (DatasetSpatialSystem = 'LTLN') then begin
      lLatLong.Lat := mapPos.Y;
      lLatLong.Long:= mapPos.X;
    end else
      for i := 0 to AppSettings.ComAddins.SpatialSystemInterfaces.Count - 1 do begin
        if (AppSettings.ComAddins.SpatialSystemInterfaces.Items[i] as
            ISpatialReference).SpatialRefSystem = DatasetSpatialSystem then
        begin
          try
            FComMapFormat := AppSettings.ComAddins.SpatialSystemInterfaces.Items[i]
                             as IBaseMapFormat;
            lLatStr  := FComMapFormat.MapCoordToLat(mapPos.X,mapPos.Y);
            lLongStr := FComMapFormat.MapCoordToLong(mapPos.X,mapPos.Y);
            if (lLatStr = ResStr_BeyondSystemLimits ) or (lLongStr = ResStr_BeyondSystemLimits) then
              raise EOutOfRangeError.Create('')
            else
            if (lLatStr = '') or (lLongStr = '') then
              raise EComAddinError.CreateNonCritical(
                  (AppSettings.ComAddins.SpatialSystemInterfaces.Items[i] as ISpatialReference).GetLastError)
            else begin
              lLatLong.Lat := LatitudeToFloat(lLatStr);
              lLatLong.Long := LongitudeToFloat(lLongStr);
            end;
          except
            on E:EIntfCastError do
              FComMapFormat := nil;
          end;//try
        end;//if
      end;//for

    { Convert back from LatLong to a spatial reference. }
    if AppSettings.SpatialRefSystem = LAT_LONG then
      lFullString:= LTLNToAccurateFormattedLTLN(lLatLong)
    else
      lFullString := ConvertfromLatLong(lLatLong, AppSettings.SpatialRefSystem);
    if lFullString=ResStr_BeyondSystemLimits then begin
      FGridReference := AllowForAccuracy(LTLNToAccurateFormattedLTLN(lLatLong), LAT_LONG);
      eSpatialRef.Text := LAT_LONG + ': ' + LocaliseSpatialRef(FGridReference);
    end
    else begin
      FGridReference := AllowForAccuracy(lFullString, AppSettings.SpatialRefSystem);
      eSpatialRef.Text := LocaliseSpatialRef(FGridReference);
    end;
  except
    on ESpatialRefError do begin
      FGridReference := ResStr_SRUnable;
      eSpatialRef.Text := ResStr_BeyondSystemLimits;
    end;
    on E:EOutOfRangeError do begin
      FGridReference := ResStr_BeyondSystemLimits;
      eSpatialRef.Text := ResStr_BeyondSystemLimits;
    end;
  end;
end;  // TfrmMap.QuerySpatialReference

{-------------------------------------------------------------------------------
  Returns the coordinates of the mouse in terms of the x,y window pixels for the Map window. 
      Returns Coords from top left of the window.
}
function TfrmMap.QueryWindowXY: TPoint;
var
  lPoint: TPoint;
  lRect: TRect;
begin
  { Get the x and y pixel coordinates of the Cursor for the window (from Top-left) }
  GetCursorPos(lPoint);
  GetWindowRect(FMapServerLink.MapHandle, lRect);
  lPoint.x := lPoint.x - lRect.Left;
  lPoint.y := lPoint.y - lRect.Top;
  Result := lPoint;
end;  // TfrmMap.QueryWindowXY

{-------------------------------------------------------------------------------
  A wrapper for the MapServer.dll call and error handling so that a sheet's internal database 
      can be read and the string contained within, returned.
  Notes: Takes in a normal ObjectID... NOT a STATIC ID.
}
function TfrmMap.ReadMapDatabase(iSheet, iObjectID, iFieldNumber: Integer): String;
var
  lRet: Integer;
  pString: Array[0..255] of char;
begin
  Result := '';
  if FMapWindowOpen then
    if (iSheet > -1) and (iObjectID > -1) then
      if (FMapServerLink.ObjectTotal[iSheet] > 0) then begin
        lRet := MapDataGetAttribute(FMapServerLink.MapHandle,
                                    iSheet, iFieldNumber, iObjectID, pString, 20);
        if lRet <> MS_SUCCESS then
          Raise EMapBrowserError.CreateNonCritical(ResStr_MapDatabaseWR)
        else
          Result := Trim(StrPas(pString));
      end; // if Object > -1 etc
end;  // TfrmMap.ReadMapDatabase 

{-------------------------------------------------------------------------------
  Finds the combined extent of all sheets.  This is better than MapGetDatasetExtent, which 
      always includes the point 0, 0 
}
function TfrmMap.ReadMapExtents: MSExtent;
var
  lNorth, lSouth, lEast, lWest: Double;
  lSheetIdx: Integer;
begin
  { Get base map extent }
  MapCheck(MapGetSheetExternalExtents(FMapServerLink.MapHandle, 0,
                                      Result.dNorth, Result.dSouth,
                                      Result.dEast, Result.dWest));
  { Check any other sheets in case they are bigger (ignore map user sheet) }
  for lSheetIdx := 1 to FMapServerLink.SheetTotal-1 do
    if FMapServerLink.SheetName(lSheetIdx)<>MAPUSER_SHEET_NAME then begin
      MapCheck(MapGetSheetExternalExtents(FMapServerLink.MapHandle, lSheetIdx,
                                          lNorth, lSouth, lEast, lWest));
      Result.dNorth := Max(Result.dNorth, lNorth);
      Result.dSouth := Min(Result.dSouth, lSouth);
      Result.dEast := Max(Result.dEast, lEast);
      Result.dWest := Min(Result.dWest, lWest);
    end;
end;  // TfrmMap.ReadMapExtents 

{-------------------------------------------------------------------------------
  Procedure to refresh TfrmLocationDetails location boundary list, when changed by deleting a 
      map boundary  
}
procedure TfrmMap.RefreshLocationBoundaries;
var
  lfrmLocations: TForm;
begin
  lfrmLocations := dmFormActions.GetFormInstance(TfrmLocations);
  if lfrmLocations <> nil then
    if TfrmLocations(lfrmLocations).DetailForm is TfrmLocationDetails then
      TfrmLocationDetails(TfrmLocations(lfrmLocations).DetailForm).RefreshBoundaries;
end;  // TfrmMap.RefreshLocationBoundaries 

{-------------------------------------------------------------------------------
  Forces a refresh of the mapUser sheet, thus calling the WMU_MAPUSERDRAW message.  
}
procedure TfrmMap.RefreshMap;
begin
  if Assigned(FMapServerLink) then
  begin
    InvalidateRect(FMapServerLink.MapHandle, nil, True);
  end;
end;  // TfrmMap.RefreshMap

{-------------------------------------------------------------------------------
  Refresh the sheets on the map - called by MapOptions
}
procedure TfrmMap.RefreshMapSheets;
var
  lCursor: TCursor;
  lSheetID: Integer;
  lSheetRec: _Recordset;
  lSheetType: Integer;
  lDSFileName: String;
  lLastRefresh: String;

  { Sheet not in map dataset, so it needs importing, attaching, or creating }
  function SheetMissing: Integer;
  var
    lLayer: TLayerItem;
  begin
    if FileExists(lDSFileName) then begin
      // Already available so attach
      MapCheck(MapAttachSheet(FMapServerLink.MapHandle, PChar(lDSFileName)));
      Result := FMapServerLink.SheetTotal - 1;
    end else begin
      Result := 0;
      if (lSheetType <> 3) and FileExists(lSheetRec.Fields['File_Name'].Value) then
        // Import it, then store the imported sheet name
        Result := ImportMapFile(lSheetRec.Fields['File_Name'].Value)
      else
        // Source file doesn't exist, must be a new polygon layer to create.
        AddPolygonLayerToMap(lSheetRec.Fields[PK_MAP_SHEET].Value);

      // If importing from file went wrong, source file was not good. Need to clean up the database.
      if Result = -1 then begin
        // Clean up the Map_Sheet table.
        dmDatabase.ExecuteSQL('DELETE Map_Sheet WHERE Map_Sheet_Key = ''' +
                              lSheetRec.Fields[PK_MAP_SHEET].Value + '''');
        // Remove layer from legend too.
        FLayerLegend.Refresh;
      end else begin
        // Refresh variables.
        Result      := FMapServerLink.SheetTotal - 1;
        lDSFileName := FMapServerLink.SheetFileName(Result);

        // Now we know the GSF file, store the info, but no path!
        dmDatabase.ExecuteSQL('UPDATE Map_Sheet SET Dataset_Sheet_FileName = ' +
                              QuotedStr(ExtractFileName(lDSFileName)) +
                              ' WHERE Map_Sheet_Key = ''' +
                              lSheetRec.Fields[PK_MAP_SHEET].Value + '''');
        // Layer wants path too though.
        lLayer := FLayerLegend.LayerByKey(lSheetRec.Fields[PK_MAP_SHEET].Value);
        if Assigned(lLayer) then lLayer.FileName := lDSFileName;
      end;
    end;
  end;

  procedure CheckPolygonSettings(const ASheetID: Integer);
  var
    lUpdateColour, lUpdatePattern: Boolean;
    lColour: Cardinal;
    lPattern: Integer;
    lType: Byte;
    lFillState: Bool;
  begin
    MapCheck(MapGetSheetFillPoly(FMapServerLink.MapHandle, ASheetID, lFillState));
    if not lFillState then
      MapCheck(MapSetSheetFillPoly(FMapServerLink.MapHandle, ASheetID, True));
    { Only update if there are some polygons }
    if FMapServerLink.ObjectTotal[ASheetID] > 0 then begin
      { Examine first object to determine if update required }
      { Only update pattern if a polygon }
      MapCheck(MapGetObjectType(FMapServerLink.MapHandle, ASheetID, 0, lType));
      lUpdatePattern := False;
      if (lType = MS_REGION_OBJECT) or (lType = MS_CIRCLE_OBJECT) then begin
        MapCheck(MapGetObjectFillPattern(FMapServerLink.MapHandle, ASheetID, 0, lPattern));
        lUpdatePattern := lPattern <> ConvertPatternToMSPattern(
                                          lSheetRec.Fields['Pattern_Index'].Value);
      end;
      if lType<>MS_POINT_OBJECT then begin
        // You can't change the display settings of a point since it is a small bitmap.
        MapCheck(MapGetObjectColor(FMapServerLink.MapHandle, ASheetID, 0, lColour));
        lUpdateColour := lColour <> Cardinal(lSheetRec.Fields['Unselected_Colour'].Value);
        if lUpdatePattern or lUpdateColour then
          SetUnselectedPolygonColour(TPolygonLayerItem(LayerBySheetID(ASheetID)),
                                   lUpdateColour, lUpdatePattern);
      end;
    end;
  end;

  { Ensure the sheet's cut in and out scales are correct }
  procedure CheckCutInAndOut(const ASheetID: Integer);
  var
    lMaxScale, lMinScale: double;
    lCut: String;
  begin
    lCut := StringReplace(lSheetRec.Fields['Cut_In_Scale'].Value, ',', ThousandSeparator, [rfReplaceAll]);
    lMaxScale := (RemoveCommas(lCut) * FMapCoordsPerMetre / 100) * GetMaxZoomScale;
    lCut := StringReplace(lSheetRec.Fields['Cut_Out_Scale'].Value, ',', ThousandSeparator, [rfReplaceAll]);
    lMinScale := (RemoveCommas(lCut) * FMapCoordsPerMetre / 100) * GetMaxZoomScale;
    MapCheck(MapSetSheetScaleLimits(FMapServerLink.MapHandle,
                                    ASheetID ,True, lMinScale, lMaxScale));
  end;
  
  procedure CheckBoundingBox(const ASheetID: Integer);
  var
    lSWCoord, lNECoord: MSCoord;
    lNorth, lSouth, lEast, lWest: Double;
  begin
    { Convert LatLongs to Specific Dataset system Northing and Easting,
      since that is the way the map is displayed }
    with lSheetRec do
      if not (VarIsNull(Fields['SW_LONG'].Value) or VarIsNull(Fields['SW_LONG'].Value)) then
      begin
        lSWCoord := msCoord(LatLongToSpecificEN(LatLong(Fields['SW_LAT'].Value,
                                                        Fields['SW_LONG'].Value),
                                                DatasetSpatialSystem));
        lNECoord := msCoord(LatLongToSpecificEN(LatLong(Fields['NE_LAT'].Value,
                                                        Fields['NE_LONG'].Value),
                                                DatasetSpatialSystem));

        { Check the internal extents of the sheet to see what the width to
          height ratio should be like. }
        MapCheck(MapGetSheetInternalExtents(FMapServerLink.MapHandle, ASheetID,
                                            lNorth, lSouth, lEast, lWest),
                 Format(ResStr_ErrAccessingSheetExt, [Fields['Sheet_Name'].Value]));
        if (lSWCoord.x <> lWest) or (lSWCoord.y <> lSouth) or
           (lNECoord.x <> lEast) or (lNECoord.y <> lNorth) then
          MapCheck(MapSetSheetExternalExtents(FMapServerLink.MapHandle, ASheetID,
                                              lNECoord.y, lSWCoord.y, lNECoord.x, lSWCoord.x),
                   Format(ResStr_ErrSettingSheetExt, [Fields['Sheet_Name'].Value]));
      end;
  end;
  
  { Clear change flags as record now handled }
  procedure ClearChangeFlags;
  begin
    with lSheetRec do
      if Fields['New_Data'].Value or Fields['Modified_Data'].Value then
        dmDatabase.ExecuteSQL('UPDATE Map_Sheet SET New_Data = 0, Modified_Data = 0 ' +
                              'WHERE Map_Sheet_Key = ''' + 
                              Fields[PK_MAP_SHEET].Value + '''');
  end;

begin
  { Prevent Timer calling a Refresh at this point in time }
  tmRefresh.Enabled := False;
  lCursor := HourglassCursor;
  try
    // If something was removed from another screen, or another machine,
    // update the dataset first.
    RemoveSheets;
    // Then proceed with updated/added stuff, if any.
    lLastRefresh := FormatDateTime('yyyy-mm-dd hh:mm:ss', FLastSheetRefresh);
    // Remember when we did this BEFORE OPENING, so we pick up all changes
    FLastSheetRefresh := Now;
    try
      // Get all changed records pertinent to this user
      lSheetRec := dmDatabase.ExecuteSQL(Format(SQL_CHANGED_MAPSHEETS,
                     [BaseMapKey, lLastRefresh, lLastRefresh]), True);
      with lSheetRec do
        while not Eof do begin
          lSheetType  := Fields['Sheet_Type'].Value;
          lDSFileName := DatasetSheetFileName(lSheetType,
                                           VarToStr(Fields['Dataset_Sheet_FileName'].Value));
          lSheetID    := FMapServerLink.SheetIDByFileName(lDSFileName);

          if not Fields['Remove_Sheet'].Value then begin
            // Check sheet is in map dataset. Import, create or attach as needed.
            if lSheetID = -1 then lSheetID := SheetMissing;
            // If lSheetID is still -1, something went wrong and the error message would
            // already have been shown.
            if lSheetID <> -1 then begin
              // Now proceed.
              if lSheetType = 3 then
                CheckPolygonSettings(lSheetID)
              else begin
                CheckCutInAndOut(lSheetID);
                if lSheetType > 0 then // Vector/Raster layer, NOT base map.
                  CheckBoundingBox(lSheetID)
                else
                if Fields['New_Data'].Value then
                  MapScale := 0;       // importing a new base map so reset scale.
              end;
              // And hide if necessary.
              MapCheck(MapSelectSheet(FMapServerLink.MapHandle, lSheetID,
                                      Fields['Sheet_Displayed'].Value));
            end;
          end else
          // Sheet invisible or removed.
          if lSheetID <> -1 then // if not missing, make sure it's invisible.
            MapCheck(MapSelectSheet(FMapServerLink.MapHandle, lSheetID, False));

          ClearChangeFlags;
          MoveNext;
        end;
      if lSheetRec.RecordCount > 0 then begin // Refresh, if we had anything to do
        dmDatabase.ExecuteSQL('DELETE FROM Map_Sheet WHERE Base_Map_Key = ''' + BaseMapKey +
                              ''' AND (Computer_ID =Host_Name() OR Sheet_Type=3) ' +
                              'AND Remove_Sheet = 1');
        OrganiseSheets;
        RefreshMap;
      end;
    finally
      lSheetRec.Close;
    end; // try
  finally
    tmRefresh.Enabled := True;
    Screen.Cursor := lCursor;
  end; // try
end;  // TfrmMap.RefreshMapSheets

{-------------------------------------------------------------------------------
  RelinkMovedBoundary takes the sheet key and static id for a polygon, and changes all 
      location/admin boundary links to point to a new sheet/static id. 
}
procedure TfrmMap.RelinkMovedBoundary(const iOldSheetKey: String; iOldStaticID: Integer; const 
    iNewSheetKey: String; iNewStaticID: Integer);
begin
  dmDatabase.ExecuteSQL(Format('UPDATE Location_Boundary ' +
                               'SET Object_ID=%d, Map_Sheet_Key=''%s'' ' +
                               'WHERE Object_ID=%d AND Map_Sheet_Key=''%s''',
                               [iNewStaticID, iNewSheetKey, iOldStaticID, iOldSheetKey]));
  dmDatabase.ExecuteSQL(Format('UPDATE Admin_Boundary ' +
                               'SET Object_ID=%d, Map_Sheet_Key=''%s'' ' +
                               'WHERE Object_ID=%d AND Map_Sheet_Key=''%s''',
                               [iNewStaticID, iNewSheetKey, iOldStaticID, iOldSheetKey]));
end;  // TfrmMap.RelinkMovedBoundary 

{-------------------------------------------------------------------------------
  Takes an object (by object ID, not Static ID) and ensures that the associated boundary 
      record is linked correctly 
}
procedure TfrmMap.RelinkToBoundaryRecord(iSheetID, iObjectID: Integer);
var
  lBoundaryKey: String;
  lStaticID: Integer;
  lSheetKey: String;
begin
  lBoundaryKey := ReadMapDatabase(iSheetId, iObjectID, MS_FIELD_LOCATION_BOUNDARY_KEY);
  if lBoundaryKey <> '' then begin
    { Read the object's static ID }
    lStaticID := FMapServerLink.StaticIDforObject(iSheetId, iObjectID);
    lSheetKey := LayerBySheetID(iSheetID).MapSheetKey;
    { and update the database }
    dmDatabase.ExecuteSQL(Format('UPDATE Location_Boundary ' +
                                 'SET Object_ID=%d, Map_Sheet_Key=''%s'' ' +
                                 'WHERE Location_Boundary_Key=''%s''',
                                 [lStaticID, lSheetKey, lBoundaryKey]));
  end;
end;  // TfrmMap.RelinkToBoundaryRecord 

{-------------------------------------------------------------------------------
  If a sheet has been removed by another user, then we must detect its absence from the 
      MAP_SHEET table and detach it.
  Called only when the Map form is opened. 
}
procedure TfrmMap.RemoveSheets;
var
  i, lCount: Integer;
  lSQL: String;
begin
  lSQL := 'SELECT Count(*) AS Total FROM Map_Sheet WHERE Base_Map_Key = ''' + BaseMapKey +
          ''' AND (Computer_ID = Host_Name() OR Sheet_Type=3) ' +
          'AND Dataset_Sheet_Filename LIKE %s';
  with FMapServerLink do
    for i := SheetTotal - 1 downto 0 do
      if SheetName(i) <> MAPUSER_SHEET_NAME then begin
        with dmDatabase.ExecuteSQL(Format(lSQL,
             [QuotedStr('%' + ExtractFileName(SheetFileName(i)) + '%')]), True) do
        begin
          lCount := Fields['Total'].Value;
          Close;
        end;
        // Record not found corresponding to map sheet.
        if lCount = 0 then begin
          if Assigned(LayerBySheetID(i)) then
            // If Layer in legend, go through it to delete the layer.  That could be
            // because of another user removing polygon layers on another machine...
            with FLayerLegend do begin
              // Suppress message.
              ConfirmBeforeDelete := False;
              DeleteItem(IndexOf(LayerBySheetID(i)));
              ConfirmBeforeDelete := True;
            end
          else begin
            // Layer not in legend, only need to remove from Map Dataset file.
            // Unselect the selection if on the deleted sheet.
            if SelectedRegion.SheetID = i then
              SelectedRegion := PolygonID(-1, 0);
            // And then drop sheet.
            MapDetachSheet(MapHandle, i);
          end;
        end;
      end;
end;  // TfrmMap.RemoveSheets 

{-------------------------------------------------------------------------------
  Rounds the coordinates to the specified precision.  A positive value indicates the number of 
      significant figures, negative value indicates number of decimal places.
      However, each part of the coordinate is only rounded if its fractional
      part is not close to one third; this prevents rounding issues when using
      certain addin spatial reference systems such as MTBQYX.
}
procedure TfrmMap.RoundCoords(var AmsCoords: msCoord; APrecision: Integer);
  {-----------------------------------------------------------------------------
    Indicates if the given Double is within 0.00001 of one third
  }
  function IsCloseToOneThird(const ANumber: Double): Boolean;
  begin
    Result := Abs(ANumber - (1/3)) <= 0.00001;
  end;
begin
  SetRoundMode(rmNearest);
  If Not IsCloseToOneThird(Frac(AmsCoords.X)) then
    AmsCoords.X := RoundTo(AmsCoords.X, APrecision);
  If Not IsCloseToOneThird(Frac(AmsCoords.Y)) then
    AmsCoords.Y := RoundTo(AmsCoords.Y, APrecision);
end;  // TfrmMap.RoundCoords

{-------------------------------------------------------------------------------
  Are two arrays the same?
}
function TfrmMap.SameArray(const iArrayA, iArrayB: TLocationArray): Boolean;
var
  lCountA, lCountB: Integer;
  lItems: Integer;
begin
  lCountA := High(iArrayA);
  lCountB := High(iArrayB);
  
  if (lCountA = -1) or (lCountB = -1) or (lCountA <> lCountB) then begin
    Result := False;
    Exit;
  end else
    for lItems := 0 to lCountA do
      if (iArrayA[lItems].ObjectID <> iArrayB[lItems].ObjectID) or
         (iArrayA[lItems].SheetID <> iArrayB[lItems].SheetID) then
      begin
        Result := False;
        Exit;
      end;
  
  Result := True;
end;  // TfrmMap.SameArray 

{-------------------------------------------------------------------------------
  Event handler for clicking on the sample type menu items. When the menu is clicked, sets the 
      SampleTypeKeyValue property to that of the menu item. 
}
procedure TfrmMap.SampleTypeClick(Sender: TObject);
var
  lIndex: Integer;
  lPath: String;
begin
  with Sender as TMenuItem do begin
    lIndex := (TMenuItem(Sender).MenuIndex);
    SampleTypeKeyValue := FSampleTypeKeyList.Items[lIndex].KeyField1;
  end;
  
  lPath := GetRecordCardPathFromDB(SampleTypeKeyValue);
  if not FileExists(lPath) then
    raise EMapBrowserError.CreateNonCritical(ResStr_RCNotFound + lPath);
  
  AppSettings.PlaceCardToLoad := lPath;
  Tool := TOOL_DISPLAY_RECORDCARD;
end;  // TfrmMap.SampleTypeClick

{-------------------------------------------------------------------------------
  Populates the SampleType menu
}
function TfrmMap.SampleTypeMenuItems: Integer;
var
  lCount: Integer;
  lNewMenuItem: TMenuItem;
  lNewPopupItem: TMenuItem;
  lImage: TImage;
begin
  try
    { Clear old menu items }
    for lCount := pmSampleTypes.Items.Count - 1 downto 0 do begin
      lNewPopupItem:=pmSampleTypes.Items[lCount];
      pmSampleTypes.Items.Delete(lCount);
      lNewPopupItem.Free;
      lNewMenuItem:=mnuMapAddSample.Items[lCount];
      mnuMapAddSample.Delete(lCount);
      lNewMenuItem.Free;
      FSampleTypeKeyList.Clear;
    end;
    lCount := 0;
  
    { Data required.. holds (i) shortname, (ii) bitmap index in i.l.
                      and (iii) key value }
    { Populate Key List with sample_type_keys and names
      Populate an image list with the bitmaps }
    try
      with dmFormActions.qrySampleType do begin
        Open;
        while not Eof do begin
          { Read the details to the EditableKeyList }
          if FieldByName('RECORDING_CARD').AsString <> '' then begin
            FSampleTypeKeyList.AddItem(FieldByName(PK_SAMPLE_TYPE).AsString,
                                       FieldByName('SHORT_NAME').AsString);
            lImage := TImage.Create(Self);
            lImage.Picture.Assign(FieldByName('IMAGE'));
  
            lNewMenuItem := TMenuItem.Create(Self);
            lNewMenuItem.Caption:= FSampleTypeKeyList.Items[lCount].KeyField2;
            lNewMenuItem.OnClick:= SampleTypeClick;
            lNewMenuItem.Bitmap := lImage.Picture.Bitmap;
            mnuMapAddSample.Add(lNewMenuItem);
  
            { if less menu items in the list than samples to plot, then... }
            if mnuMapAddSample.Count > FSampleTypeKeyList.Header.ItemCount then
              mnuMapAddSample.Remove(MnuMapAddSample.Items[0]);

            { Create a toolbar button of sample types }
            lNewPopupItem := TMenuItem.Create(Self);
            lNewPopupItem.Caption:= FSampleTypeKeyList.Items[lCount].KeyField2;
            lNewPopupItem.OnClick:= SampleTypeClick;
            lNewPopupItem.Bitmap := lImage.Picture.Bitmap;
            pmSampleTypes.Items.Add(lNewPopupItem);
            Inc(lCount);
          end;
          Next;
        end; // if there is a recording card
        Close;
      end; // while not eof
    except
      on EDataBaseError do begin
        Close;
        EMapBrowserError.CreateNonCritical(ResStr_DBWriteFail);
      end;
    end; // try.. except
  finally
    Result := pmSampleTypes.Items.Count;
  end;
end;  // TfrmMap.SampleTypeMenuItems 

{-------------------------------------------------------------------------------
  Saves the Map on the Clipboard to AFilePath, if path is valid  
}
procedure TfrmMap.SaveMapImage(const iFilePath: String; iGetConfirmation: Boolean);
var
  lClipboard: TClipboard;
  lBitmap: TBitmap;
  lSEExtent: Integer;
  lJPEG: TJPEGImage;
begin
  if (iFilePath = '') then
    SaveMapImageAs
  else
  { If map file path has been specified, then continue with save }
  if (FileExists(iFilePath)) and iGetConfirmation and
     (MessageDlg(ResStr_OverwriteImage, mtConfirmation,
                  [mbYes, mbNo], 0) = mrNo) then begin
    SaveMapImageAs;
  end else begin
    { If map file path is valid, then continue with save }
    lClipboard := TClipboard.Create;
    lBitmap    := TBitmap.Create;
    lSEExtent  := MakeLong(MapCanvasSize.x, MapCanvasSize.y);
  
    try
      if (MapCopyToClipboard(FMapServerLink.MapHandle, $00000000, lSEExtent,
                             PChar(''), $FFFFFF, $000000) <> MS_SUCCESS) then
        EMapBrowserError.Create(ResStr_ErrorSavingMap);
  
      lBitmap.Assign(lClipboard);
      if CompareText(ExtractFileExt(iFilePath),  '.bmp') = 0 then
        lBitmap.SaveToFile(iFilePath)
      else begin
        lJPEG := TJPEGImage.Create;
        try
          lJPEG.Assign(lBitmap);
          lJPEG.SaveToFile(iFilePath);
        finally
          lJPEG.Free;
        end; // try
      end;
    finally
      lBitmap.Free;
      lClipboard.Free;
    end; // try..finally
  end; // else (MessageConfirmation=mrYes)
end;  // TfrmMap.SaveMapImage 

{-------------------------------------------------------------------------------
  Opens Save dialog and saves the map file to the specified location as either bitmap or JPEG. 
}
procedure TfrmMap.SaveMapImageAs;
var
  lSaveDialog: TSaveDialog;
begin
  lSaveDialog := TSaveDialog.Create(nil);
  try
    lSaveDialog.Title   := ResStr_SaveMapImg;
    lSaveDialog.Filter  := 'Bitmap files (*.bmp)|*.BMP|'+
                           'JPEG files (*.jpg)|*.JPG';
    lSaveDialog.Options := [ofOverwritePrompt, ofPathMustExist];
  
    try
      if lSaveDialog.Execute then begin
        AppSettings.MapImageFilePath := lSaveDialog.FileName;
        if ExtractFileExt(AppSettings.MapImageFilePath) = '' then begin
            // no extension so add one
          if lSaveDialog.FilterIndex = 1 then
            AppSettings.MapImageFilePath := AppSettings.MapImageFilePath + '.bmp'
          else
            AppSettings.MapImageFilePath := AppSettings.MapImageFilePath + '.jpg';
        end else
        if (CompareText(ExtractFileExt(AppSettings.MapImageFilePath), '.bmp') <> 0) and
           (CompareText(ExtractFileExt(AppSettings.MapImageFilePath), '.jpg') <> 0) then
          raise EMapBrowserError.CreateNonCritical(ReStr_SavingMapFiles);
        SaveMapImage(AppSettings.MapImageFilePath, False);
            // Save the bitmap to specified location
      end; // if lSaveDialog.Execute
    except
      on Exception do begin
        AppSettings.MapImageFilePath := '';
        raise;
      end;
    end; // try..except
  finally
    lSaveDialog.Free;
  end; // try
end;  // TfrmMap.SaveMapImageAs

{-------------------------------------------------------------------------------
  Draws gridlines on the screen, via the MapUser sheet, according to the scale and spacing
      required 
}
procedure TfrmMap.ScreenDrawGridlines(GridExtents: MsExtent);
var
  Pen: HDc;
  SmallPen: HDc;
  lWest, lNorth: Double;
  lPixToOrigin: TPoint;
  lPixCurrPos: TPoint;
  lCount: Integer;
begin
  { Read the position of the screen extents and apply shift accordingly }
  lWest := GridExtents.dWest - GridOffsetX;
  lNorth:= GridExtents.dNorth - GridOffsetY;

  { Drawing the main Cross hairs }
  Smallpen := CreatePen(PS_DASHDOT, 1, GRIDLINE_COLOR);
  try
    SelectObject(FDevice, Smallpen);

    { Calculate the number of Pixels to the origin }
    lPixToOrigin.x := Round((0 - lWest) * CoordtoPixelFactor.x);
    lPixToOrigin.y := Round(lNorth * CoordtoPixelFactor.y);
    { Draw the two lines }
    MoveToEx(FDevice, lPixToOrigin.x, 0, nil);
    LineTo(FDevice, lPixToOrigin.x, MapCanvasSize.y);
    MoveToEx(FDevice, 0, lPixToOrigin.y, nil);
    LineTo(FDevice, MapCanvasSize.x, lPixToOrigin.y);
  finally
    DeleteObject(SmallPen);
  end;
  
  { Drawing Sub-Gridlines }
  Pen := CreatePen(PS_DOT, 1, GRIDLINE_COLOR);
  try
    SelectObject(FDevice, Pen);

    { while between origin and the east }
    lPixCurrPos.x := lPixToOrigin.x;
    lCount := 0;
    while lPixCurrPos.x < MapCanvasSize.x do begin
      Inc(lCount);
      lPixCurrPos.x := Round(CoordtoPixelFactor.x *
                             ((lCount * GridLineScaleX * FMapCoordsPerMetre) - lWest));
      MoveToEx(FDevice, lPixCurrPos.x, 0, nil);
      LineTo(FDevice, lPixCurrPos.x, MapCanvasSize.y);
    end;
  
    { while between origin and the west }
    lPixCurrPos.x := lPixToOrigin.x;
    lCount := 0;
    while lPixCurrPos.x > 0 do begin
      Inc(lCount);
      lPixCurrPos.x := Round(CoordToPixelFactor.x *
                             ((0 - lWest) - (lCount * GridLineScaleX * FMapCoordsPerMetre)));
      MoveToEx(FDevice, lPixCurrPos.x, 0, nil);
      LineTo(FDevice, lPixCurrPos.x, MapCanvasSize.y);
    end;

    { while between origin and the north }
    lPixCurrPos.y := lPixToOrigin.y;
    lCount := 0;
    while lPixCurrPos.y > 0 do begin
      Inc(lCount);
      lPixCurrPos.y := Round(CoordtoPixelFactor.y *
                             (lNorth - (lCount * GridLineScaleY * FMapCoordsPerMetre)));
      MoveToEx(FDevice, 0, lPixCurrPos.y, nil);
      LineTo(FDevice, MapCanvasSize.x, lPixCurrPos.y);
    end;
  
    { while between origin and the south }
    lPixCurrPos.y := lPixToOrigin.y;
    lCount := 0;
    while lPixCurrPos.y < MapCanvasSize.y do begin
      Inc(lCount);
      lPixCurrPos.y := Round(CoordtoPixelFactor.x *
                            (lNorth + (lCount * GridLineScaleY * FMapCoordsPerMetre)));
      MoveToEx(FDevice, 0, lPixCurrPos.y, nil);
      LineTo(FDevice, MapCanvasSize.x, lPixCurrPos.y);
    end;
  finally
    DeleteObject(Pen);
  end; // try..finally
end;  // TfrmMap.ScreenDrawGridlines

{-------------------------------------------------------------------------------
  To be able to convert a distance in pixels to a distance in tems of Global Map Coordinates
}
function TfrmMap.ScreenToMap(ScreenX, ScreenY: Integer): msCoord;
begin
  Result.x := (VisibleExtents.dWest + (ScreenX * PixelToCoordFactor.x));
  Result.y := (VisibleExtents.dSouth + Abs(ScreenY - MapCanvasSize.y) * PixeltoCoordFactor.y);
end;  // TfrmMap.ScreenToMap

{-------------------------------------------------------------------------------
  Sets the map so it is ready to have a bounding box drawn on it and resets properties. 
}
procedure TfrmMap.SelectArea;
begin
  { Set the BoundingBox to zero }
  ZeroBoundingBox;
  ZeroDragBox;
  Tool := TOOL_SELECT_AREA;
  FSelectingArea := True;
end;  // TfrmMap.SelectArea

//==============================================================================
// Procedure Name: SelectArea
//        Purpose: Updates the selected region of the map to the rectangle
//                 defined by the specified latitude-longitude values of its SW
//                 and NE corners. Draws a boundary onto the map to indicate
//                 the selected area, and prepares for the user to redefine or
//                 resize this selection.
//     Parameters: SW: TLatLong - The south-westerly corner of the selection.
//                 NE: TLatLong - The north-easterly corner of the selection.
//------------------------------------------------------------------------------
procedure TfrmMap.SelectArea(SW, NE: TLatLong);
begin
  FBoundingBox.SouthWest := SW;
  FBoundingBox.NorthEast := NE;

  Tool := TOOL_SELECT_AREA;
  FSelectingArea := True;

  DrawBoundingBox;
end;  // TfrmMap.SelectArea

//==============================================================================
// Function Name: NearestPointInReferenceSystem
//       Purpose: Given the coordinates of a pixel on the map panel, returns the
//                nearest valid pixel when the precision of the current spatial
//                reference system is taken into account.
//------------------------------------------------------------------------------
function TfrmMap.NearestPointInReferenceSystem(X, Y: Integer): TPoint;
var
  lMapCoord: TMapCoord;
  lLatLong: TLatLong;
  lSpatialRef: String;
begin
  lMapCoord := TMapCoord(ScreenToMap(X, Y));

  lLatLong := SpecificENToLatLong(lMapCoord, DatasetSpatialSystem);

  lSpatialRef := ConvertFromLatLong(lLatLong, AppSettings.SpatialRefSystem);
  lSpatialRef := AllowForAccuracy(lSpatialRef, AppSettings.SpatialRefSystem);
  if lSpatialRef <> ResStr_BeyondSystemLimits then
  begin
    lLatLong := ConvertToLatLong(lSpatialRef, AppSettings.SpatialRefSystem);
    lMapCoord := LatLongToSpecificEN(lLatLong, DatasetSpatialSystem);
    Result := MapToScreen(lMapCoord.x, lMapCoord.y, VisibleExtents);
  end else
    raise EMapBrowserError.CreateNonCritical(ResStr_BeyondSystemLimits);

end;

{-------------------------------------------------------------------------------
  Actually select a region by creating a copy of it in the edit buffer
}
procedure TfrmMap.SelectRegion(const APolygon: TPolygonID);
var
  lLayer: TLayerItem;
begin
  MapEditClearObject(FMapServerLink.MapHandle);
  if APolygon.ObjectID > -1 then begin
    // Find the layer by sheet ID
    lLayer := LayerBySheetID(APolygon.SheetID);
    // And refresh
    if Assigned(lLayer) and (lLayer is TPolygonLayerItem) then begin
      MapCheck(MapEditSelectObject(FMapServerLink.MapHandle,
                                   APolygon.SheetID, APolygon.ObjectID));
      MapCheck(MapEditSetColor(FMapServerLink.MapHandle,
                               TPolygonLayerItem(lLayer).SelectedColour));
      MapCheck(MapEditSetEdgeColor(FMapServerLink.MapHandle,
                                   TPolygonLayerItem(lLayer).SelectedColour));
      MapCheck(MapEditUpdate(FMapServerLink.MapHandle));
      MapCheck(MapUpdateObject(FMapServerLink.MapHandle, APolygon.SheetID, APolygon.ObjectID));
    end;
  end;
end;  // TfrmMap.SelectRegion 

{-------------------------------------------------------------------------------
  Set method for the property. Validation 
}
procedure TfrmMap.SetBoundingBox(const Value: TBoundingBox);
var
  lMessage: String;
  lSouthWestSR: String;
  lNorthEastSR: String;
begin
  { Convert to Eastings and Northings }
  try
    { Check that all four corners are valid Spatial Refs }
    if not (ValidLatLongRecord(Value.SouthWest) and
            ValidLatLongRecord(Value.NorthEast)) then
      Raise EMapBrowserError.CreateNonCritical(ResStr_InvalidBoundingBox);

    { Check that they are valid }
    try
      { Set flag so that if box is to large for system, it shrinks to fit }
      SpatialRefFuncs.tfAcceptLargerBoundingBox := True;
      try
        lSouthWestSR := ConvertFromLatLong(Value.SouthWest, Appsettings.SpatialRefSystem);
        lNorthEastSR := ConvertFromLatLong(Value.NorthEast, Appsettings.SpatialRefSystem);
      finally
        tfAcceptLargerBoundingBox := False;
      end;
    except
      on EOutOfRangeError do
      // nowt
    end;
    if (lSouthWestSR = ResStr_BeyondSystemLimits) or (lNorthEastSR = ResStr_BeyondSystemLimits)
        or Sametext(LeftStr(lSouthWestSR, Length(LAT_LONG)), LAT_LONG)
        or Sametext(LeftStr(lNorthEastSR, Length(LAT_LONG)), LAT_LONG) then
      Raise EMapBrowserError.CreateNonCritical(ResStr_BBOutsideSystem);
  except
    on EMapBrowserError do begin
      ZeroDragBox;
      raise;
    end;
    on ESpatialRefError do begin
      ZeroDragBox;
      raise EMapBrowserError.CreateNonCritical(Format(ResStr_UnrecognisedSR , [lMessage]));
    end;
  end; // try..except
  FBoundingBox.SouthWest := ConvertToLatLong(lSouthWestSR, Appsettings.SpatialRefSystem);
  FBoundingBox.NorthEast := ConvertToLatLong(lNorthEastSR, Appsettings.SpatialRefSystem);
end;  // TfrmMap.SetBoundingBox


{-------------------------------------------------------------------------------
  On changing the distribution scale the Displayed values in any active lists of TDrawItems
      must also be updated.
}
procedure TfrmMap.SetDistributionScaleX(const Value: Double);
var
  i: Integer;
begin
  if not Assigned(DatasetLegend) then Exit;

  FDistributionScaleX := Value;
  for i := 0 to DatasetLegend.Count - 1 do
    UpdatePointsDisplayed(DatasetLegend[i].DistributionPoints);
end;  // TfrmMap.SetDistributionScaleX

procedure TfrmMap.SetDistributionScaleY(const Value: Double);
var
  i: Integer;
begin
  if not Assigned(DatasetLegend) then Exit;

  FDistributionScaleY := Value;
  for i := 0 to DatasetLegend.Count - 1 do
    UpdatePointsDisplayed(DatasetLegend[i].DistributionPoints);
end;  // TfrmMap.SetDistributionScaleY

{-------------------------------------------------------------------------------
  Returns the scale of the map grid lines
}
procedure TfrmMap.SetGridLineScaleX(const Value: Double);
var
  i: Integer;
begin
  { NB: Value is what is proposed the scale is changed to }
  if Value <> 0 then begin
    if (Value * FMapCoordsPerMetre*CoordtoPixelFactor.x) < 10 then
    begin
      { Set to the best gridline scale }
      for i := 0 to pmGridLines.Items.Count - 1 do
        if (pmGridLines.Items[i].Checked) and (i <> pmGridLines.Items.Count) then
        begin
          { if largest Scale }
          if i = 0 then pmGridLines.Items[pmGridLines.Items.Count - 1].Click
                   else pmGridLines.Items[i - 1].Click;
        end; // if (pmGridLines.Items[iIndex].Checked) ...

      raise EMapBrowserError.CreateNonCritical(ResStr_InvalidGridLines);
    end else
      FGridLineScaleX := Value;
  end else begin
    // if Value <> 0
    FGridLineScaleX := Value;
    FGridActive:=False;
  end;
end;  // TfrmMap.SetGridLineScaleX

procedure TfrmMap.SetGridLineScaleY(const Value: Double);
var
  i: Integer;
begin
  { NB: Value is what is proposed the scale is changed to }
  if Value <> 0 then begin
    if (Value * FMapCoordsPerMetre*CoordtoPixelFactor.y) < 10 then
    begin
      { Set to the best gridline scale }
      for i := 0 to pmGridLines.Items.Count - 1 do
        if (pmGridLines.Items[i].Checked) and (i <> pmGridLines.Items.Count) then
        begin
          { if largest Scale }
          if i = 0 then pmGridLines.Items[pmGridLines.Items.Count - 1].Click
                   else pmGridLines.Items[i - 1].Click;
        end; // if (pmGridLines.Items[iIndex].Checked) ...

      raise EMapBrowserError.CreateNonCritical(ResStr_InvalidGridLines);
    end else
      FGridLineScaleY := Value;
  end else begin
    // if Value <> 0
    FGridLineScaleY := Value;
    FGridActive:=False;
  end;
end;  // TfrmMap.SetGridLineScaleY

{-------------------------------------------------------------------------------
}
procedure TfrmMap.SetGridOffsetX(value: Double);
begin
  FGridOffsetX := value;
end;  // TfrmMap.SetGridOffsetX

procedure TfrmMap.SetGridOffsetY(value: Double);
begin
  FGridOffsetY := value;
end;  // TfrmMap.SetGridOffsetY

{-------------------------------------------------------------------------------
  Because the actual Canvas size of the map window changes according to whether there are
      scroll
       bars or not, and the scroll bars are controlled by the map... not by me, the
      method determines the size of the map window. 
}
procedure TfrmMap.SetMapCanvasSize(const Value: TPoint);
var
  lScrollWidthX, lScrollWidthY: Integer;
  lExtents: msExtent;
begin
  lExtents := ReadMapExtents;

  { if scroll bars are showing... }
  if (Round(VisibleExtents.dWest) > Round(lExtents.dWest)) or
     (Round(VisibleExtents.dEast) < Round(lExtents.dEast)) then
    lScrollWidthY := GetSystemMetrics(SM_CYVSCROLL)
  else
    lScrollWidthY := 0;

  { vertical scroll bar }
  if (Round(VisibleExtents.dNorth) < Round(lExtents.dNorth)) or
     (Round(VisibleExtents.dSouth) > Round(lExtents.dSouth)) then
    lScrollWidthX := GetSystemMetrics(SM_CXVSCROLL)
  else
    lScrollWidthX := 0;

  FMapCanvasSize.x := pnlMapPanel.Width - lScrollWidthX;
  FMapCanvasSize.y := pnlMapPanel.Height - lScrollWidthY;
end;  // TfrmMap.SetMapCanvasSize 

{-------------------------------------------------------------------------------
  Cursors in MapServer windows are Icons - NOT the usual delphi cursors.  The cursors for the
      map have been saved to a resource file.. MapIcon.res and the MapCursor property is a 
      TMapCursor to aid familiarity with cursor types. 
  Dependencies: needs MapIcon.res file
}
procedure TfrmMap.SetMapCursor(const Value: TCursor);
var
  lCursor: HCursor;
begin
  if FMapWindowOpen then
  begin
    case Value of
      crHourGlass     : lCursor := LoadCursor(0, IDC_WAIT) ;
      crZoomIn        : lCursor := LoadCursor(HInstance, 'MAPZOOMIN');
      crZoomOut       : lCursor := LoadCursor(HInstance, 'MAPZOOMOUT');
      crCross         : lCursor := LoadCursor(0, IDC_CROSS);
      crSizeAll       : lCursor := LoadCursor(0, IDC_SIZEALL);
      crArrow         : lCursor := LoadCursor(0, IDC_ARROW);
      crFindSourceData: lCursor := LoadCursor(HInstance, 'FINDSOURCE');
      crSizeNESW      : lCursor := LoadCursor(0, IDC_SIZENESW);
      crSizeNS        : lCursor := LoadCursor(0, IDC_SIZENS);
      crSizeNWSE      : lCursor := LoadCursor(0, IDC_SIZENWSE);
      crSizeWE        : lCursor := LoadCursor(0, IDC_SIZEWE);
      0               : Exit;
    else
      lCursor := 0;
    end;
    FMapCursor := Value;
    if MapSetWindowCursor(FMapServerLink.MapHandle, lCursor) <> MS_SUCCESS then
      Raise EMapBrowserError.CreateNonCritical(ResStr_Cursor);
  end;
end;  // TfrmMap.SetMapCursor 

{-------------------------------------------------------------------------------
  A wrapper for the MapServer.dll call and error handling. When creating a new object,
  pass in values and it will commit the object and return the static ID so that it can
  be stored in the sheet's database.
  An object must have been created.
}
function TfrmMap.SetMapObjectDetails(iObjectType: Byte; iVisible: Boolean; iColor: Cardinal;
    iFillStyle: shortint; iCopyKeyValue: Boolean; iKeyValue, iBoundaryKey: pChar): Integer;
var
  lStaticID, lActualID, lDestSheetID: Integer;
begin
  MapCheck(MapEditSetObjectType(FMapServerLink.MapHandle, iObjectType));
  if iObjectType <> MS_POINT_OBJECT then begin
    MapCheck(MapEditSetColor(FMapServerLink.MapHandle, iColor));
    MapCheck(MapEditSetEdgeColor(FMapServerLink.MapHandle, iColor));
    MapCheck(MapEditSetFillPattern(FMapServerLink.MapHandle, iFillStyle));
    MapCheck(MapEditSetVisible(FMapServerLink.MapHandle, True));
  end;
  MapCheck(MapEditGetID(FMapServerLink.MapHandle, lActualID));
  MapCheck(MapEditGetSheet(FMapServerLink.MapHandle, lDestSheetID));
  MapCheck(MapEditCommitObject(FMapServerLink.MapHandle));
  MapSetSheetFillPoly(FMapServerLink.MapHandle, lDestSheetID, True);
  MapCheck(MapUpdateObject(FMapServerLink.MapHandle, lDestSheetID, lActualID));
  lStaticID := FMapServerLink.StaticIDforObject(lDestSheetID, lActualID);
  FMapServerLink.WriteToMapDatabase(MS_FIELD_STATICID, lDestSheetID, lActualID,
                                    IntToStr(lStaticID));

  Result := lStaticID;
  if iCopyKeyValue then
  begin
    { if the Object copied onto the object sheet has a key value which is to
      be copied also, then we look for this Key Value in the Location_Boundary
      table and set the Object ID in the table to that of the object which has
      just been created and assigned that key value. }
    MapCheck(MapDataSetAttribute(FMapServerLink.MapHandle, lDestSheetID,
                                 MS_FIELD_LOCATION_KEY, lActualID, iKeyValue));
    MapCheck(MapDataSetAttribute(FMapServerLink.MapHandle, lDestSheetID,
                                 MS_FIELD_LOCATION_BOUNDARY_KEY, lActualID, iBoundaryKey));
    RelinkToBoundaryRecord(lDestSheetID, lActualID);
  end;
end;  // TfrmMap.SetMapObjectDetails

{-------------------------------------------------------------------------------
  Checks that the new scale is valid (the minimum scale is limited to 1:100 and the outer
      scale is limited by the size of the dataset in pixels on the screen.
  
  Dependencies: Map dataset has to be open, MapUser sheet has to be created each time the map 
      is opened.
  Side Effects: Calls SetGridLineScale so that we can also check that, if gridlines are 
      displayed then they are validated and not drawn if they would be too close together.
}
procedure TfrmMap.SetMapScale(const Value: Double);
var
  lScale, lNewScale: Double;
  lExtents: msExtent;
  lMaxZoom: Double;
begin
  { Get the current scale }
  MapCheck(MapGetScale(FMapServerLink.MapHandle, lScale));
  lExtents := ReadMapExtents;
  
  { if not zooming to extents...  }
  if Value <> 0 then begin
    { Don't allow zooming if the scale is < 1:100  }
    lMaxZoom := GetMaxZoomScale;
    if (lScale <= lMaxZoom) and (Tool = TOOL_ZOOM_IN) then begin
      RefreshMap;
      raise EMapBrowserError.CreateNonCritical(ResStr_ScaleBounds);
    end else
    { if either the horiz or vertical size of the
      dataset < 10 pixels, do not allow a zoom out }
    if ((Tool = TOOL_ZOOM_OUT) and
       (((CoordToPixelFactor.y *(lExtents.dNorth - lExtents.dSouth)) < 10) or
       ((CoordToPixelFactor.x * (lExtents.dEast - lExtents.dWest)) < 10))) then
    begin
      RefreshMap;
      Application.Processmessages;
      raise EMapBrowserError.CreateNonCritical(ResStr_ScaleBounds);
    end;
  end; // if Value <> 0
  { Rescale the map accordingly }

  MapCheck(MapSetScale(FMapServerLink.MapHandle, Value));

  { Get the new scale }
  MapCheck(MapGetScale(FMapServerLink.MapHandle, lNewScale));
  VisibleExtents := FVisibleExtents;
  FMapScale := lNewScale;
  SetGridLineScaleX(FGridLineScaleX);
  SetGridLineScaleY(FGridLineScaleY);
end;  // TfrmMap.SetMapScale

{-------------------------------------------------------------------------------
  Handles the drawing of the screen Gridlines and all the dragged and dropped samples,
      biotopes etc
  Notes: sUserData is a record which has various parts
  hMapWnd         = handle to the MapServer map window
  hdc             = device context of Mapserver map window
  sViewRect       =
  sUpdateRect     =
  sUserUpdateRect =
}
procedure TfrmMap.SetMapUserData(const Value: psUserData);
var
  lTempExtent: MsExtent;
  i: Integer;
  graySquares: TStringList;
begin
  { Point FDevice to the HDc }
  FDevice := Value.HDc;

  { if not printing then call the methods to redraw the  MapUser details,
    sending the current exents to the appropriate function.
    if Printing then do not draw the MapUser stuff because otherwise it
    clutters up the visual print }
  if not FPrinting then begin
    { Determine how much can be seen on the window }
    with Value.sViewExtent do
      if (lTempExtent.dNorth <> VisibleExtents.dNorth) or
         (lTempExtent.dSouth <> VisibleExtents.dSouth) or
         (lTempExtent.dEast <> VisibleExtents.dEast) or
         (lTempExtent.dWest <> VisibleExtents.dWest) then
        VisibleExtents := Value.sViewExtent;

    { Draw Gridlines }
    if FGridActive then
      ScreenDrawGridlines (Value.sViewExtent);

    { In case of survey data }
    FPointsDrawn := False;
    graySquares := TStringList.Create;
    try
      for i := 0 to DatasetLegend.Count - 1 do
        if DatasetLegend[i].Visible then
          DrawDistributionPoints(Value.sViewExtent, DatasetLegend[i], graySquares);
    finally
      graySquares.Free;
    end;
    actClear.Enabled := FPointsDrawn;
  end; // if FPrinting = False
  FMapUserData := Value;
end;  // TfrmMap.SetMapUserData

{-------------------------------------------------------------------------------
  Sets the MouseDownMapPos property
}
procedure TfrmMap.SetMouseDownPixPos(const Value: TPoint);
begin
  MouseDownMapPos := QueryMapPos;
  FMouseDownPixPos := Value;
end;  // TfrmMap.SetMouseDownPixPos 

{-------------------------------------------------------------------------------
  Sets the MouseUpMapPos property
}
procedure TfrmMap.SetMouseUpPixPos(const Value: TPoint);
begin
  MouseUpMapPos := QueryMapPos;
  FMouseUpPixPos := Value;
end;  // TfrmMap.SetMouseUpPixPos

{-------------------------------------------------------------------------------
}
procedure TfrmMap.SetSampleTypeKeyValue(const Value: TKeyString);
begin
  FSampleTypeKeyValue := Value;
end;  // TfrmMap.SetSampleTypeKeyValue 

{-------------------------------------------------------------------------------
  Where necesssary colors a region green to show it is selected and colors the previously selected one red again This now achieves this by copying the polygon onto the Edit buffer of Map Server and using this as a temporary polygon.  this way it doesn't reappear on other machines.
}
procedure TfrmMap.SetSelectedRegion(const APolygonID: TPolygonID);

  { Set the enabled state of items affected by selected item and system security }
  procedure SetEnabledState(ADeletePoly, AAssocBoundary, AMovePoly: Boolean);
  begin
    actDeletePolygon.Enabled     := ADeletePoly;
    actAssociateBoundary.Enabled := AAssocBoundary;
    actMovePolygon.Enabled       := AMovePoly;
  
    if actCancelSubtract.Visible then
      actSubtractBoundary.Enabled := ADeletePoly;
  end;

begin
  try
    if Tool in [TOOL_POINTER, TOOL_SUBTRACT_BOUNDARY] then
      SelectRegion(APolygonID);

    { Update previosly selected object, but ignore errors if this fails.  This will
       be because the sheet orders have been changed so the selection is invalid }
    if (FSelectedRegion.SheetID <> -1) and (FSelectedRegion.ObjectID <> -1) then
      MapUpdateObject(FMapServerLink.MapHandle,
                      FSelectedRegion.SheetID, FSelectedRegion.ObjectID);

    FSelectedRegion := APolygonID;
    eLocation.Text  := ObjectDetails.ObjectName;
    case AppSettings.UserAccessLevel of
      ualAddOnly:  SetEnabledState(False,
                                   FSelectedRegion.ObjectID <> -1,
                                   FSelectedRegion.ObjectID <> -1);
      ualFullUser,
      ualAdmin:    SetEnabledState(FSelectedRegion.ObjectID <> -1,
                                   FSelectedRegion.ObjectID <> -1,
                                   FSelectedRegion.ObjectID <> -1);
    else // read only
      SetEnabledState(False, False, False);
    end; // case

    // Enable Locations report if we have a location selected
    dmFormActions.actPlacesForOccurrencesReport.Enabled :=
        (ObjectDetails.ObjectLocationKey <> '') and not ObjectDetails.IsAdminArea;
    dmFormActions.actOccurrencesForPlacesReport.Enabled :=
        (ObjectDetails.ObjectLocationKey <> '') and not ObjectDetails.IsAdminArea;
  except
    on E:Exception do
      raise EMapBrowserError.Create(ResStr_ErrorCallSetSelectedRegion, E);
  end; // try
end;  // TfrmMap.SetSelectedRegion

{-------------------------------------------------------------------------------
  Accessor method for ShownHint  
}
procedure TfrmMap.SetShownHint(const Value: Boolean);
begin
  FShownHint := Value;
end;  // TfrmMap.SetShownHint

{-------------------------------------------------------------------------------
  Makes a note of the boundary to be subtracted from. 
}
procedure TfrmMap.SetSuperBoundary;
begin
  if ProceedWithSubtraction(True) then begin
    actSubtractBoundary.Caption := ResStr_Cap_SubtractBoundClickable;
    actSubtractBoundary.Hint    := ResStr_SubtractBound;
    Tool                        := TOOL_SUBTRACT_BOUNDARY;
    FSuperBoundaryID            := SelectedRegion;
    MessageDlg(ResStr_SelectBoundaryToDeleteFrom,
        mtInformation, [mbOK], 0)
  end;
end;  // TfrmMap.SetSuperBoundary 

{-------------------------------------------------------------------------------
  Sets the status of toolbar buttons 
  Enables and disables pop-up menus 
  Sets the MapCursor property 
}
procedure TfrmMap.SetTool(const Value: Byte);
var
  i: Integer;
begin
  try
    { if continuing to draw, that button is still active }
    if (Tool <> Value) and (Value <> TOOL_SELECT_AREA) and
       (Tool = TOOL_SELECT_AREA) and (FSelectingArea = True) then
    begin
      MessageDlg(ResStr_SelectingLocationData, mtInformation, [mbOK], 0);
      ZeroDragBox;

      RefreshMap;
    end;

    { If the value of FTool has changed... }
    if (FTool <> Value) and (Value <> TOOL_ZOOM_TO_EXT) then
    begin
      { If we were drawing a polygon, then get rid of any unfinished drawing }
      if FTool = TOOL_DRAW_POLY then CancelDrawing;

      { Disable the menu items and popups }
      EnableDrawMenu(False);

      { Set the tool to the new value }
      FTool := Value;

      { Change Cursor according to the new tool and perform any specific operations }
      case Value of
        TOOL_POINTER:
            begin
              MapCursor := crArrow;
              if FSelectingArea then
              begin
                Messagedlg(ResStr_CanReturnMapData, mtInformation, [mbOK], 0);
                //ZeroBoundingBox;
                MapCursor := crCross;
                FTool := TOOL_SELECT_AREA;
              end;
            end;
        TOOL_SELECT_AREA       : MapCursor := crCross;
        TOOL_ZOOM_IN           : MapCursor := crZoomIn;
        TOOL_ZOOM_OUT          : MapCursor := crZoomOut;
        TOOL_PAN               : MapCursor := crSizeAll;
        TOOL_DISPLAY_RECORDCARD: MapCursor := crCross;
        TOOL_FIND_SOURCE_DATA  : MapCursor := crFindSourceData;
        TOOL_DRAW_POLY:
            begin
              MapCursor   := crCross;
              FFirstCoord := True;
            end;
        TOOL_SUBTRACT_BOUNDARY : MapCursor := crArrow;
      end; // case
      actSubtractBoundaryToolReset(Value);
    end; // if FTool <> Value

    { Uncheck all the tool actions if the selected tool is to be checked. }
    case Value of
      TOOL_POINTER, TOOL_SELECT_AREA, TOOL_ZOOM_IN, TOOL_ZOOM_OUT, TOOL_PAN,
      TOOL_FIND_SOURCE_DATA, TOOL_DRAW_POLY, TOOL_SUBTRACT_BOUNDARY :
          begin
            for i := 0 to alMaptools.ActionCount -1 do
              if alMaptools.Actions[i] is TCustomAction then
                TCustomAction(alMaptools.Actions[i]).Checked := False;
            // uncheck the top level button/menu item that contains the drawing action
            DepressPolygonDropdown(False);
            // Check the specific selected tool's action
            case Value of
              TOOL_POINTER, TOOL_SELECT_AREA: actPointer.Checked        := True;
              TOOL_ZOOM_IN                  : actZoom.Checked           := True;
              TOOL_ZOOM_OUT                 : actUnZoom.Checked         := True;
              TOOL_PAN                      : actPan.Checked            := True;
              TOOL_FIND_SOURCE_DATA         : actFindSourceData.Checked := True;
              TOOL_DRAW_POLY                :
                  begin
                    actDraw.Checked := True;
                    DepressPolygonDropdown(True);
                  end;
            end; // case
          end;
    end; // case
  except
    on E:Exception do
      raise EMapBrowserError.Create(ResStr_SetToolError, E);
  end; // try
end;  // TfrmMap.SetTool 

{-------------------------------------------------------------------------------
  These are not property set methods but are called when AppSettings.Polygon*SelectedColour is 
      changed  
}
procedure TfrmMap.SetUnselectedPolygonColour(ALayer: TPolygonLayerItem; ADoColour, ADoPattern:
    Boolean);
var
  i: Integer;
  lRememberedSelectedRegion: TPolygonID;
  lSheetID: Integer;
  lObjectType: uChar;
  lCursor: TCursor;
begin
  // Don't change unless necessary -  this is slow across a network
  if ALayer.Visible then begin
    LockWindowUpdate(0);
    // Unlock updates, because by default we lock during construction of form
    lCursor := HourglassCursor;
    frmMain.SetStatus(ResStr_UpdatingPolygonColours);
    try
      frmMain.ProgressBar.TaskPosition := 0;
      lSheetID := FMapServerLink.SheetIDByFileName(ALayer.FileName);
      lRememberedSelectedRegion := SelectedRegion;
      SelectedRegion := PolygonID(0, -1);

      for i := 0 to FMapServerLink.ObjectTotal[lSheetID] - 1 do begin
        if ADoColour or ADoPattern then  // update the objecttype if color or patten changes
          MapCheck(MapGetObjectType(FMapServerLink.MapHandle, lSheetID, i, lObjectType));
          if lObjectType <> MS_POINT_OBJECT then
            MapCheck(MapSetObjectColor(FMapServerLink.MapHandle,
                                     lSheetID, i, ALayer.UnselectedColour));

        // Only polygons need fill pattern and edge colour
        if lObjectType = MS_REGION_OBJECT then begin
          if ADoColour then
            MapCheck(MapSetObjectEdgeColor(FMapServerLink.MapHandle,
                                           lSheetID, i, ALayer.UnselectedColour));
          if ADoPattern then
            MapCheck(MapSetObjectFillPattern(FMapServerLink.MapHandle,
                                             lSheetID, i, ALayer.MSPattern));
        end;
        frmMain.ProgressBar.TaskPosition := i * 100 div
                                            FMapServerLink.ObjectTotal[lSheetID];
      end;
      // Make sure the selected region is once again selected
      SelectedRegion := lRememberedSelectedRegion;
    finally
      frmMain.SetStatus('');
      frmMain.ProgressBar.TaskPosition := 0;
      DefaultCursor(lCursor);
    end; // try
  end; // if visible
end;  // TfrmMap.SetUnselectedPolygonColour

{-------------------------------------------------------------------------------
  Registering which controls can have drag / drop events and defining the details and methods 
      to call 
}
procedure TfrmMap.SetupDestinationControls;
var
  i, j, lTotal: Integer;
  lAddin: IMapDropFormat;
  lTableNames: Array of String;
begin
  // Gather all table names of data allowed to be dropped on the map, addins included.
  SetLength(lTableNames, 5);
  lTableNames[0] := TN_SURVEY;
  lTableNames[1] := TN_SURVEY_EVENT;
  lTableNames[2] := TN_TAXON_LIST_ITEM;
  lTableNames[3] := TN_BIOTOPE_LIST_ITEM;
  lTableNames[4] := MIXED_DATA;
  lTotal := 5;
  FAddinTables.Clear;
  with AppSettings.ComAddins.MapDropFormatAddins do
    for i := 0 to Count - 1 do begin
      // We know the COM supports this interface, that's why it's in thist list.
      lAddin := CreateComObject(StringToGUID(Strings[i])) as IMapDropFormat;

      // Allocate extra room in the array now.
      SetLength(lTableNames, lTotal + lAddin.SupportedTableCount);
      for j := 0 to lAddin.SupportedTableCount - 1 do begin
        // Don't overwrite the names already in there.
        lTableNames[lTotal + j] := lAddin.SupportedTable[j];

        // Save the table name and the addin to call when data dropped.
        // But only save the first one. First come first served!
        if FAddinTables.IndexOfName(lAddin.SupportedTable[j]) = -1 then
          FAddinTables.Add(lAddin.SupportedTable[j] + '=' + Strings[i]);
      end;
      // Remember where the end is.
      Inc(lTotal, lAddin.SupportedTableCount);
    end;

  RegisterDropComponent(pnlMapPanel, MapDataDropped, lTableNames, [CF_JNCCDATA]);
  { Can also drag to legend }
  RegisterDropComponent(sgDatasets, MapDataDropped, lTableNames, [CF_JNCCDATA]);

  RegisterDragComponent(pnlDrag, GetLocationKey);
  // Register the labels so that drag works even over them.
  RegisterDragComponent(staticSpatialRef, GetLocationKey);
  RegisterDragComponent(staticLocation, GetLocationKey);
  RegisterCopyPasteComponent(eSpatialRef);
end;  // TfrmMap.SetupDestinationControls 

{-------------------------------------------------------------------------------
  Selectively disables functionality so that user access is limitted.
}
procedure TfrmMap.ApplySecurity;
var
  i: Integer;
begin
  case AppSettings.UserAccessLevel of
    ualReadOnly :
        begin
          mnuMapAddBoundary.Enabled    := False;
          actDeletePolygon.Enabled     := False;
          actAssociateBoundary.Enabled := False;
          actSubtractBoundary.Enabled  := False;
          mnuMapAddSample.Enabled      := False;
          mnuMapAddBoundary.Enabled    := False;
          actAddPolygonLayer.Enabled   := False;
        end;
    ualRecorder: // as above but can add sample
        begin
          mnuMapAddBoundary.Enabled    := False;
          actDeletePolygon.Enabled     := False;
          actAssociateBoundary.Enabled := False;
          actSubtractBoundary.Enabled  := False;
          mnuMapAddBoundary.Enabled    := False;
          mnuMapAddSample.Enabled      := SampleTypeMenuItems > 0;
          actAddPolygonLayer.Enabled   := False;
        end;
    ualAddOnly :
        begin
          actDeletePolygon.Enabled     := False;
          actAssociateBoundary.Enabled := (FSelectedRegion.ObjectID <> -1);
          actSubtractBoundary.Enabled  := False;
          mnuMapAddBoundary.Enabled    := False;
          mnuMapAddSample.Enabled      := SampleTypeMenuItems > 0;
          actAddPolygonLayer.Enabled   := False;
        end;
    else
        begin
          actDeletePolygon.Enabled     := (FSelectedRegion.ObjectID <> -1);
          actAssociateBoundary.Enabled := (FSelectedRegion.ObjectID <> -1);
          actSubtractBoundary.Enabled  := (FSelectedRegion.ObjectID <> -1) or
                                          (not actCancelSubtract.Visible);
          mnuMapAddSample.Enabled      := SampleTypeMenuItems > 0;
          actAddPolygonLayer.Enabled   := True;
        end;
  end; // case
  
  { Make the three edit menu options behave }
  If (Tool <> TOOL_DRAW_POLY) then EnableDrawMenu(False);

  { Make the tool buttons follow the Menu items }
  with frmMain.tbContext do begin
    for i := 3 to mnuChildMap.Count - 1 do
      if i - 3 < ButtonCount then
        { skip buttons with actions - they are handled }
        if Buttons[i - 3].Action = nil then
          Buttons[i - 3].Enabled := mnuChildMap.Items[i].Enabled;

    // Last 3 items of the map menu need to be handled differently, due to mismatch of indexes.
    for i := 0 to ButtonCount - 1 do
      if Buttons[i].DropdownMenu = pmDistributionPoints then
        Buttons[i].Enabled := mnuMapDistributionPoints.Enabled
      else
      if Buttons[i].DropdownMenu = pmGridLines then
        Buttons[i].Enabled := mnuMapGridLines.Enabled
      else
      if Buttons[i].DropdownMenu = pmSampleTypes then
        Buttons[i].Enabled := mnuMapAddSample.Enabled;
  end;
end;  // TfrmMap.ApplySecurity 

{-------------------------------------------------------------------------------
  Set method for the VisibleExtents property.  A wrapper for the call to determine the size of 
      the current dataset and a call to adjust the CanvasSize property accordingly.
}
procedure TfrmMap.SetVisibleExtents(const Value: MsExtent);
var
  psExtent: msExtent;
begin
  MapCheck(MapGetVisibleExtent(FMapServerLink.MapHandle, psExtent),
           ResStr_CannotEstablishVisibleExtents);
  FVisibleExtents := psExtent;
  { Changing the Visible Extents may change the MapCanvasSize }
  MapCanvasSize := FMapCanvasSize;
end;  // TfrmMap.SetVisibleExtents 

{-------------------------------------------------------------------------------
  Creates a new Edit object or adds a coordinate to the existing one.
}
procedure TfrmMap.StartDrawPoly;
var
  lNumCoords: Integer;
  lCoord: msCoord;
begin
  if CurrentDrawingLayer = nil then Exit;  // can't draw as no layer available

  SelectedRegion := PolygonID(0, -1);
  lCoord := MouseDownMapPos;
  if FFirstCoord = True then begin
    { if this is the First coordinate then create the object }
    FFirstCoord := False;  { No longer the First co-ordinate }

    CreateNewMapObject(MS_LINE_OBJECT);

    { Use the current layer to select the initial line colour whilst drawing }
    MapCheck(MapEditSetColor(FMapServerLink.MapHandle,
                             TPolygonLayerItem(CurrentDrawingLayer).SelectedColour));

    { Set object to be visible }
    MapCheck(MapEditSetVisible(FMapServerLink.MapHandle, True));

    FFirstCoord := False;

    { Enable the relevant popup menu and menu options }
    EnableDrawMenu(True);

    { Duplicate the first point so we don't get random lines - Map Server bug }
    MapCheck(MapEditInsertObjectCoord(FMapServerLink.MapHandle, MS_LAST_COORD, lCoord));
    FDuplicatePresent := True;
  end else
  // FFirstCoord = False - must remove duplicate point if it exists
  if FDuplicatePresent then begin
    MapCheck(MapEditDeleteObjectCoord(FMapServerLink.MapHandle, 0));
    FDuplicatePresent := False;
  end;

  { Add new co-ordinate to the EditObject }
  MapCheck(MapEditInsertObjectCoord(FMapServerLink.MapHandle, MS_LAST_COORD, lCoord));
  { Update object in Edit store to display any changes }
  MapCheck(MapEditUpdate(FMapServerLink.MapHandle));
  MapCheck(MapEditGetNumCoords(FMapServerLink.MapHandle, lNumCoords));
  { Set enabled states for Draw polygon/line menu items }
  actFinishLineCurrent.Enabled      := (lNumCoords > 1) and (not FDuplicatePresent);
  mnuEditFinishLineAny.Enabled      := (lNumCoords > 1) and (not FDuplicatePresent);
  pmDrawingFinishLineAny.Enabled    := (lNumCoords > 1) and (not FDuplicatePresent);
  actFinishPolygonCurrent.Enabled   := lNumCoords > 2;
  mnuEditFinishPolygonAny.Enabled   := lNumCoords > 2;
  pmDrawingFinishPolygonAny.Enabled := lNumCoords > 2;
end;  // TfrmMap.StartDrawPoly

{-------------------------------------------------------------------------------
  Starts the panning by storing the start position.
}
procedure TfrmMap.StartPan;
begin
  MapCheck(MapGetViewOrigin(FMapServerLink.MapHandle, FOriginStartPan.X, FOriginStartPan.Y));
  ShownHint := True; // turn off impending location hint, as it will now be wrong!
end;  // TfrmMap.StartPan 

{-------------------------------------------------------------------------------
  Subtracts the selected boundary from the super-boundary if it is OK to do so. 
}
procedure TfrmMap.SubtractBoundary;
var
  lSubPoly: TSubMapPolygon;
  lSuperPoly: TSuperMapPolygon;
  lCursor: TCursor;
begin
  if (FSuperBoundaryID.SheetID = SelectedRegion.SheetID) and
     (FSuperBoundaryID.ObjectID = SelectedRegion.ObjectID) then
    MessageDlg(ResStr_SelectBoundaryToHole, mtInformation, [mbOK], 0)
  else begin
    lSubPoly := TSubMapPolygon.CreateFromObjectID(FMapServerLink.MapHandle,
                                                  SelectedRegion.SheetID,
                                                  SelectedRegion.ObjectID,
                                                  AppSettings.SpatialRefSystem);
    try
      lSuperPoly := TSuperMapPolygon.CreateFromObjectID(FMapServerLink.MapHandle,
                                                        FSuperBoundaryID.SheetID,
                                                        FSuperBoundaryID.ObjectID,
                                                        AppSettings.SpatialRefSystem);
      try
        if lSuperPoly.FullyContains(lSubPoly) and ProceedWithSubtraction(False) then begin
          lCursor := HourglassCursor;
          try
            lSuperPoly.Subtract(lSubPoly);
          finally
            DefaultCursor(lCursor);
          end;
          SelectedRegion := FSuperBoundaryID;

          // Force changes to be written to map dataset file.
          FMapServerLink.SaveChanges(BaseMapKey);
        end else
          MessageDlg(ResStr_BoundaryFullyContained, mtInformation, [mbOK], 0);
      finally
        lSuperPoly.Free
      end;
    finally
      lSubPoly.Free
    end;
  end;
end;  // TfrmMap.SubtractBoundary 

{-------------------------------------------------------------------------------
  When dropping a TKeyList onto the map browser, if the table name is "SURVEY" then this 
      offers two options, to display the survey events or to display survey samples.  
      Selection of the former populates FEventList with TMapDrawItems while selection of the 
      latter populates FSampleList also with the details of samples included in the survey
      events.
  Dependencies: FMapHandle, FdmMap must exist
  Side Effects: WMU_MAPUSERDRAW triggered and the map will redraw. 
}
procedure TfrmMap.SurveyDropped(ASourceData: TKeyList);
var
  i: Integer;
  lKeys: String;
  lRS: _Recordset;
begin
  // FSurveyList is an editable KeyList which holds the data passed in.
  FSurveyList.Assign(ASourceData);

  lKeys := '''' + FSurveyList.Items[0].KeyField1 + '''';
  for i := 1 to FSurveyList.Header.ItemCount -1 do
    lKeys := lKeys + ',''' + FSurveyList.Items[i].KeyField1 + '''';

  with TdlgSurveyDataOptions.Create(nil) do
    try
      if ShowModal = mrOK then begin
        // From the dialog, create the appropriate list of TDrawItem Objects.
        case rgOptions.ItemIndex of
          { Plot survey events - Populate the EventList TList with the
            details required to draw the distribution points }
          0: begin
               lRS := dmDatabase.ExecuteSQL('SELECT * FROM Survey_Event WHERE Spatial_Ref<>'''' ' +
                                            'AND Survey_Key IN (' + lKeys + ')', True);
               if CreateDistributionList(nil, lRS, nil, 'Events', TN_SURVEY_EVENT, PK_SURVEY_EVENT) = nil then
                 MessageDlg(ResStr_NoRecordings, mtInformation, [mbOk], 0);
             end;

          { Plot samples - Populate the SampleList TList with the
            details required to draw the distribution points }
          1: begin
               lRS := dmDatabase.ExecuteSQL('SELECT S.* FROM Sample S INNER JOIN Survey_Event SE ' +
                                            'ON S.Survey_Event_Key = SE.Survey_Event_Key ' +
                                            'WHERE S.Spatial_Ref <> '''' ' +
                                            'AND SE.Survey_Key IN (' + lKeys + ')', True);
               if CreateDistributionList(nil, lRS, nil, 'Samples', TN_SAMPLE, PK_SAMPLE) = nil then
                 MessageDlg(ResStr_NoRecordings, mtInformation, [mbOk], 0);
             end;
        end;
      end;
    finally
      Free;
    end;
end;  // TfrmMap.SurveyDropped 

{-------------------------------------------------------------------------------
  On dropping a taxon onto the map, this calls methods to create an FEventList and an 
      FSampleList of Event Keys dropped and of the samples contained within those events
  Dependencies: FMapHandle, FMapServerLink.dmMap must exist
  Side Effects: Creates TMapDrawItems and populates the taxon list with them
  Special Notes: Triggers a WMU_MAPUSER message and redraws the map. 
}
procedure TfrmMap.TaxonDropped(ASourceData: TKeyList);
var
  i: Integer;
  lWantSubTaxa: Boolean;
  lTitle: String;
  lTableName: String;
  lDataItem: TDatasetItem;
  lRS: _Recordset;
begin
  // Initialise a few things
  lDataItem := nil;
  // Ask about taxon groups
  lWantSubTaxa := dmGeneralData.IncludeSubTaxa; // ask if children of taxa should be included
  if lWantSubTaxa then lTableName := TN_TAXON_GROUP
                  else lTableName := TN_TAXON_LIST_ITEM;
  // Work out the title to use
  lTitle := '';
  if AppSettings.DisplayTaxonCommonNames then
    lTitle := dmGeneralData.GetCommonName(ASourceData.Items[0].KeyField1);
  // either not displaying common names, or it doesn't have one
  if lTitle = '' then
    lTitle := dmGeneralData.GetTaxonName(ASourceData.Items[0].KeyField1);
  { This never happens in Rec2K, unless an addin or external entity were to provide the
      keylist }
  if ASourceData.Header.ItemCount > 1  then // more than one taxa dropped
    lTitle := lTitle + ResStr_AndOthers;

  // Assume nothing is going to be plotted.
  for i := 0 to ASourceData.Header.ItemCount -1 do begin
    lRS := GetTaxonOccurrences(ASourceData.Items[i].KeyField1, lWantSubTaxa);
    // Keep adding to same DatasetItem
    lDataItem := CreateDistributionList(lDataItem, lRS, nil,
                                        lTitle, lTableName, PK_TAXON_LIST_ITEM);
  end;
  // If DatasetItem is still nil here, nothing was found, so display message.
  if lDataItem = nil then
    MessageDlg(ResStr_NoRecordings, mtInformation, [mbOk], 0);
end;  // TfrmMap.TaxonDropped

function TfrmMap.GetTaxonOccurrences(ATLIKey: string; AWantSubTaxa: boolean): _Recordset;
var
  lSQL, lSQLGroup: string;
begin
  if AppSettings.UseRecommendedTaxaNames then begin
    lSQL := SQL_MAP_TAXA_LIST_NAMESERVER;
    lSQLGroup := SQL_MAP_TAXA_CONTAINED_JOIN_NAMESERVER;
  end else
  begin
    lSQL := SQL_MAP_TAXA_LIST;
    lSQLGroup := SQL_MAP_TAXA_CONTAINED_JOIN;
  end;
  if AWantSubTaxa then
    Result := dmDatabase.ExecuteSQL(Format(lSQL,
        [AppSettings.UserID, lSQLGroup, 'JG', ATLIKey]), True)
  else
    Result := dmDatabase.ExecuteSQL(Format(lSQL,
        [AppSettings.UserID, '', 'J', ATLIKey]), True);
end;

{-------------------------------------------------------------------------------
  This function creates a string containing the lat long coordinates of 3 points which are at
      the points (0,0), (0,3) and (3,0) on the map to indicate to what accuracy the spatial
      references should be calculated.
}
function TfrmMap.ThreePoints: String;
var
  lTopLeft, lTopRight, lBottomLeft: TLatLong;
begin
  lTopLeft := SpecificENToLatLong(TMapCoord(ScreenToMap(0, 0)), DatasetSpatialSystem);

  lTopRight := SpecificENToLatLong(TMapCoord(ScreenToMap(0, NUM_ACCURACY_PIX)),
                                   DatasetSpatialSystem);
  
  lBottomLeft := SpecificENToLatLong(TMapCoord(ScreenToMap(NUM_ACCURACY_PIX,0)),
                                     DatasetSpatialSystem);
  
  Result := FloatToStr(lTopLeft.Lat) + ListSeparator +
            FloatToStr(lTopLeft.Long) + ListSeparator +
            FloatToStr(lTopRight.Lat) + ListSeparator +
            FloatToStr(lTopRight.Long) + ListSeparator +
            FloatToStr(lBottomLeft.Lat) + ListSeparator +
            FloatToStr(lBottomleft.Long);
end;  // TfrmMap.ThreePoints

{-------------------------------------------------------------------------------
  Detects if the number of polygons on any of the map sheets has changed.
}
procedure TfrmMap.tmRefreshTimer(Sender: TObject);
var
  lSheetID: Integer;
  i: Integer;
  lLayer: TPolygonLayerItem;
begin
  inherited;
  // Don't interfere with drawing, or dragging. Try again on next timer tick.
  if (Tool in [TOOL_DRAW_POLY, TOOL_SUBTRACT_BOUNDARY, TOOL_DEL_POLY]) or
     (GetAsyncKeyState(VK_LBUTTON) and $8000 <> 0) then
    Exit;

  // Find out if layers have been either added or removed by another user.
  with dmDatabase.ExecuteSQL('SELECT Count(*) AS Total FROM Map_Sheet ' +
                             'WHERE (Computer_ID = Host_Name() OR Sheet_Type = 3) ' +
                             'AND Base_Map_Key = ''' + BaseMapKey + '''', True) do
    if Fields['Total'].Value <> FLayerLegend.Count then begin
      // Get the latest dataset, with the changes.
      FMapServerLink.ReActivateDataset;
      FLayerLegend.Refresh;
      ActivePolygonLayersChange;
    end;

  // Find out if there is anything new from last time the refresh was requested.
  // And do it if something actually happened, but only if.
  if not dmDatabase.ExecuteSQL(Format(SQL_CHANGED_MAPSHEETS,
             [BaseMapKey, FormatDateTime('yyyy-mm-dd hh:mm:ss', FLastSheetRefresh),
              FormatDateTime('yyyy-mm-dd hh:mm:ss', FLastSheetRefresh)]), True).Eof then
  begin
    // Get the latest dataset, with the changes.
    FMapServerLink.ReActivateDataset;

    with FLayerLegend do
      for i := 0 to Count - 1 do begin
        lSheetID := FMapServerLink.SheetIDByFileName(LegendItems[i].Filename);
        if (LegendItems[i] is TPolygonLayerItem) and (lSheetID <> -1) then begin
          lLayer := TPolygonLayerItem(LegendItems[i]);
          lLayer.LastNumObjects := FMapServerLink.ObjectTotal[lSheetID];
        end;
      end;

    RefreshMapSheets;
    Application.ProcessMessages;
  end;
end;  // TfrmMap.tmRefreshTimer

{-------------------------------------------------------------------------------
}
procedure TfrmMap.UpdateMapWindowSelector;
var
  i: Integer;
  lSelected: String;
begin
  AppSettings.UpdateMapMenu(Self, mnuMapWindow);
  cmbActiveMap.Clear;

  with AppSettings.AvailableMaps do
    for i := 0 to Count - 1 do begin
      cmbActiveMap.Items.AddObject(Items[i].DisplayName, Items[i]);
      if BaseMapKey = Items[i].BaseMapKey then
        lSelected := Items[i].DisplayName;
    end;

  with cmbActiveMap do begin
    ItemIndex := Items.IndexOf(lSelected);
    if ItemIndex = -1 then ItemIndex := 0;
    Caption := ResStr_MapWindow+ ' - ' + Text;
  end;

  pnlMapSelector.Visible := AppSettings.AvailableMaps.Count > 1;
end;  // TfrmMap.UpdateMapWindowSelector

{-------------------------------------------------------------------------------
  Updates the status of the Distribution Point and Gridline Menus
}
procedure TfrmMap.UpdateMenus;
var
  i: Integer;
begin
  for i := 0 to mnuMapDistributionPoints.Count-1 do
    mnuMapDistributionPoints.Items[i].Checked := pmDistributionPoints.Items[i].Checked;
  for i := 0 to mnuMapGridLines.Count-1 do
    mnuMapGridLines.Items[i].Checked := pmGridLines.Items[i].Checked;
end;  // TfrmMap.UpdateMenus

{-------------------------------------------------------------------------------
  Having dragged and dropped survey data onto a distribution map, the visibility of this data
      is dependant on the degree of accuracy that the recordings were made with.  Therefore,
      whether a point will be displayed or not varies with the distribution scale.
  Points to be displayed are stored in TLists as TMapDrawItems.  The 'Displayed' status of
      which is updated with a change in the DistributionScale, so that, if there are many, the
      calculations do not have to be made each time the map is manipulated and the points draw
      onto the MapUser sheet.
}
procedure TfrmMap.UpdatePointsDisplayed(AList: TObjectList);
var
  i: Integer;
begin
  for i := 0 to AList.Count - 1 do
    with TMapDrawItem(AList[i]) do
      if Accuracy = -1 then
        Displayed := PointDisplayedStatus(SpatialRef, SpatialSys)
      else
        Displayed := ((Accuracy <= DistributionScaleX) and (Accuracy <= DistributionScaleY))
                     or (DistributionScaleX = 1);
end;  // TfrmMap.UpdatePointsDisplayed

{-------------------------------------------------------------------------------
  Updates the status of the Distribution Point and Gridline PopUp Menus
}
procedure TfrmMap.UpdatePopups;
var
  i: Integer;
begin
  for i := 0 to mnuMapDistributionPoints.Count - 1 do
    pmDistributionPoints.Items[i].Checked := mnuMapDistributionPoints.Items[i].Checked;
  for i := 0 to mnuMapGridLines.Count - 1 do
    pmGridLines.Items[i].Checked := mnuMapGridLines.Items[i].Checked;
end;  // TfrmMap.UpdatePopups

{-------------------------------------------------------------------------------
}
procedure TfrmMap.WMAssociateBoundary(var Message: TMessage);
begin
  if SelectedRegion.ObjectID<> - 1 then
    LinkToBoundary(SelectedRegion.SheetID, SelectedRegion.ObjectID);
end;  // TfrmMap.WMAssociateBoundary 

{-------------------------------------------------------------------------------
}
procedure TfrmMap.WMRefreshColours(var Msg: TMessage);
begin
  Repaint;
end;  // TfrmMap.WMRefreshColours 

{-------------------------------------------------------------------------------
  This is the fundamental message which is called when the MapUSer sheet is to be re-drawn by 
      MapServer.
  
  The "lParam" component of the message contains a structure of 3 different sets of N,S,E and 
      W global extents.  These hold data defining (in no particular order)
  - the coordinates of the MapWindow newly exposed
  - the total map window coordinates
  - the 'live' area covered by the dataset.
  It is all a bit vague, and best understood by capturing the exents and drawing polygons on 
      them so that one can see which part of the message corresponds to which part of the
      screen. for this app it is not really important. 
}
procedure TfrmMap.WMUMapUserDraw(var Msg: TMessage);
var
  lCursor: TCursor;
begin
  inherited;
  lCursor := MapCursor;
  try
    MapCursor := crHourGlass;
    MapUserData := psUserData(Msg.lParam);
  finally
    MapCursor := lCursor;
  end;
end;  // TfrmMap.WMUMapUserDraw

{-------------------------------------------------------------------------------
  Resets the BoundingBox Property to zero 
}
procedure TfrmMap.ZeroBoundingBox;
begin
  FBoundingBox.SouthWest.Lat := NULL_LATLONG;
  FBoundingBox.SouthWest.Long:= NULL_LATLONG;
  FBoundingBox.NorthEast.Lat := NULL_LATLONG;
  FBoundingBox.NorthEast.Long:= NULL_LATLONG;
end;  // TfrmMap.ZeroBoundingBox

{-------------------------------------------------------------------------------
  Resets FSartPoint etc values.
}
procedure TfrmMap.ZeroDragBox;
begin
  FMouseUpPixPos.x   := 0;
  FMouseUpPixPos.y   := 0;
  FMouseDownPixPos.x := 0;
  FMouseDownPixPos.y := 0;
  RefreshMap;
end;  // TfrmMap.ZeroDragBox 

{-------------------------------------------------------------------------------
  if no objects were found when querying the objects at a specific set of coordinates then 
      this is called to fill the fields in the TQueryDetails object. 
}
function TfrmMap.ZeroObjectsFound(const iCursorPosition: TMapCursorPos): TQueryDetails;
begin
  Result.MapPosition.X     := iCursorPosition.MapPos.X;
  Result.MapPosition.y     := iCursorPosition.MapPos.y;
  Result.ObjectName        := AddinResourceStrings.ResStr_None;
  Result.ObjectID          := -1;
  Result.ObjectStaticID    := -1;
  Result.ObjectCentroid.x  :=  0;
  Result.ObjectCentroid.y  :=  0;
  //  Result.ObjectType       := MS_UNDEFINED_OBJECT;
  Result.ObjectLocationKey := '';
  Result.SheetName         := '';
  Result.SheetID           := -1;
end;  // TfrmMap.ZeroObjectsFound

{-------------------------------------------------------------------------------
  Act when a COM Dataset is sent to the map
}
procedure TfrmMap.AddCOMDataset(const ATitle: string; ADataset: IMapDropFormat);
begin
  if CreateDistributionList(nil, nil, ADataset, ATitle, ADataset.DatasetTableName,
      '') = nil then
    MessageDlg(ResStr_NoRecordings, mtInformation, [mbOk], 0);
end;

{-------------------------------------------------------------------------------
  Display the Extract Grid Squares dialog
}
procedure TfrmMap.actExtractGridSquaresExecute(Sender: TObject);
begin
  with TdlgExtractGridSquares.Create(nil) do
    try
      LayerLegend := FLayerLegend;
      if (not ObjectDetails.IsAdminArea) and (ObjectDetails.ObjectLocationKey<>'') then begin
        LocationKey := ObjectDetails.ObjectLocationKey;
        LocationName := ObjectDetails.ObjectName;
        LocationObjectID := ObjectDetails.ObjectStaticID;
      end;
      LocationSheetID := ObjectDetails.SheetID;
      BaseMapKey := FBaseMapKey;
      ShowModal;
    finally
      Free;
    end; // try
end;

{-------------------------------------------------------------------------------
}
procedure TfrmMap.pmQuickReportsClick(Sender: TObject);
begin
  inherited;
  BringToFront; // in case popup activated when not the active mdi child, which can happen
  frmMain.PopulateQuickReportSubMenu(pmQuickReports,
      [ktSamplesInPolygon, ktLocationsInPolygon], SQL_SAMPLES_FOR_POLYGON);
end;

{-------------------------------------------------------------------------------
}
procedure TfrmMap.pmBatchUpdateClick(Sender: TObject);
begin
  inherited;
  BringToFront; // in case popup activated when not the active mdi child, which can happen
  frmMain.PopulateBatchUpdateSubMenu(pmBatchUpdate,
      [ktSamplesInPolygon, ktLocationsInPolygon], SQL_SAMPLES_FOR_POLYGON);
end;

{-------------------------------------------------------------------------------
  Enable / disables the PolygonReport pop up menu
}
procedure TfrmMap.SetPopUpMenu(ASet: boolean);
begin
  if ASet then
    FMapPopupMenu := pmPolygonReport
  else
    FMapPopupMenu := nil;
end;

{-------------------------------------------------------------------------------
  Finds the samples in the chosen polygon and put them in TempList in DB
}
procedure TfrmMap.ReadSamplesInPolygon(AConnection: TADOConnection;
    AIncludePartialOverlap: boolean);
var
  lPolygonScanner : TPolygonScanner;
  lStaticID : Integer;
begin
  begin
    lPolygonScanner := TPolygonScanner.Create(MapServerLink.MapHandle, DatasetSpatialSystem);
    try
      //Getting the static ID of the selected polygon
      MapCheck(MapGetObjectStaticID(MapServerLink.MapHandle, SelectedRegion.SheetID,
                                      SelectedRegion.ObjectID, lStaticID));

      //Gets the samples for this polygon and stores them in the temporary table
      //#TempList in the DB
      lPolygonScanner.GetSamplesForPolygon(SelectedRegion.SheetID,
                           lStaticID, AConnection, AIncludePartialOverlap);
    finally
      lPolygonScanner.Free;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Finds the locations in the chosen polygon and put them in TempList in DB
}
procedure TfrmMap.ReadLocationsInPolygon(AConnection: TADOConnection;
  AIncludePartialOverlap: boolean);
var
  lPolygonScanner : TPolygonScanner;
  lStaticID : Integer;
begin
  begin
    lPolygonScanner := TPolygonScanner.Create(MapServerLink.MapHandle, DatasetSpatialSystem);
    try
      //Getting the static ID of the selected polygon
      MapCheck(MapGetObjectStaticID(MapServerLink.MapHandle, SelectedRegion.SheetID,
                                      SelectedRegion.ObjectID, lStaticID));

      //Gets the samples for this polygon and stores them in the temporary table
      //#TempList in the DB
      lPolygonScanner.GetLocationsForPolygon(SelectedRegion.SheetID,
                           lStaticID, AConnection, AIncludePartialOverlap);
    finally
      lPolygonScanner.Free;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  When a row is selected, enables or disables the up/down buttons.
}
procedure TfrmMap.sgLayersSelectCell(Sender: TObject; ACol, ARow: Integer;
  var CanSelect: Boolean);
begin
  inherited;
  btnLayerUp.Enabled   := ARow > 0;
  btnLayerDown.Enabled := ARow < sgLayers.RowCount - 1;
end;

{-------------------------------------------------------------------------------
  Moves the selected layer one position down.
}
procedure TfrmMap.btnLayerDownClick(Sender: TObject);
begin
  inherited;
  if sgLayers.Selection.Top < sgLayers.RowCount - 1 then
    FLayerLegend.MoveLayer(sgLayers.Selection.Top, sgLayers.Selection.Top + 1);
end;

{-------------------------------------------------------------------------------
  Moves the selected layer one position up.
}
procedure TfrmMap.btnLayerUpClick(Sender: TObject);
begin
  inherited;
  if sgLayers.Selection.Top > 0 then
    FLayerLegend.MoveLayer(sgLayers.Selection.Top, sgLayers.Selection.Top - 1);
end;

procedure TfrmMap.CleanUpBoundaryImport;
begin

  { Detach and Delete Sheets }
  MapCheck(MapSelectSheet(FMapServerLink.MapHandle, FMapServerLink.SheetTotal - 1, False));
  MapCheck(MapDetachSheet(FMapServerLink.MapHandle, FMapServerLink.SheetTotal - 1));
  { Delete Sheets }
  if FDeleteTemporaryMapFile then
    FdmMap.DeleteMapSheetFiles(AppSettings.MapFilePath, FShortName, stVector);

  frmMain.ProgressBar.TaskPosition := 0;
  frmMain.TaskFinished;
  MapCursor := FPreBoundaryImportMapCursor;
  AppSettings.UpdateMapWindowSelectors;
  mBoundaryImportInProgress := False;
end;


//==============================================================================
// Procedure Name: tmBoundingBoxTimer
//        Purpose: The map now renders the bounding box rectangle according to
//                 its geographical definition, and therefore zooming and
//                 scrolling the map do interfere with it. However our "fake"
//                 rectangle which stores the latest screen coordinates of the
//                 box (to handle mouse events) must be updated periodically, as
//                 there is no message or event from the map to notify us.
//------------------------------------------------------------------------------
procedure TfrmMap.tmBoundingBoxTimer(Sender: TObject);
begin
  if Assigned(FMapServerLink) then
  begin
    if FSelectingArea then
    begin
      UpdateFakeRect;

      if (FBoundingBoxResizeDragElement = NoSideOrCorner) and
          (Tool = TOOL_SELECT_AREA) then
        UpdateAreaSelectionToolCursor(GetPossibleBoundingBoxResizeDragElement(
            pnlMapPanel.ScreenToClient(Mouse.CursorPos)));
    end;
  end;
end;

//==============================================================================
// Function Name: GetPossibleBoundingBoxResizeDragElement
//    Parameters: MousePixel - The coordinates on the map panel where the mouse
//                cursor is pointing.
//       Purpose: Determines which side or corner, if any, of the selection
//                bounding box should be dragged to resize it if the left mouse
//                button is held down and the cursor moved from this point.
//------------------------------------------------------------------------------
function TfrmMap.GetPossibleBoundingBoxResizeDragElement(
    const MousePixel: TPoint): TRectangleElement;
var
  Offsets: TRect;
begin
  Result := NoSideOrCorner;

  // Check that the bounding box is populated.
  if (BoundingBox.SouthWest.Lat <> NULL_LATLONG) and
      (BoundingBox.NorthEast.Lat <> NULL_LATLONG) then
  begin
    with Offsets do begin
      Left := Abs(BoundingBoxFake.Left - MousePixel.X);
      Right := Abs(BoundingBoxFake.Right - MousePixel.X);
      Top := Abs(BoundingBoxFake.Top - MousePixel.Y);
      Bottom := Abs(BoundingBoxFake.Bottom - MousePixel.Y);
    end;

    // Now determine which side or corner, if any, the mouse cursor is close to.
    if (MousePixel.Y > BoundingBoxFake.Top - DRAG_THRESHOLD) and
        (MousePixel.Y < BoundingBoxFake.Bottom + DRAG_THRESHOLD) then
    begin
      if (Offsets.Left < DRAG_THRESHOLD) then
      begin
        if Offsets.Top < DRAG_THRESHOLD then
          Result := TopLeftCorner
        else if Offsets.Bottom < DRAG_THRESHOLD then
          Result := BottomLeftCorner
        else
          Result := LeftSide;
      end else if (Offsets.Right < DRAG_THRESHOLD) then
      begin
        if (Offsets.Top < DRAG_THRESHOLD) then
          Result := TopRightCorner
        else if (Offsets.Bottom < DRAG_THRESHOLD) then
          Result := BottomRightCorner
        else
          Result := RightSide;
      end else if (MousePixel.X > BoundingBoxFake.Left - DRAG_THRESHOLD) and
          (MousePixel.X < BoundingBoxFake.Right + DRAG_THRESHOLD) then
      begin
        if Offsets.Top < DRAG_THRESHOLD then
          Result := TopSide
        else if Offsets.Bottom < DRAG_THRESHOLD then
          Result := BottomSide;
      end;
    end;
  end;
end;

//==============================================================================
// Procedure Name: DrawBoundingBox
//        Purpose: Draws onto the map surface a box that surrounds the area
//                 defined by the bounding box property, but only if the corners
//                 that define the box contain values.
//------------------------------------------------------------------------------
procedure TfrmMap.DrawBoundingBox;
var
  BottomLeft, TopRight, TopLeft, BottomRight: MSCoord;
begin
  if (BoundingBox.SouthWest.Lat <> NULL_LATLONG) and
      (BoundingBox.NorthEast.Lat <> NULL_LATLONG) then
  begin
    BottomLeft := MSCoord(LatLongToSpecificEN(BoundingBox.SouthWest,
        DatasetSpatialSystem));
    TopRight := MSCoord(LatLongToSpecificEN(BoundingBox.NorthEast,
        DatasetSpatialSystem));

    TopLeft.X := BottomLeft.X;
    TopLeft.Y := TopRight.Y;
    BottomRight.X := TopRight.X;
    BottomRight.Y := BottomLeft.Y;

    MapEditClearObject(FMapServerLink.MapHandle);

    // Create a new line on the map.
    CreateNewMapObject(MS_LINE_OBJECT);

    MapEditSetColor(FMapServerLink.MapHandle, SCREEN_GRAY_COLOR);

    // Set the line to be visible.
    MapEditSetVisible(FMapServerLink.MapHandle, True);

    // Draw the lines that define the rectangle.
    MapEditInsertObjectCoord(FMapServerLink.MapHandle, MS_LAST_COORD, TopLeft);
    MapEditInsertObjectCoord(FMapServerLink.MapHandle, MS_LAST_COORD, TopRight);
    MapEditInsertObjectCoord(FMapServerLink.MapHandle, MS_LAST_COORD,
        BottomRight);
    MapEditInsertObjectCoord(FMapServerLink.MapHandle, MS_LAST_COORD,
        BottomLeft);
    MapEditInsertObjectCoord(FMapServerLink.MapHandle, MS_LAST_COORD, TopLeft);

    // Render the rectangle.
    MapEditUpdate(FMapServerLink.MapHandle);
  end;
end;

//==============================================================================
// Procedure Name: UpdateFakeRect
//        Purpose: We need to store a "fake" rectangle which represents the
//                 bounding box on the screen, rather than geographically. We do
//                 this for changing the cursor to a resize cursor and anything
//                 else where the screen coordinates are needed - it's too
//                 expensive computationally to try to convert the map
//                 coordinates to the screen coordinates on MouseMove etc. We
//                 call this procedure periodically in a timer instead.
//------------------------------------------------------------------------------
procedure TfrmMap.UpdateFakeRect;
var
  BottomLeftEN, TopRightEN: TMapCoord;
  BottomLeftPixel, TopRightPixel: TPoint;
begin
  if (BoundingBox.SouthWest.Lat <> NULL_LATLONG) and
      (BoundingBox.NorthEast.Lat <> NULL_LATLONG) then
  begin
    BottomLeftEN := LatLongToSpecificEN(BoundingBox.SouthWest,
        DatasetSpatialSystem);
    TopRightEN := LatLongToSpecificEN(BoundingBox.NorthEast,
        DatasetSpatialSystem);

    BottomLeftPixel := MapToScreen(BottomLeftEN.X, BottomLeftEN.Y,
        VisibleExtents);
    TopRightPixel := MapToScreen(TopRightEN.X, TopRightEN.Y, VisibleExtents);

    with BoundingBoxFake do
    begin
      Left := BottomLeftPixel.X;
      Right := TopRightPixel.X;
      Top := TopRightPixel.Y;
      Bottom := BottomLeftPixel.Y;
    end;
  end;
end;

{ Does the Boundary Import after the mapping has been done (either automatically or manually)
}
procedure TfrmMap.CompleteBoundaryImport(sheetIndex, importObjectTotal,
    destSheetID: Integer; locationKeys: TStringList; polygonIndices: Array of
    Integer);
var
  copyKeyValue: Boolean;
  staticIDs: array of Integer;
  objectType: uChar;
  siteID: string;
  sheetKey: string;
  locationBoundaryKey: string;
  sql: string;
  vagueDate: TVagueDate;
  staticID: Integer;
  fieldName: Array[0..255] of char;
  lRet: Integer;
  i: Integer;
begin

  // TODO improve the error handling

  RefreshMap;

  { Find out whether the input sheet has a KeyValue field and if so store
    the name to compare against the object sheets KeyValue index field }
  lRet := MapDataGetAttributeFieldName(
      FMapServerLink.MapHandle,
      sheetIndex,
      MS_FIELD_LOCATION_KEY,
      fieldName,
      20);

  if (lRet <> MS_SUCCESS) or FDeleteTemporaryMapFile then
  begin
    // any failure here - cannot link up location keys
    copyKeyValue := False
  end
  else
  begin
    // check field name correct
    copyKeyValue :=
        CompareText(Trim(StrPas(fieldName)), MS_FIELD_LOCATION_KEY_NAME) = 0;
  end;

  { SetUp the ProgressBar... }
  frmMain.SetStatus(ResStr_CopyingObjects);
  frmMain.ProgressBar.TaskPosition := 0;

  SetLength(staticIDs, importObjectTotal);
  for i := 0 to importObjectTotal - 1 do
  begin
    MapCheck(MapGetObjectType(FMapServerLink.MapHandle, sheetIndex, i, objectType));
    staticIDs[i] := CopyObjectToObjectSheet(
        FMapServerLink.SheetTotal - 1,
        i,
        destSheetID,
        objectType,
        True,
        copyKeyValue);
    frmMain.ProgressBar.TaskPosition := Trunc((100 / importObjectTotal) * i);
  end; // for lCount..
  siteID   := AppSettings.SiteID;
  sheetKey := FMapServerLink.SheetMapSheetKey(destSheetID);

  FMapServerLink.SaveChanges(BaseMapKey);

  for i := 0 to locationKeys.Count - 1 do
  begin
    // Gets the static ID of the ith (new) polygon.
    staticID := staticIDs[polygonIndices[i]];
    locationBoundaryKey :=
        dmGeneralData.GetNextKey(TN_LOCATION_BOUNDARY, PK_LOCATION_BOUNDARY);

    vagueDate := StringToVagueDate(DateToStr(Date));

    sql := Format('INSERT INTO Location_Boundary ('
        + 'Location_Boundary_Key, '
        + 'Location_Key, '
        + 'Version, '
        + 'Map_Sheet_Key, '
        + 'Object_ID, '
        + 'From_Vague_Date_Start, '
        + 'From_Vague_Date_End, '
        + 'From_Vague_Date_Type, '
        + 'Entered_By) '
        + 'VALUES (''%s'',''%s'',1,''%s'',''%d'',''%s'',''%s'',''%s'',''%s'')',
        [locationBoundaryKey,
            locationKeys[i],
            sheetKey,
            staticID,
            FloatToStr(vagueDate.StartDate),
            FloatToStr(vagueDate.EndDate),
            vagueDate.DateTypeString,
            AppSettings.UserID]);
    dmGeneralData.ExecuteSQL(sql, ResStr_InsertFailed, false);
    FMapServerLink.LinkObjectToBoundary(
        sheetKey,
        staticID,
        locationKeys[i],
        locationBoundaryKey);
  end;
end;

function TfrmMap.GetIsBoundaryImportInProgress: Boolean;
begin
  Result := Assigned(FBoundaryLocationMatch) and mBoundaryImportInProgress;
end;

procedure TfrmMap.MatchedBoundaries(save, showMap: Boolean);
var
  lPolygonIndices: Array of Integer;
  i: Integer;
begin
  if save then
  begin
    SetLength(lPolygonIndices, FBoundaryLocationMatch.PolygonIndicesLength);
    for i := 0 to FBoundaryLocationMatch.PolygonIndicesLength - 1 do
    begin
      lPolygonIndices[i] := FBoundaryLocationMatch.PolygonIndices[i];
    end;

    CompleteBoundaryImport(FBoundaryLocationMatch.SourceSheetId, FImportObjectTotal,
        FBoundaryLocationMatch.TargetSheetId, FLocationKeys,
        lPolygonIndices);
  end;

  CleanUpBoundaryImport();

  WindowState := FCurrentWindowState;

  FreeAndNil(FLocationKeys);
  FBoundaryLocationMatch := nil;
  if showMap then
  begin
    Self.Show();
  end;
end;

end.
