//==============================================================================
//  Unit:        FilterResult
//
//  Implements:  TfrmFilterResult
//
//  Description: Displays the results of filters produced via the Report Wizard,
//               dlgWizard.  Allows user to send these results to the map (if
//               spatial data is included in the filter), to export these results
//               via dlgDataExport as NBN Data, CIS or DMAP as default export
//               formats or to produce a report.  In the case of printing a report,
//               if the "New Layout" option was chosen in dlgWizard then the
//               QuickReport designer is called to allow the user to design a new
//               template for the report.  If the "Use Existing Layout" option was
//               selected then the user is prompted to choose a report template
//               before the print preview for that report is displayed.
//                                          
//  Author:      John van Breda
//  Created:     8 April 1999
//
//  Last Revision Details:
//    $Revision: 226 $
//    $Date: 4.06.10 13:32 $
//    $Author: Andrewkemp $
//
//==============================================================================

unit FilterResult;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  BaseChildUnit, StdCtrls, Buttons, Grids, ExtCtrls, Menus, BaseFormUnit, Db,
  Wizard, DataClasses, BaseExportableUnit, DropSource, JNCCDatasets, ClipBrd,
  Recorder2000_TLB, ReportDesigner, Printers, QRPrntr, OnlineHelp, Observations,
  Locations, Filter, Constants, ActnList, ReportsData, GeneralFunctions,
  ImageListButton, ReportGenerator, ADODB, DatabaseAccessADO, DBGrids, SpatialRefFuncs,
  RTFGrid, JNCCGrid, Snapshot, ADODB_TLB, ComCtrls, ReportWizardSettings,
  CRReportSQL, CRCommonClasses, ReportExport, GoogleEarthExport, Map,
  xmldom, XMLIntf, msxmldom, DBFilterGrid;

resourcestring
  ResStr_NoDataToExport = 'The dataset does not contain any data that can be exported.';
  SFilterResult_NoOccurrences   = 'No occurrences found';
  SFilterResult_OneOccurrence   = '1 occurrence found';
  SFilterResult_ManyOccurrences = '%s occurrences found';
  SFilterResult_NoRecords       = 'No records found';
  SFilterResult_OneRecord       = '1 record found';
  SFilterResult_ManyRecords     = '%s records found';
  SFilterResult_Title           = 'Filter Result';
  SFilterResult_ToolTip         = 'Filter record set';
  SFilterResult_Untitled        = 'Untitled';
  SFilterResult_Template        = 'Template';
  SFilterResult_Snapshot        = 'Snapshot';
  SFilterResult_SaveMessage     =
      'The %s that you saved only contains the way the data is presented. To be ' +
      'able to run the exact'#13'same report again, you also need to save the ' +
      'data selection criteria as chosen through the Wizard.'#13#13;
  SFilterResult_SaveChanges     = 'Do you want to save the changes to your report?';
  ResStr_NoPrinterAvailable = 'There are no printers available.  Please make sure there is a default printer ' +
                              'connected and try again.';
  ResStr_ReportDisplay =  'You did not select a template or snapshot for your report.'#13 +
                          'If you run this report in the future, you will be asked to ' +
                          'select a template or snapshot so the report can be displayed, ' +
                          'printed or exported.'#13#13 +
                          'If you want to proceed, click <OK>, otherwise click <Cancel>.';

  ResStr_ColumnNotFound = 'The column ''%s'' specified in the ' +
                          'report cannot be found in the result set.'#13#13 +
                          'Please check the report file for syntax errors or other inaccuracies.';

  ResStr_InvalidMapKey  =
      'Unable to generate the report:'#13
      + 'Invalid map key. The polygon specified in the report does not belong to a know map.';

  ResStr_PageFooter = 'Printed At: %s';
  ResStr_PageTitle =  'Report Output';
  ResStr_InvalidReportFile = '''%s'' is not a valid Recorder 2000 report file.';

  //Status
  ResStr_CreatingReport = 'Creating Report...';
  ResStr_RunningSnapshot = 'Running Snapshot';
  ResStr_OpeningReportTemp =  'Opening Report Template';


const
  REPORT_VERSION_2 = '2';
  REPORT_VERSION_3 = '3';

type
  TfrmFilterResult = class(TBaseExportable, IReportResults)
    pnlTitle: TPanel;
    mnuEdit: TMenuItem;
    mnuEditCopy: TMenuItem;
    N1: TMenuItem;
    mnuEditView: TMenuItem;
    Reports1: TMenuItem;
    mnuReportOpen: TMenuItem;
    mnuReportWizard: TMenuItem;
    N3: TMenuItem;
    pnlButtons: TPanel;
    pnlButtons2: TPanel;
    mnuReportMap: TMenuItem;
    pmOutput: TPopupMenu;
    dlgSave: TSaveDialog;
    alResult: TActionList;
    actResultCopy: TAction;
    actResultView: TAction;
    lblOccCount: TLabel;
    OccurrencesforPlacesReport1: TMenuItem;
    PlacesforOccurrencesReport1: TMenuItem;
    bbBack: TImageListButton;
    bbFinish: TImageListButton;
    btnOutput: TImageListButton;
    lblClickInstruction: TLabel;
    dbgResults: TDBFilterGrid;
    actPrintGrid: TAction;
    actNewSnapshot: TAction;
    actEditSnapshot: TAction;
    actRunSnapshot: TAction;
    actNewReportTemplate: TAction;
    actEditReportTemplate: TAction;
    actRunReportTemplate: TAction;
    pmPrintGrid: TMenuItem;
    pmOutputCopy: TMenuItem;
    pmOutputMap: TMenuItem;
    pmOutputExportData: TMenuItem;
    N2: TMenuItem;
    pmRunReportTemplate: TMenuItem;
    pmEditReportTemplate: TMenuItem;
    pmNewReportTemplate: TMenuItem;
    N5: TMenuItem;
    pmRunSnapshot: TMenuItem;
    pmNewSnapshot: TMenuItem;
    pmEditSnapshot: TMenuItem;
    DataSource: TDataSource;
    qryCustomReport: TJNCCQuery;
    pnlPreparingReport: TPanel;
    N4: TMenuItem;
    mnuOutputExportExcel: TMenuItem;
    mnuOutputExportOther: TMenuItem;
    pmOutputExportGoogleEarth: TMenuItem;
    pmOutputRevalidateItems: TMenuItem;
    mnuOutputExportSHP: TMenuItem;
    pmOutputResultCopyAll: TMenuItem;
    actResultCopyAll: TAction;
    mnuEditCopyAll: TMenuItem;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormActivate(Sender: TObject);
    procedure bbBackClick(Sender: TObject);
    procedure bbFinishClick(Sender: TObject);
    procedure btnOutputClick(Sender: TObject);
    procedure pmGridClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormDeactivate(Sender: TObject);
    procedure actResultCopyExecute(Sender: TObject);
    procedure actResultCopyAllExecute(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure dbgResultsMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure actNewSnapshotExecute(Sender: TObject);
    procedure actEditSnapshotExecute(Sender: TObject);
    procedure actRunSnapshotExecute(Sender: TObject);
    procedure actPrintGridExecute(Sender: TObject);
    procedure actNewReportTemplateExecute(Sender: TObject);
    procedure actEditReportTemplateExecute(Sender: TObject);
    procedure actRunReportTemplateExecute(Sender: TObject);
    procedure dbgResultsCellClick(Column: TColumn);
    procedure dbgResultsMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure dbgResultsDrawColumnCell(Sender: TObject; const Rect: TRect;
      DataCol: Integer; Column: TColumn; State: TGridDrawState);
    procedure mnuOutputExportOtherClick(Sender: TObject);
    procedure mnuOutputExportExcelClick(Sender: TObject);
    procedure pmOutputExportGoogleEarthClick(Sender: TObject);
    procedure pmOutputRevalidateItemsClick(Sender: TObject);
    procedure dbgResultsColumnMoved(Sender: TObject; FromIndex,
      ToIndex: Integer);
    procedure mnuOutputExportSHPClick(Sender: TObject);
    procedure dbgResultsFilterOperator(Sender: TObject; Column: Integer;
        const Operator: String; var Accept: Boolean);
    procedure dbgResultsFilterChanged(Sender: TObject);
  private
    FHintWindow: THintWindow;
    FCustomReport          : Boolean;
    FWizard                : TdlgWizard;
    FSettings              : TReportWizardSettings;
    FReportDesigner        : TfrmReportDesigner;
    FSnapshot              : TfrmSnapshot;
    FReportFile            : String;
    FAccessingMapFromWizard: Boolean;
    FV2SQL                 : String;
    FtfMouseOverCell      : Boolean;
    FdbgResultsMousePoint   : TPoint;
    FReportVersion          : String;
    FCRReportFile           : TReportFile;
    FConnection             : TADOConnection;
    FReportSaved: Boolean;
    FReportSaveRejected: Boolean;
    FColumnOrderChanged: Boolean;
    FWindowState: TWindowState;
    FClientFilteredColumns: TStrings;
    FColumnWidths: array of Integer;
    FColumnFields: TStringList;
    procedure PrepareGridOutput;
    procedure PrintPageFooter(XMin, XMax, YMax: integer);
    procedure PrintPageHeader(XMin, XMax: integer; PageHeaderText,
      PageNumber: string);
    procedure StartNewPage(XMin, XMax, YMax: integer; PageHeaderText,
      PageNumber: string);
    procedure PrintGrid(const PrintIt:boolean);
    procedure WMRunWizard(var Msg:TMessage); message WM_RUN_WIZARD;
    procedure UpdateResults(ACriteria: TStrings = nil);
    procedure GetColumnIndexes;
    procedure ResizeColumns;
    procedure DisplaySelectedName(const ANameKey: TKeyString);
    procedure DisplaySelectedLocation(const ASampleKey: TKeyString);
    procedure DisplaySelectedOccurrence(const ObsType: TRelatedData;
      const AKeyData: TKeyData);
    procedure EnableActionsDependingOnAvailableData;

    procedure ShowDesigner(iTemplate: string; itfPreview : Boolean = true);
    procedure ShowSnapshot(iSnapshot: string; itfRun : Boolean = true);
    function GetTemplateFile: string;
    procedure SetTemplateFile(const Value: string);
    function GetSnapshotFile: string;
    procedure SetSnapshotFile(const Value : string);
    function SaveReport(AFilename: String): Boolean; overload;
    procedure SetReportFile(const Value: String);
    procedure LoadV2WizardFile(AstFileName : String);
    procedure GenericConstruction;
    procedure SendToMapClick(Sender: TObject);
    procedure RunCustomReport(const AFileName: string; AKeyType: TKeyType; const
        AKey: string);
    procedure SetCountLabel(const ALabel: string; ACount: integer);
    procedure NavigateCustomReport(AColumn: TColumn);
    procedure UpdateSnapshotTemplateActions;
    function IsHyperlink(const AText: string): boolean;
    function GetCellText(X, Y: integer): string;
    procedure ExternalHyperlink(const ALink: string);
    procedure GenericExport(AExportClass: TBaseExportClass);
    procedure SetReportSaveRejected(const Value: Boolean);
    function GetColumnIndex(AName: string) : Integer;
    procedure MemoriseColumns;
    procedure RecallColumns;
  protected
    procedure GetNodeData(const Sender: TObject; var oDropSource: TJNCCDropSource);
    // IReportResults interface
    function Get_ReportConnection: ADODb_Tlb._Connection; safecall;
    function Get_ReportSQL: WideString; safecall;
    property ReportSaveRejected: Boolean read FReportSaveRejected write
        SetReportSaveRejected;
    function GetAttributeField(AName: string) : TField;
    procedure CloseParameterScreen;
    procedure ResetClientFilter;
    function MakeCriteria: TStrings;
    procedure ClientFilterColumn(Index: Integer); virtual;
    procedure ApplyGridFilterToResults;
    procedure ApplyCriteriaToCustomReport(ACriteria: TStrings);
    procedure ResultsFilterRecord(DataSet: TDataSet; var Accept: Boolean);
        virtual;
    function CountRecords: Integer;
  public
    constructor Create(AOwner:TComponent); override;
    constructor CreateAndRun(AOwner : TComponent; istFileName : string;
        AKeyType: TKeyType=ktDefault; const AKey: string='');
    constructor CreateAndRunWithFilter(AOwner : TComponent; const AFileName : string;
                ASQLProviderIntf : IProvidesOccurrencesSQL);
    destructor Destroy; override;
    procedure ApplySecurity; override;
    procedure CancelReportCallback;
    procedure DisplayGridCallback;
    procedure RunWizard;  // resets the form as required and runs the wiz
    procedure PreviewFinished;
    procedure PreviewScreen; override;
    procedure PrintScreen; override;
    procedure SaveExportMetaData(const ExportFileName: String);
    function SaveReportAs: Boolean;
    function SaveReport: Boolean; overload;
    function GetKeyList: TKeyList; override;
    function GetKeyListForValidation: TKeyList; override;
    procedure RunReport(AFileName : String;
              AKeyType: TKeyType=ktDefault; const AKey: string='');
    procedure RunReportWithFilter(const AFileName : string;
      ASQLProviderIntf : IProvidesOccurrencesSQL);
    procedure UpdateMapWindowSelector; override;
    property Snapshot:TfrmSnapshot read FSnapshot write FSnapshot;
    property ReportDesigner:TfrmReportDesigner read FReportDesigner write FReportDesigner;
    property TemplateFile : string read GetTemplateFile write SetTemplateFile;
    property SnapshotFile : string read GetSnapshotFile write SetSnapshotFile;
    property ReportFile : String read FReportFile write SetReportFile;
    property AccessingMapFromWizard:boolean read FAccessingMapFromWizard write FAccessingMapFromWizard;
    property ReportVersion : String read FReportVersion;
    property ReportSaved: Boolean read FReportSaved;
  end;

//==============================================================================
implementation

{$R *.DFM}

uses
  FormActions, Maintbar, IndOrg, Reports, ReportPreview,
  ApplicationSettings, COMClasses, GeneralData, WizardData, Contnrs, Math,
  StrUtils, XMLDoc, RunReportOptions, ExceptionForm,
  Variants, CRConstants, CRColumns, ComObj, ResourceStrings, EasyShell,
  SQLConstants, ExternalFilter, MapServerLink, MapPolygonScanner,
  ReportToSHPExport, VagueDate, PatternMatching;

const
  CLASS_SupportRucksack: TGUID = '{DC4804A5-183A-11D3-B6E3-0060979160B0}';
  LINE_BREAK=#13#10;
  ST_OCCURRENCE = 'occurrence';
  ST_RECORD     = 'record';

type
  TPostProcessGrid = class(TObject)
  private
    FGrid: TDBGrid;
    FReportFile: TReportFile;
    procedure SetColumnCaptions;
    procedure SetColumnVisible;
    procedure SetColumnWidth;
    procedure SortColumns;
  public
    constructor Create(AGrid: TDBGrid; const AReportFile: TReportFile);
    procedure PostProcessGrid;
    property Grid: TDBGrid read FGrid;
  end;

  // provide access to protected TDBGrid
  TDBGridAccessor = class(TDBGrid);

{-------------------------------------------------------------------------------
  Compare method for column sorts
}
function ColumnCompare(Item1, Item2: Pointer): Integer;
begin
  if not ((TObject(Item1) is TReportColumn) and (TObject(Item2) is TReportColumn)) then
    Result := 0
  else if TReportColumn(Item1).Position < TReportColumn(Item2).Position then
    Result := -1
  else if TReportColumn(Item1).Position > TReportColumn(Item2).Position then
    Result := 1
  else
    Result := 0;
end;

{-------------------------------------------------------------------------------
  Does the given field name correspond to a spatial reference?
}
function IsSpatialReferenceField(const AFieldName: String): Boolean;
begin
  Result := AnsiEndsText('SPATIAL_REF', AFieldName)
end;

//==============================================================================
constructor TfrmFilterResult.Create(AOwner:TComponent);
begin
  inherited Create(AOwner);
  ReportFile  := '';
  FCustomReport := false;
  GenericConstruction;
  FReportVersion := REPORT_VERSION_3;
  SendMessage(Handle,WM_UPDATE_MENU_ICONS,0,0);
  PostMessage(Handle,WM_RUN_WIZARD,0,0);
end;  // Create

{-------------------------------------------------------------------------------
}
constructor TfrmFilterResult.CreateAndRun(AOwner: TComponent;
  istFileName: string; AKeyType: TKeyType=ktDefault; const AKey: string='');
var
  lCloseAction : TCloseAction;
begin
  inherited Create(AOwner);
  try
    GenericConstruction;
    bbBack.Enabled := false; // shouldn't be able to navigate back through the wizard.
    RunReport(istFileName, AKeyType, AKey);
  except
  //Make sure the actions are re-enabled
    lCloseAction := canone;
    FormClose(nil, lCloseAction);
    raise;
  end;
end;

{-------------------------------------------------------------------------------
  Description : Creates the form, runs a given report but replaces the filter
              with the supplied SQL
  Created : 11/03/2003 }
constructor TfrmFilterResult.CreateAndRunWithFilter(AOwner : TComponent; const AFileName : string;
                ASQLProviderIntf : IProvidesOccurrencesSQL);
var
  lCloseAction : TCloseAction;
begin
  inherited Create(AOwner);
  try
    GenericConstruction;
    bbBack.Enabled := false; // shouldn't be able to navigate back through the wizard.
    RunReportWithFilter(AFileName, ASQLProviderIntf);
  except
    //Make sure the actions are re-enabled
    lCloseAction := canone;
    FormClose(nil, lCloseAction);
    raise;
  end;
end;

{-------------------------------------------------------------------------------
  Description : Code that is shared between constructors
  Created : 11/03/2003 }
procedure TfrmFilterResult.GenericConstruction;
begin
  //Help Setup
  HelpContext := IDH_FILTERRESULT;
  // Run the wizard
  FReportDesigner:=nil;
  FSnapshot := nil;
  dmFormActions.actOpenReport.Enabled  :=false;
  dmFormActions.actReportWizard.Enabled:=false;
  pmOutputRevalidateItems.Visible := AppSettings.UserAccessLevel = ualAdmin;
  FtfMouseOverCell := false;
  lblOccCount.Left := pnlTitle.Width - lblOccCount.Width - 8;
  lblOccCount.Caption := SFilterResult_NoOccurrences;
  FSettings := TReportWizardSettings.Create;
  UpdateMapWindowSelector;
  ReportSaveRejected := false;
  FClientFilteredColumns := TStringList.Create;
  // A list mapping the columns in order to the underlying fields
  FColumnFields := TStringList.Create;
end;

//==============================================================================
procedure TfrmFilterResult.FormActivate(Sender: TObject);
begin
  inherited;
  frmMain.ClearContextToolbar(true);
  frmMain.SetContextToolbar(Self,mnuEdit,0,[]);

  dmFormActions.actPrint.Enabled  :=false;
  if Assigned(dbgResults.Datasource) then begin
    if (FWizard<>nil) and dbgResults.Datasource.Dataset.Active then begin
      dmFormActions.actPrint.Enabled  := dbgResults.Datasource.Dataset.RecordCount > 0;
      dmFormActions.actExport.Enabled := dbgResults.Datasource.Dataset.RecordCount > 0;
    end;
  end;
  dmFormActions.actPreview.Enabled:=dmFormActions.actPrint.Enabled;
  frmMain.mnuFileSave.Enabled:=true;
  frmMain.mnuFileSaveAs.Enabled:=true;
end;  // FormActivate

//==============================================================================
procedure TfrmFilterResult.FormDeactivate(Sender: TObject);
begin
  inherited;
  dmFormActions.actPrint.Enabled  :=false;
  dmFormActions.actPreview.Enabled:=false;
  dmFormActions.actExport.Enabled :=True;
  frmMain.mnuFileSave.Enabled     :=false;
  frmMain.mnuFileSaveAs.Enabled   :=false;
end;  // FormDeactivate

//==============================================================================
procedure TfrmFilterResult.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
var
  lMsg: String;
begin
  inherited;
  CanClose := True;
  try
    if Assigned(FReportDesigner) then
      CanClose := FReportDesigner.CloseQuery;
    if not CanClose then raise EAbort.Create('');

    if Assigned(FSnapshot) then
      CanClose := FSnapshot.CloseQuery;
    if not CanClose then Exit;

    if Assigned(FWizard) then
      if not FWizard.ReportGenerated and not FColumnOrderChanged then raise EAbort.Create('');

    CloseParameterScreen;

    if not (ReportSaved or ReportSaveRejected) then begin
      lMsg := '';
      if not FColumnOrderChanged and ((TemplateFile <> '') or (SnapshotFile <> '')) then
      begin
        if TemplateFile <> '' then
          lMsg := SFilterResult_Template
        else
          lMsg := SFilterResult_Snapshot;
        lMsg := Format(SFilterResult_SaveMessage, [lMsg]);
      end;

      case MessageDlg(lMsg + SFilterResult_SaveChanges,
                      mtConfirmation, [mbYes, mbNo, mbCancel], 0) of
        mrYes: CanClose := SaveReport;
        mrNo: begin
          CanClose := True;
          ReportSaveRejected := true;
        end;
      else
        CanClose := False;
        Exit;
      end;
    end;
  except on EAbort do ;
  end;
  if CanClose and Assigned(FReportDesigner) then
    PostMessage(FReportDesigner.Handle, WM_CLOSE, 0, 0);
end;  // FormCloseQuery

//==============================================================================
procedure TfrmFilterResult.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  inherited;
  if Assigned(FWizard) then
  begin
    // drop the templlist table if a polygon filter was used
    FWizard.dmWizard.connReport.Execute(SQL_TEMPLIST_DROP);
  end;
  dmFormActions.actReportWizard.Enabled:=true;
  dmFormActions.actOpenReport.Enabled  :=true;
  Action:=caFree;
end;  // FormClose

//==============================================================================
destructor TfrmFilterResult.Destroy;
begin
  FClientFilteredColumns.Free;
  FCRReportFile.Free;
  FCRReportFile := nil;
  frmMain.ClearContextToolbar(false);
  // If the wizard is still there, remove it
  FWizard.Free;
  FWizard := nil;
  FSettings.Free;
  FSettings := nil;
  FSnapshot.Free;
  FSnapshot := nil;
  FColumnFields.Free;
  inherited Destroy;
end;  // Destroy

//==============================================================================
procedure TfrmFilterResult.WMRunWizard(var Msg: TMessage);
begin
  RunWizard;
end;  // WMRunWizard

//==============================================================================
procedure TfrmFilterResult.RunWizard;
var lModalResult: TModalResult;
begin
  if FWizard = nil then begin
    FReportSaved := False;
    FWizard:=TdlgWizard.Create(Self, FSettings);
    FConnection := FWizard.dmWizard.connReport;
  end;

  WindowState := wsMinimized;
  try
    Enabled := False;
    FAccessingMapFromWizard := False;  // Used when going to the map for bounding box

    lModalResult := FWizard.ShowModal;

    if lModalResult = mrOk then
    begin
      UpdateResults;
      // If any changes occurred, reset save flag.
      if FWizard.ReportChanged then FReportSaved := False;
    end else
    if lModalResult = mrCancel then begin
      dmFormActions.actPrint.Enabled   := false;
      dmFormActions.actPreview.Enabled := false;
      frmMain.mnuFileSave.Enabled      := false;
      frmMain.mnuFileSaveAs.Enabled    := false;
      Close;
    end else
      FAccessingMapFromWizard := True;
  finally
    Enabled:=true;
  end;
end;  // RunWizard

{-------------------------------------------------------------------------------
  Stores the current grid column widths so that they can be restored later.
}
procedure TfrmFilterResult.MemoriseColumns();
var
  I, J: Integer;
begin
  J := 0;
  SetLength(FColumnWidths, dbgResults.Columns.Count);
  FColumnFields.Clear;
  for I := 0 to dbgResults.Columns.Count - 1 do
  begin
    if dbgResults.Columns[I].Visible then
    begin
      J := J + 1;
      // store the column width
      FColumnWidths[J - 1] := dbgResults.Columns[I].Width;
      // and the field it is associated with
      FColumnFields.Add(dbgResults.Columns[I].FieldName);
    end;
  end;
  SetLength(FColumnWidths, J);
end;

{-------------------------------------------------------------------------------
  Recalls the stored grid column widths and assigns them back to the grid.
}
procedure TfrmFilterResult.RecallColumns();
var
  I, J: Integer;
  lField: TField;
begin
  J := 0;
  // apply column sort order
  for i := 0 to FColumnFields.Count - 1 do begin
    lField := dbgResults.Datasource.Dataset.FindField(FColumnFields[i]);
    if Assigned(lField) then
      lField.Index := i;
  end;

  for I := 0 to dbgResults.Columns.Count - 1 do
  begin
    if dbgResults.Columns[I].Visible then
    begin
      dbgResults.Columns[I].Width := FColumnWidths[J];
      J := J + 1;
    end;
  end;
end;

//==============================================================================
procedure TfrmFilterResult.UpdateResults(ACriteria: TStrings = nil);
var lCursor :TCursor;
    i: Integer;
begin
  lCursor:=HourglassCursor;
  frmMain.SetStatus(ResStr_CreatingReport);
  try
    // if the User has finished the wizard there should be a result set here.
    // Put the title in
    // Use a sneaky trick to get the pixel width of the title text
    ReportFile := FSettings.WizardFile;
    TEditableKeyList(AppSettings.ResultList).Clear; //Clear Appsettings property
     if not Assigned(ACriteria) then
    begin
      ResetClientFilter;
      dbgResults.Datasource := nil;
    end;
    //Find out what sort of report this is by looking at the SQL
    //May not work but is a heuristic
    if Pos('''T''',FSettings.FilterSQL) = 0 then
      FWizard.dmWizard.PrepareOutput(FSettings, 'B', ACriteria)
    else if Pos('''B''',FSettings.FilterSQL) = 0 then
      FWizard.dmWizard.PrepareOutput(FSettings, 'T', ACriteria)
    else
      FWizard.dmWizard.PrepareOutput(FSettings, ' ', ACriteria);
    for i := 0 to 5 do //hide key columns
      FWizard.dmWizard.dsResults.DataSet.Fields[I].Visible := false;
    GetColumnIndexes;

    { This is a workaround to allow DisableControls and EnableControls to be
      called on the results dataset in ApplyGridFilterToResults, which increases
      performance. Without this line, an exception can occur when EnableControls
      is called on the dataset. GetColumnIndexes causes a deLayoutChange event
      (see ADODB.pas) which assigns nil to the "Reserved" pointer. This line
      triggers RefreshBuffers which sets Reserved again, preventing exception. }
    FWizard.dmWizard.dsResults.DataSet.Fields[0].Value;

    if FClientFilteredColumns.Count = 0 then
      SetCountLabel(ST_OCCURRENCE, FWizard.dmWizard.qryResults.RecordCount)
    else
      SetCountLabel(ST_OCCURRENCE, CountRecords);

    dbgResults.Datasource := FWizard.dmWizard.dsResults;

    if not Assigned(ACriteria) then
      // this was not the previous query with added filtering
      ResizeColumns;
    EnableActionsDependingOnAvailableData;
    UpdateSnapshotTemplateActions;
    dmFormActions.actSave.Enabled := True;
    frmMain.mnuFileSave.Enabled := True;
    frmMain.mnuFileSaveAs.Enabled := True;
  finally
    DefaultCursor(lCursor);
    frmMain.SetStatus('');
    frmMain.SetProgress(0);
  end;
end;  // UpdateResults

{-------------------------------------------------------------------------------
  Update the enabled state of the actions related to templates and snapshots
}
procedure TfrmFilterResult.UpdateSnapshotTemplateActions;
var
  lEnable: boolean;
begin
  lEnable := dbgResults.Datasource.Dataset.Recordcount>0;
  actRunSnapshot.Enabled := (SnapshotFile <> '') and lEnable and not FSettings.StandardReport;
  actEditSnapshot.Enabled := (SnapshotFile <> '') and lEnable and not FSettings.StandardReport;
  actNewSnapshot.Enabled := lEnable and (not FSettings.StandardReport) and Assigned(FWizard);
  actRunReportTemplate.Enabled := lEnable and ((TemplateFile <>'')or FSettings.StandardReport);
  actEditReportTemplate.Enabled := lEnable and (TemplateFile <> '') and not FSettings.StandardReport;
  actNewReportTemplate.Enabled := lEnable and not FSettings.StandardReport;

  actRunReportTemplate.Caption := ResStr_Cap_RunReportTemplate + ExtractFileName(TemplateFile);
  actEditReportTemplate.Caption := ResStr_Cap_EditReportTemp + ExtractFileName(TemplateFile);
  actRunSnapshot.Caption := ResStr_Cap_RunSnapshot + ExtractFileName(SnapshotFile);
  actEditSnapshot.Caption := ResStr_Cap_EditSnapshot + ExtractFileName(SnapshotFile);
end;

//==============================================================================
procedure TfrmFilterResult.bbFinishClick(Sender: TObject);
begin
  inherited;
  Close;
end;  // bbFinishClick


//==============================================================================
procedure TfrmFilterResult.bbBackClick(Sender: TObject);
begin
  inherited;
  PostMessage(Handle,WM_RUN_WIZARD,0,0);
end;  // bbBackClick

//==============================================================================
procedure TfrmFilterResult.PreviewFinished;
begin
  Enabled := True;
  pnlPreparingReport.Visible := False;
  dbgResults.Datasource.Dataset.First;
  dbgResults.Datasource.Dataset.EnableControls;
end;

//==============================================================================
procedure TfrmFilterResult.PreviewScreen;
begin
  PrintGrid(false);
end;  // PreviewScreen

//==============================================================================
procedure TfrmFilterResult.PrintScreen;
begin
  PrintGrid(true);
end;  // PrintScreen

//==============================================================================
procedure TfrmFilterResult.btnOutputClick(Sender: TObject);
var lPos: TPoint;
begin
  inherited;
  lPos := btnOutput.ClientToScreen(Point(0, btnOutput.Height));
  pmOutput.Popup(lPos.X, lPos.Y);
end;  // bbPrintClick

//==============================================================================
procedure TfrmFilterResult.pmGridClick(Sender: TObject);
begin
  inherited;
  if Printer.Printers.Count = 0 then begin

    MessageDlg(ResStr_NoPrinterAvailable, mtInformation, [mbOk], 0);
    Exit;
  end;
  if dmFormActions.dlgPrint.Execute then
    PrintGrid(true);
end;  // pmGridClick

//==============================================================================
procedure TfrmFilterResult.PrintGrid(const PrintIt:boolean);
begin
  with QRPrinter do begin
    Orientation:=Printer.Orientation;
    Destination:=qrdMetafile;
    CleanUp;
    BeginDoc;
    PrepareGridOutput;
    EndDoc;
    if PrintIt then Print
               else Preview;
  end;
end;  // PrintGrid

{ Show the report designer, and load a template if one is specified.
    Report is automatically previewed if the template is specified }
procedure TfrmFilterResult.ShowDesigner(iTemplate : string; itfPreview : Boolean = true );
begin
  if FreportDesigner=nil then  // Create only one designer
    FReportDesigner:=TfrmReportDesigner.Create(self);
  FReportDesigner.QuickReport.DataSet:=dbgResults.Datasource.Dataset;
  FReportDesigner.ShowDataFieldList;
  FReportDesigner.ReportFileName := iTemplate;
  FReportDesigner.Show;

  if (iTemplate <> '') then
  begin
    FReportDesigner.QRepDesigner.LoadReport(iTemplate);
    if itfPreview then begin
      // reset cursor because PreviewScreen does not return
      Screen.Cursor := crDefault;
      dbgResults.Datasource.Dataset.DisableControls;
      FReportDesigner.PreviewScreen;
    end;
  end;
end;


//==============================================================================
function TfrmFilterResult.GetKeyListForValidation: TKeyList;
begin
  Result := GetKeyList;
end;

function TfrmFilterResult.GetKeyList: TKeyList;
var
  lNewKeyList: TEditableKeyList;
  lBookMark : TBookMarkStr;
  lKey: string;

    procedure SetTableAndKey(const ATable, AKey: string);
    begin
      lNewKeyList.SetTable(ATable);
      lKey := AKey;
    end;

    procedure GetKeyListTable;
    begin
      if dbgResults.DataSource.Dataset.FieldList.IndexOf('Occurrence_Key')>-1 then
        SetTableAndKey(MIXED_DATA, 'Occurrence_Key')
      else if dbgResults.DataSource.Dataset.FieldList.IndexOf('Taxon_Occurrence_Key')>-1 then
        SetTableAndKey(TN_TAXON_OCCURRENCE, 'Taxon_Occurrence_Key')
      else if dbgResults.DataSource.Dataset.FieldList.IndexOf('Biotope_Occurrence_Key')>-1 then
        SetTableAndKey(TN_BIOTOPE_OCCURRENCE, 'Biotope_Occurrence_Key')
      else if dbgResults.DataSource.Dataset.FieldList.IndexOf('Sample_Key')>-1 then
        SetTableAndKey(TN_SAMPLE, 'Sample_Key')
      else if dbgResults.DataSource.Dataset.FieldList.IndexOf('Survey_Event_Key')>-1 then
        SetTableAndKey(TN_SURVEY_EVENT, 'Survey_Event_Key')
      else if dbgResults.DataSource.Dataset.FieldList.IndexOf('Survey_Key')>-1 then
        SetTableAndKey(TN_SURVEY, 'Survey_Key')
      else if dbgResults.DataSource.Dataset.FieldList.IndexOf('Location_Key')>-1 then
        SetTableAndKey(TN_LOCATION, 'Location_Key')
      else
        raise EWizard.CreateNonCritical(ResStr_NoDataToExport);
    end;

begin
  //Return an editable key list with the selected nodes key
  lNewKeyList:= TEditableKeyList.Create;
  GetKeylistTable;
  // Construct a Key List from the result set
  with dbgResults.DataSource.Dataset do begin
    // for each result set row
    DisableControls;
    lBookMark := BookMark;
    try
      First;
      if (FReportVersion = REPORT_VERSION_3) or (FReportVersion='') then
        while not Eof do begin
          // put the data into the Key List
          if lKey='Occurrence_Key' then
            // Occurrence key used, so must include specific table in keyfield2
            lNewKeyList.AddItem(FieldValues[lKey],
                IfThen(FieldValues['Type']= 'T', TN_TAXON_OCCURRENCE, TN_BIOTOPE_OCCURRENCE))
          else
            lNewKeyList.AddItem(FieldValues[lKey], '');
          Next;
        end
      else if FReportVersion = REPORT_VERSION_2 then
        while not Eof do begin
          // put the data into the Key List
          lNewKeyList.AddItem(Fields[0].Value, Fields[1].Value);
          Next;
        end;

    finally
      BookMark := lBookMark;
      EnableControls;
    end; // try .. finally
  end; // with
  Result:= lNewKeyList;
end;  // GetKeyList

//------------------------------------------------------------------------------
procedure TfrmFilterResult.GetNodeData(const Sender: TObject;
  var oDropSource: TJNCCDropSource);
var
  lKeyList : TKeyList;
  i : integer;
begin
  { if any data present }
  if not (dbgResults.DataSource.DataSet.Eof and dbgResults.DataSource.DataSet.Bof) then
  begin
    oDropSource.DropData.SetTable(MIXED_DATA);
    lKeyList := GetKeyList;
    for i := 0 to lKeyList.Header.ItemCount - 1 do
      oDropSource.DropData.AddItem(lKeyList.Items[i].KeyField1,lKeyList.Items[i].KeyField2);

  end;
end;  // GetNodeData

//==============================================================================
//==============================================================================
//  Print Section
//------------------------------------------------------------------------------
procedure TfrmFilterResult.PrintPageHeader (XMin,XMax:integer;
  PageHeaderText,PageNumber:string);
begin
  with QRPrinter.Canvas do begin
    Font := dbgResults.Font;
    TextOut (XMin,1,PageHeaderText);
    TextOut (XMax-TextWidth (PageNumber),1,PageNumber);

    MoveTo (XMin,TextHeight ('A')+1);
    LineTo (XMax,TextHeight ('A')+1);
  end;
end;  // PrintPageHeader

//==============================================================================
procedure TfrmFilterResult.PrintPageFooter (XMin,XMax,YMax:integer);
var Footer:string;
begin
  Footer:=Format(ResStr_PageFooter, [DateTimeToStr (Now)]);
  with QRPrinter.Canvas do begin
    Font := dbgResults.Font;
    TextOut (XMin,YMax-TextHeight('A')+2, Application.Title + ' © ' + FormatDateTime('yyyy', Now));
    TextOut (XMax-TextWidth (Footer), YMax-TextHeight ('A')+2, Footer);
    MoveTo (XMin,YMax-TextHeight ('A'));
    LineTo (XMax,YMax-TextHeight ('A'));
  end;
end;  // PrintPageFooter

//==============================================================================
procedure TfrmFilterResult.StartNewPage (XMin,XMax,YMax:integer;
  PageHeaderText,PageNumber:string);
begin
  QRPrinter.NewPage;
  PrintPageHeader (XMin,XMax,PageHeaderText,PageNumber);
  PrintPageFooter (XMin,XMax,YMax);
end;  // StartNewPage

//==============================================================================
procedure TfrmFilterResult.PrepareGridOutput;
const TopX=50;
      TopY=10;
      RightMargin =50;
      BottomMargin=15;
      MinimumBottomLines=3;  // Helps to leave gap for footer

var MaxWidth,MaxHeight,HoldCount,LineHeight:integer;
    CurX,CurY,IndexCol,IndexFirstCol,CurrentPage:integer;
    LineFont                 :TFont;
    FooterIndex              :word;
    stReportTopTitle         :string;
    FinishedSet              :boolean;
    mCellContent             :TMemo;
    MaxLines                 :integer;
    aRowHeights              :array [0..400] of smallint;

    lCount,
    lCurrentRow, lLastRow    :integer;
    lTitleWidth              :integer;
    lCursor                  :TCursor;
    liMaxRows                :integer;
    lBookMark                : TBookmarkStr;
//    ldsResults               :TJNCCDataset;
  //----------------------------------------------------------------------------
  //Print the title row of the grid on the top of each new page
  procedure PrintFirstRow;
  var lColIdx,lIdx:integer;

  begin
    with QRPrinter.Canvas do begin
      Font := dbgResults.TitleFont;
      mCellContent.Font := dbgResults.TitleFont;


      lColIdx := IndexFirstCol;

      while lColIdx<dbgResults.Columns.Count do begin
        if dbgResults.Columns[lColIdx].Visible then
        begin
          if CurX+dbgResults.Columns[lColIdx].Width>MaxWidth then break;
          // Get the text
          mCellContent.Text :=dbgResults.Columns[lColIdx].Field.DisplayLabel;
          mCellContent.Width:=dbgResults.Columns[lColIdx].Width-8;
          for lIdx:=0 to mCellContent.Lines.Count-1 do
            TextOut(CurX+4,CurY+LineHeight*lIdx,mCellContent.Lines[lIdx]);
          // Draw cell left border
          MoveTo(CurX, CurY-2);
          LineTo(CurX, CurY+LineHeight * liMaxRows +2);
          Inc(CurX, dbgResults.Columns[lColIdx].Width);
        end;
        Inc(lColIdx);
      end;
      mCellContent.Font:= dbgResults.Font;
      Font := dbgResults.Font;
      // Draw row right border
      MoveTo(CurX,CurY-2);
      LineTo(CurX,CurY+LineHeight * liMaxRows +2);
      // Draw row top border
      MoveTo(TopX,CurY-2);
      LineTo(CurX,CurY-2);

      CurX:=TopX;
      Inc (CurY,LineHeight * liMaxRows + 2);
    end;
  end;  // PrintFirstRow
  //----------------------------------------------------------------------------
begin
  lCursor:=HourGlassCursor;
  LineFont:=TFont.Create;
  LineFont.Assign(dbgResults.Font);
  Canvas.Font.Assign(LineFont);

  mCellContent:=TMemo.Create (nil);
  dbgResults.Datasource.DataSet.DisableControls;
  lBookMark := dbgResults.DataSource.DataSet.BookMark;
  try
    mCellContent.Visible:=false;
    mCellContent.Parent :=Self;
    mCellContent.Font.Assign (LineFont);
    mCellContent.Clear;

    with QRPrinter do begin
      // Get a canvas to work with, as it is not created until a new page is requested.
      NewPage;
      Canvas.Font.Assign(LineFont);
      // Set page limits
      MaxWidth :=XSize(PaperWidthValue)-RightMargin;
      MaxHeight:=YSize(PaperLengthValue)-BottomMargin;

      // Get the width of the title.
      LineHeight:=Canvas.TextHeight('A');

      Canvas.Font.Size :=LineFont.Size+4;
      Canvas.Font.Style:=[fsBold];
      if Assigned(FWizard) then
        stReportTopTitle :=FWizard.TitleText
      else
        stReportTopTitle := ResStr_PageTitle;
      lTitleWidth      :=Canvas.TextWidth(stReportTopTitle);

      //Get the height of the title
      mCellContent.Font := dbgResults.TitleFont;
      IndexCol  :=0;
      liMaxRows :=0;
      while IndexCol<dbgResults.Columns.Count do begin
        // Get the text
        if dbgResults.Columns[IndexCol].Visible then
        begin
          mCellContent.Text :=dbgResults.Columns[IndexCol].Field.DisplayLabel;
          mCellContent.Width:=dbgResults.Columns[IndexCol].Width-8;
          liMaxRows := Max(liMaxRows, mCellContent.Lines.Count);
        end;
        Inc(IndexCol);
      end;

      // Set CurX and CurY for calculation on first page. See below, with HoldCount
      CurX:=TopX;
      CurY:=TopY+ LineHeight * liMaxRows+Canvas.TextHeight ('A')*2;
      Canvas.Font.Size :=LineFont.Size;
      Canvas.Font.Style:=[];

      CurrentPage:=1;
      IndexCol   :=0;
      FooterIndex:=1;

      // Find out if first page needs a '.1' or not, i.e. if all columns fit on 1 page
      HoldCount:=0;
      for lCount:=0 to dbgResults.Columns.Count-1 do Inc (HoldCount,IfThen(dbgResults.Columns[lCount].Visible,dbgResults.Columns[lCount].Width, 0));
      // And then set the first page header and footer
      PrintPageFooter (TopX,MaxWidth,MaxHeight);
      if HoldCount<=MaxWidth then
        PrintPageHeader (TopX,MaxWidth,stReportTopTitle,'Page 1')
      else
        PrintPageHeader (TopX,MaxWidth,stReportTopTitle,'Page 1.1');

      // Get the height of the column headers row

      aRowHeights[0]:=LineHeight*liMaxRows + 2;
      mCellContent.Font.Style:=[];

      dbgResults.DataSource.DataSet.First;
      lLastRow := dbgResults.DataSource.DataSet.RecNo;
      while (not dbgResults.DataSource.Dataset.EOF) do begin
        // Reset sgBuffer with next batch of records, and "load" a page
        HoldCount:=CurY;  // CurY is ready for a new page but is used here to grab only
                          // the amount of records printable on the new page
        // Determine the area that fits on one page
        lCurrentRow := lLastRow;
        if not dbgResults.DataSource.Dataset.BOF then dbgResults.DataSource.Dataset.Prior;
        while (CurY+(LineHeight+2)*MinimumBottomLines<MaxHeight-LineHeight-2) and
              (not dbgResults.DataSource.Dataset.EOF) do
        begin
          MaxLines:=1;
          for lCount:=0 to dbgResults.Columns.Count-1 do
            if dbgResults.Columns[lCount].Visible then
            begin
              mCellContent.Text :=dbgResults.Columns[lCount].Field.AsString;
              mCellContent.Width:=dbgResults.Columns[lCount].Width-8;  // 8 for cell gap on either side
              if MaxLines<mCellContent.Lines.Count then MaxLines:=mCellContent.Lines.Count;
            end;
          aRowHeights[dbgResults.DataSource.DataSet.RecNo-lCurrentRow]:=LineHeight*MaxLines;
          Inc(CurY,LineHeight*MaxLines+2);
          dbgResults.DataSource.DataSet.Next;
        end; // while page not filled, keep reading the rows
        CurY:=HoldCount;
        CurX:=TopX;
        FinishedSet:=false;
        lLastRow := dbgResults.DataSource.DataSet.RecNo;
        if dbgResults.Datasource.dataset.Eof then Inc(llastRow);

        // if first time round, first page, print Title
        if (CurrentPage=1) then begin
          Canvas.Font.Size :=LineFont.Size+4;
          Canvas.Font.Style:=[fsBold];
          Canvas.TextOut (TopX+((MaxWidth-TopX)-lTitleWidth) div 2,
                          TopY+LineHeight,stReportTopTitle);
          CurY:=TopY+LineHeight+Canvas.TextHeight ('A')*2;
          IndexFirstCol:=0;
          FooterIndex  :=1;
          // print title row, it is done when changing page in the rest of the loop
          PrintFirstRow;
        end;

        // page full, move it to the printer
        Canvas.Font := dbgResults.Font;
        dbgResults.DataSource.DataSet.MoveBy(lCurrentRow - lLastRow);
        while not FinishedSet do begin
          dbgResults.DataSource.Dataset.RecNo := lCurrentRow;
          While (dbgResults.DataSource.DataSet.RecNo < lLastRow) and not dbgResults.Datasource.DataSet.EOF do
          begin
            IndexCol:=IndexFirstCol;
            CurX    :=TopX;
            while IndexCol<dbgResults.Columns.Count do
            begin
              if dbgResults.Columns[IndexCol].Visible then
              begin
                if CurX+dbgResults.Columns[IndexCol].Width>MaxWidth then break;
                mCellContent.Text :=dbgResults.Columns[IndexCol].Field.AsString;
                mCellContent.Width:=dbgResults.Columns[IndexCol].Width-8;

                for lCount:=0 to mCellContent.Lines.Count-1 do
                  Canvas.TextOut(CurX+4,CurY+LineHeight*lCount,mCellContent.Lines[lCount]);

                Canvas.MoveTo(CurX,CurY-2);
                Canvas.LineTo(CurX,CurY+aRowHeights[dbgResults.DataSource.DataSet.RecNo-lCurrentRow]);
                Inc(CurX,dbgResults.Columns[IndexCol].Width);
              end;
              Inc(IndexCol);
            end;
            Canvas.MoveTo(CurX,CurY-2);
            Canvas.LineTo(CurX,CurY+aRowHeights [dbgResults.DataSource.DataSet.RecNo-lCurrentRow]);
            Canvas.MoveTo(TopX,CurY-2);
            Canvas.LineTo(CurX,CurY-2);

            Inc (CurY,aRowHeights[dbgResults.DataSource.DataSet.RecNo-lCurrentRow]+2);
            dbgResults.DataSource.DataSet.Next;
          end;

          Canvas.MoveTo(TopX,CurY-2);
          Canvas.LineTo(CurX,CurY-2);
          if (IndexCol<dbgResults.Columns.Count) or
             (not dbgResults.Datasource.DataSet.EOF) then
          begin
            if IndexCol<dbgResults.Columns.Count then begin // rows are not completely printed
              IndexFirstCol:=IndexCol; // Update FirstCol for next page
              CurX:=TopX;
              CurY:=TopY+LineHeight;
              Inc(FooterIndex);
              StartNewPage (TopX,MaxWidth,MaxHeight,
                            stReportTopTitle,'Page '+IntToStr (CurrentPage)+'.'+IntToStr (FooterIndex));
              if CurrentPage=1 then begin
                Canvas.Font.Size :=LineFont.Size+4;
                Canvas.Font.Style:=[fsBold];
                Canvas.TextOut (TopX+((MaxWidth-TopX)-lTitleWidth) div 2,
                                TopY+LineHeight,stReportTopTitle);
                CurY:=TopY+LineHeight+Canvas.TextHeight ('A')*2;
              end;
              // First row always on top of page
              PrintFirstRow;
            end else begin // finished and ready for new set
              FinishedSet:=true;  // set while stop condition
              if  not dbgResults.DataSource.DataSet.EOF then dbgResults.DataSource.DataSet.Next;
              // Stop new page if no more pages to output
              if not dbgResults.DataSource.DataSet.EOF then begin
                IndexFirstCol:=0;
                CurX:=TopX;
                CurY:=TopY+LineHeight;
                Inc (CurrentPage);
                if FooterIndex=1 then
                  StartNewPage (TopX,MaxWidth,MaxHeight,
                                stReportTopTitle,'Page '+IntToStr (CurrentPage))
                else
                  StartNewPage (TopX,MaxWidth,MaxHeight,
                                stReportTopTitle,'Page '+IntToStr (CurrentPage)+'.1');
                FooterIndex:=1;
                // First row always on top of page
                PrintFirstRow;
              end;
            end;
          end else
            FinishedSet:=true;  // While stop condition
        end;  // while not FinishedSet
      end; // while still rows to print
    end;  // with QRPrinter
  finally
    LineFont.Free;
    mCellcontent.Free;
    dbgResults.DataSource.Dataset.Bookmark := lBookmark;
    dbgResults.DataSource.DataSet.EnableControls;
    DefaultCursor(lCursor);
  end;
end;  // PrepareGridOutput


{===============================================================================
 Description : Copy the contents of the currently selected
               field to the clipboard
 Created : 12/03/2010 }
procedure TfrmFilterResult.actResultCopyExecute(Sender: TObject);
var
  lRtf: TRichEdit;
  lStream: TMemoryStream;
  lField: TField;
begin
  inherited;
  // Memory stream for when copying content of blob fields.
  lStream := TMemoryStream.Create;
  // Create temporary RichEdit to be able to extract plain text only.
  lRtf := TRichEdit.Create(nil);
  with dbgResults.Datasource.Dataset do begin
    DisableControls;
    lRtf.Visible := False;  // RichEdit control requires Parent set. But don't show it.
    lRtf.Parent := Self;
    lField := dbgResults.SelectedField;
    try
      if lField.IsBlob then
      begin
      // Get the content of the field in MemoryStream.
        lStream.Clear;
        TBlobField(lField).SaveToStream(lStream);
        lStream.Position := 0;
        // And transfer to temp RichEdit.
        lRtf.Lines.LoadFromStream(lStream);
        // And get the plain text
        Clipboard.AsText := AnsiReplaceStr(lRtf.Lines.Text, #13#10, '');
      end
      else Clipboard.AsText := dbgResults.SelectedField.Text;
    finally
      EnableControls;
    end;
  end;
end;  // actResultCopyExecute

{===============================================================================
 Description : Copy the contents of the query to the clipboard
 Created : 05/12/2002 }
procedure TfrmFilterResult.actResultCopyAllExecute(Sender: TObject);
var lBuffer:string;

  function GetFieldHeaders : string;
  var i: Integer;
  begin
    Result := '';
    with dbgResults.Datasource.Dataset do
      for i := 0 to Fields.Count - 1 do
        if Fields[i].Visible then
          Result := Result  + '"' + DuplicateCharacters(StringReplace(Fields[i].DisplayName, #9, ' ', [rfReplaceAll]), '"') + '"'#9;
    // Lop off last tab.
    if Result <> '' then
      Result := Copy(Result, 1, Length(Result) - 1);
  end;

  function GetRowString : string;
  var i: Integer;
      lRtf: TRichEdit;
      lStream: TMemoryStream;
  begin
    // Memory stream for when copying content of blob fields.
    lStream := TMemoryStream.Create;
    // Create temporary RichEdit to be able to extract plain text only.
    lRtf := TRichEdit.Create(nil);
    try;
      lRtf.Visible := False;  // RichEdit control requires Parent set. But don't show it.
      lRtf.Parent := Self;
      Result := '';
      with dbgResults.Datasource.Dataset do
        for i := 0 to Fields.Count - 1 do
          if Fields[i].Visible then
            if Fields[i].IsBlob then begin
              // Get the content of the field in MemoryStream.
              lStream.Clear;
              TBlobField(Fields[i]).SaveToStream(lStream);
              lStream.Position := 0;
              // And transfer to temp RichEdit.
              lRtf.Lines.LoadFromStream(lStream);
              // And get the plain text, skipping the terminating CR-LF characters.
              Result := Result + '"' +
                        DuplicateCharacters(Copy(lRtf.Lines.Text, 1, Length(lRtf.Lines.Text) - 2), '"') +
                        '"'#9;
            end else
              Result := Result + '"' + DuplicateCharacters(Fields[i].Text, '"') + '"'#9;
      // lop off last tab
      if Result <> '' then
        Result := Copy(Result, 1, Length(Result) - 1);
    finally
      lStream.Free;
      lRtf.Free;
    end;
  end;

begin
  inherited;
  with dbgResults.Datasource.Dataset do begin
    DisableControls;
    try
      lBuffer := GetFieldHeaders;
      First;
      while not Eof do begin
        lBuffer := lBuffer + #13#10 + GetRowString;
        Next;
      end;
      First;
      Clipboard.AsText := lBuffer;
    finally
      EnableControls;
    end; //try
  end;
end;  // actResultCopyAllExecute

//==============================================================================
procedure TfrmFilterResult.FormResize(Sender: TObject);
begin
  inherited;
  if FAccessingMapFromWizard then begin
    WindowState:=wsMinimized;
    TfrmMap(frmMain.GetForm(TfrmMap)).BringToFront;
  end;
end;  // FormResize


//==============================================================================

procedure TfrmFilterResult.DisplaySelectedName(const ANameKey:TKeyString);
var lKeyList:TEditableKeyList;
begin
  if ANameKey<>'' then begin
    lKeyList:=TEditableKeyList.Create;
    try
    //We don't know whether this is an individual or an organisation
    // so add to both.
      lKeyList.AddItem(ANameKey,'Individual');
      lKeyList.AddItem(ANameKey, 'Organisiation');
      if dmFormActions.actNames.Execute then
        TfrmIndOrg(frmMain.GetForm(TfrmIndOrg)).DisplayNames(lKeyList);
    finally
      lKeyList.Free;
    end;
  end;
end;  // DisplayNames
//==============================================================================

procedure TfrmFilterResult.DisplaySelectedLocation(const ASampleKey: TKeyString);
var lKey    :TKeyString;
    lKeyList:TEditableKeyList;
begin
  With  dmGeneralData.qryAllPurpose do
  begin
    SQL.Text := 'Select Location_Key from Sample where Sample_Key =' + QuotedStr(ASampleKey);
    Open;
    try
      lKey := Fields[0].AsString;
    finally
      Close;
    end;
  end;

  if lKey<>'' then begin
    lKeyList:=TEditableKeyList.Create;
    try
      lKeyList.AddItem(lKey,'');
      if dmFormActions.actLocations.Execute then
        TfrmLocations(frmMain.GetForm(TfrmLocations)).DisplayLocations(lKeyList);
    finally
      lKeyList.Free;
    end;
  end;
end;  // DisplaySelectedLocation
//==============================================================================

procedure TfrmFilterResult.DisplaySelectedOccurrence(const ObsType:TRelatedData;
  const AKeyData:TKeyData);
var lKeyList:TEditableKeyList;
    lKey    :TKeyString;
    lAddit  :string;
begin
  lKeyList:=TEditableKeyList.Create;
  try
    lKey  :=AKeyData.ItemKey;
    lAddit:=AKeyData.ItemAdditional;
    // Put Key and additional bit in keylist and display the observation
    lKeyList.AddItem(lKey, lAddit);
    with TfrmObservations(dmFormActions.DisplayForm(TfrmObservations)) do begin
      DisplayObservations(ObsType, lKeyList);
      if Assigned(tvObservations.Selected) then
        with tvObservations do
          case ObsType of
            rdEvent:
                begin
                  // Expand Survey node and select first Event node that should be the only one there.
                  Selected.Expand(false);
                  Selected := Selected.GetFirstChild;
                end;
            rdSample:
                begin
                  // Expand Survey node and select first Event node that should be the only one there.
                  Selected.Expand(false);
                  Selected := Selected.GetFirstChild;
                  if Assigned(Selected) then begin
                    // Expand Event node and select first Sample node that should be the only one there.
                    Selected.Expand(false);
                    Selected := Selected.GetFirstChild;
                  end;
                end;
            rdOccurrence:
                begin
                  // Expand Survey node all the way and select the Occurrence node that should be the only one there.
                  Selected.Expand(true);
                  while Assigned(Selected.GetFirstChild()) do
                    Selected := Selected.GetFirstChild;
                end;
          end;
    end;
  finally
    lKeyList.Free;
  end;
end;  // DisplaySelectedOccurrence

{===============================================================================
 Description : Change cursor to handpointer when over a cell in the grid, and
               show a tool tip over the filter image.
 Created : 5/12/2002 }
procedure TfrmFilterResult.dbgResultsMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  lMouseCell: TGridCoord;
  lColumn: TReportColumn;
  lCursor: TCursor;
  SizeRect, CellRect: TRect;
begin
  inherited;

  lMouseCell := dbgResults.MouseCoord(X, Y);

  if (lMouseCell.X = 0) and (lMouseCell.Y = 0) then
  begin
    dbgResults.Cursor := crDefault;
    FtfMouseOverCell := false;

    if not Assigned(FHintWindow) then
      FHintWindow := THintWindow.Create(Self);

    SizeRect := FHintWindow.CalcHintRect(1024, SFilterResult_ToolTip, nil);

    // Create the rectangle for the tool tip's size and position.
    with CellRect do
    begin
      Left := Mouse.CursorPos.X + 20;
      Top := Mouse.CursorPos.Y;
      Right := Left + SizeRect.Right;
      Bottom := Top + SizeRect.Bottom;
    end;

    FHintWindow.ActivateHint(CellRect, SFilterResult_ToolTip);
  end else
  begin
    // Hide the filter tool tip when it's not needed.
    if Assigned(FHintWindow) then
      FHintWindow.ReleaseHandle;

    if (lMouseCell.X > 0) and (lMouseCell.Y > 0) then
    begin
      if Assigned(FCRReportFile) then
      begin
        try
          lCursor := crDefault; // default
          try
            lColumn := FCRReportFile.ReportColumns.ColumnByName(
                dbgResults.Columns[lMouseCell.X-1].Field.FieldName);
            if lColumn.KeyColumn<>'' then
              lCursor := crHandPoint;
          except
            on EReportColumnsException do ; // ignore - no column specified
          end;
          if lCursor=crDefault then
            if IsHyperlink(GetCellText(lMouseCell.X, lMouseCell.Y)) then
              lCursor := crHandPoint;
          dbgResults.Cursor := lCursor;
        except
          on Exception do
            dbgResults.Cursor := crDefault; // no column details so no hotlink
        end;
      end
      else
        dbgResults.Cursor := crHandPoint;
      FtfMouseOverCell:=true;
    end else
    begin
      dbgResults.Cursor := crDefault;
      FtfMouseOverCell:=false;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Retrieve the contents of a given cell on the grid
}
function TfrmFilterResult.GetCellText(X, Y: integer): string;
var
  lOldRow: Integer;
  lCol: integer;
begin
  if dgIndicator in dbgResults.Options then
    lCol := X-1
  else
    lCol := X;
  with TDBGridAccessor(dbgResults) do begin
    if (lCol<0) or (lCol>=Columns.Count) or (lCol<1) or (lCol>Datalink.RecordCount) then
      Exit;
    // record the current row and switch to the one hovered over
    lOldRow := Datalink.ActiveRecord;
    Datalink.ActiveRecord := Y-1;
    try
      Result := dbgResults.Datasource.Dataset.Fields[lCol].Displaytext;
    finally
      Datalink.ActiveRecord := lOldRow;
    end; // try
  end;
end;

{===============================================================================
 Description : Send the output of the report to the map
 Created : 06/12/2002 }
procedure TfrmFilterResult.SendToMapClick(Sender: TObject);
begin
  inherited;
  dmFormActions.MapWindowMenuClick(Sender);
  // Pass the connection to the map, so it can read the spatial references
  // directly
  TfrmMap(frmMain.ActiveMDIChild).FilterDropped(FConnection);
end;

{-------------------------------------------------------------------------------
}
procedure TfrmFilterResult.actNewSnapshotExecute(Sender: TObject);
begin
  inherited;
  ShowSnapshot('');
end;

procedure TfrmFilterResult.actEditSnapshotExecute(Sender: TObject);
begin
  inherited;
  ShowSnapshot(SnapshotFile, False);
end;

procedure TfrmFilterResult.actRunSnapshotExecute(Sender: TObject);
begin
  inherited;
  frmMain.SetStatus(ResStr_RunningSnapshot);
  try
    ShowSnapshot(SnapshotFile)
  finally
    frmMain.SetStatus('');
  end;
end;

{-------------------------------------------------------------------------------
}
procedure TfrmFilterResult.actPrintGridExecute(Sender: TObject);
begin
  inherited;
  PrintGrid(True);
end;

procedure TfrmFilterResult.actNewReportTemplateExecute(Sender: TObject);
begin
  inherited;
  ShowDesigner('');
end;

procedure TfrmFilterResult.actEditReportTemplateExecute(Sender: TObject);
begin
  inherited;
  ShowDesigner(TemplateFile, false);
end;

function TfrmFilterResult.GetTemplateFile: string;
begin
  Result := FSettings.TemplateFile;
end;

procedure TfrmFilterResult.SetTemplateFile(const Value: string);
var
  lStandardReport: boolean;
begin
  FSettings.TemplateFile := Value;
  lStandardReport := FSettings.StandardReport;
  if lStandardReport then
  begin
    actRunReportTemplate.Enabled := true;
    actRunReportTemplate.Caption := ResStr_Cap_RunReportTemplate;
    actEditReportTemplate.Enabled := false;
    actEditReportTemplate.Caption := ResStr_Cap_EditReportTemp;
    actNewReportTemplate.Enabled := false;
    SnapshotFile := '';
    actNewSnapshot.Enabled := false;
  end
  else
  begin
    actRunReportTemplate.Enabled :=  (Value <>'');
    actEditReportTemplate.Enabled := (Value <> '');
    actEditReportTemplate.Caption := ResStr_Cap_EditReportTemp + DuplicateCharacters(ExtractFileName(Value), '&');
    actRunReportTemplate.Caption := ResStr_Cap_RunReportTemplate + DuplicateCharacters(ExtractFileName(Value), '&');
    actNewReportTemplate.Enabled := true;
  end;
end;

{-------------------------------------------------------------------------------
}
procedure TfrmFilterResult.actRunReportTemplateExecute(Sender: TObject);
begin
  inherited;
  frmMain.SetStatus(ResStr_OpeningReportTemp);
  try
    if FSettings.StandardReport then
      //Run a standard report
      with TfrmReports.Create(Self) do
        if ReportFile = ResStr_PlacesForOccurrences then
          dmFormActions.actPlacesForOccurrencesReportExecute(nil)
        else
          dmFormActions.actOccurrencesForPlacesReportExecute(nil)
    else
      ShowDesigner(TemplateFile, True);
  finally
    frmMain.SetStatus('');
  end;
end;

{-------------------------------------------------------------------------------
}
function TfrmFilterResult.GetSnapshotFile: string;
begin
  Result := FSettings.SnapshotFile;
end;

procedure TfrmFilterResult.SetSnapshotFile(const Value: string);
begin
  actEditSnapshot.Enabled := (Value <> '');
  actRunSnapshot.Enabled := (Value <> '');
  actEditSnapshot.Caption := ResStr_Cap_EditSnapshot + DuplicateCharacters(ExtractFileName(Value), '&');
  actRunSnapshot.Caption := ResStr_Cap_RunSnapshot + DuplicateCharacters(ExtractFileName(Value), '&');
  FSettings.SnapshotFile := value;
end;

//This function shows the snapshot that from the file specified.
//If the file is not an empty string and itfRun is true then the snapshot is run.
procedure TfrmFilterResult.ShowSnapshot(iSnapshot: string; itfRun: Boolean);
var
  lCursor: TCursor;
begin
  if FSnapshot=nil then  // Create only one TfrmSnapshot
  begin
    lCursor := HourglassCursor;
    try
      if iSnapshot <> '' then
      begin
        if itfRun then
          try
            FSnapshot:=TfrmSnapshot.CreateAndRun(Self, FConnection,
                                                 FWizard.dmWizard.ReportGenerator.Attributes,
                                                 SnapshotFile);
            FSnapshot.Close;
            FSnapshot :=nil;
          except on TSnapshotException do
             If Assigned(FSnapshot) then FSnapshot.Show;
          end
        else
        begin
          FSnapshot := TfrmSnapshot.CreateAndLoad(Self, FConnection,
                                                  FWizard.dmWizard.ReportGenerator.Attributes,
                                                  SnapshotFile);
          FSnapshot.Show;
        end
      end
      else
      begin
        FSnapshot := TfrmSnapshot.Create(Self, FConnection,
                                         FWizard.dmWizard.ReportGenerator.Attributes);
        FSnapshot.Show;
      end;
    finally
      DefaultCursor(lCursor);
    end;
  end;
end;

{-------------------------------------------------------------------------------
 This function runs a report given a certain file name. If a template file is
 specified in the report file them the report is displayed. If a snapshot file is
 specified then the snapshot is run.
}
procedure TfrmFilterResult.RunReport(AFileName : String;
              AKeyType: TKeyType=ktDefault; const AKey: string='');
begin
  // As we're running a saved report, set the flag.
  FReportSaved := True;
  
  pnlPreparingReport.Caption := ResStr_PreparingReport + '...';
  pnlPreparingReport.Visible := True;
  pnlPreparingReport.Repaint;
  if SameText(ExtractFileExt(AFileName), '.xml') then begin
    RunCustomReport(AFileName, AKeyType, AKey);
    pnlPreparingReport.Visible := False;
  end else
    RunReportWithFilter(AFileName, nil);
end;

{-------------------------------------------------------------------------------
  Run a custom XML report
}
procedure TfrmFilterResult.RunCustomReport(const AFileName: string; AKeyType:
    TKeyType; const AKey: string);
var
  lXMLDoc: IXMLDocument;
  lDocNode: IXMLNode;
begin
  FCustomReport := true;

  FWindowState := WindowState;
  WindowState := wsMinimized;
  lXMLDoc := NewXMLDocument;
  try
    if ExtractFilePath(AFileName)='' then
      lXMLDoc.LoadFromFile(AppSettings.ReportPath + AFileName)
    else
      lXMLDoc.LoadFromFile(AFileName);
  except
    on E:EDOMParseError do
      raise EWizard.CreateNonCritical(ResStr_XMLReportDefProblem + E.Message);
  end; // try
  lDocNode := lXMLDoc.ChildNodes[EL_CUSTOMREPORT];
  if not Assigned(FConnection) then begin
    // create a connection if we haven't used the wizard to give us a connection
    FConnection := TADOConnection.Create(Self);
    FConnection.LoginPrompt := False;
    FConnection.CommandTimeout := 0;
    dmDatabase.SetConnectionToMainDB(FConnection);
    FConnection.Open;
    dmDatabase.SetApplicationRole(FConnection);
    qryCustomReport.Connection := FConnection;
    qryCustomReport.ParseSQL := False;
  end;
  FCRReportFile := TReportFile.Create(FConnection);

  FCRReportFile.ReadXml(lDocNode);
  FCRReportFile.KeyType := AKeyType;
  FCRReportFile.ItemKey := AKey;

  if frmMain.ActiveMDIChild is TfrmMap then begin
    if AKeyType = ktSamplesInPolygon then
       TfrmMap(frmMain.ActiveMDIChild).ReadSamplesInPolygon(
           FConnection,
           FCRReportFile.CurrentWhereClause.PartialOverlap)
    else
    if AKeyType = ktLocationsInPolygon then
      TfrmMap(frmMain.ActiveMDIChild).ReadLocationsInPolygon(
          FConnection,
          FCRReportFile.CurrentWhereClause.PartialOverlap);
  end;
  FCRReportFile.BuildTableForReport(qryCustomReport, DisplayGridCallback, CancelReportCallback);
  EnableActionsDependingOnAvailableData;
end;

procedure TfrmFilterResult.SetCountLabel(const ALabel: string; ACount: integer);
begin
  if SameText(ALabel, ST_OCCURRENCE) then
    case ACount of
      0: lblOccCount.Caption := SFilterResult_NoOccurrences;
      1: lblOccCount.Caption := SFilterResult_OneOccurrence;
      else
        lblOccCount.Caption := Format(SFilterResult_ManyOccurrences, [IntToStr(ACount)]);
    end
  else
    case ACount of
      0: lblOccCount.Caption := SFilterResult_NoRecords;
      1: lblOccCount.Caption := SFilterResult_OneRecord;
      else
        lblOccCount.Caption := Format(SFilterResult_ManyRecords, [IntToStr(ACount)]);
    end;
  lblOccCount.Visible := true;
end;

{-------------------------------------------------------------------------------
  Description : Runs a report file.  If the filter is supplied, then replaces
      the reports normal filter with the new one
  Created : 11/03/2003 }
procedure TfrmFilterResult.RunReportWithFilter(const AFileName : string;
                                        ASQLProviderIntf : IProvidesOccurrencesSQL);
var lCursor : TCursor;
    lOccTypes : string;
    lReportType : string;

  procedure GetSamplesFromPolygons(polygonList: TObjectList);
  var
    i: Integer;
    mapServerLink: TMapServerLink;
    scanner: TPolygonScanner;
    polygon: TPolygonInfo;
  begin
    mapServerLink := TMapServerLink.Create(nil);
    try
      for i := 0 to polygonList.Count - 1 do begin
        polygon := TPolygonInfo(polygonList[i]);
        if not Assigned(AppSettings.AvailableMaps.ItemsByKey[polygon.MapKey]) then
          raise EWizard.CreateNonCritical(ResStr_InvalidMapKey);

        scanner := TPolygonScanner.Create(
            mapServerLink.MapHandle,
            AppSettings.AvailableMaps.ItemsByKey[polygon.MapKey].SpatialSystem);
        try
          mapServerLink.ActiveDataset := polygon.MapKey + '.gds';
          scanner.GetSamplesForPolygon(
              mapServerLink.SheetIDByFileName(
                  AppSettings.ObjectSheetFilePath + polygon.LayerKey + '.gsf'),
              polygon.PolygonId,
              FWizard.dmWizard.connReport,
              FSettings.PolygonIncludeOverlap);
        finally
          scanner.Free;
        end;
      end;
    finally
      mapServerLink.Free;
    end;
  end;

begin
  lCursor := HourglassCursor;
  try
    if not Assigned(FWizard) then begin
      FWizard := TdlgWizard.Create(self, FSettings);
      FConnection := FWizard.dmWizard.connReport;
    end;
    try
      if ExtractFileDir(AFileName) = '' then
        FWizard.LoadWizardFile(IncludeTrailingPathDelimiter(Appsettings.ReportPath) + AFileName)
      else
        FWizard.LoadWizardFile(AFileName);
      if Assigned(ASQLProviderIntf) then begin
        // find which occurrence types we want to request by looking at the type column
        lOccTypes := '';
        // checks must be case insensitive - see Mantis 146 comment 411
        if Pos('''t'' as type', Lowercase(FSettings.FilterSQL))>0 then
          lOccTypes := lOccTypes + 'T';
        if Pos('''b'' as type', Lowercase(FSettings.FilterSQL))>0 then
          lOccTypes := lOccTypes + 'B';
        FSettings.FilterSQL := ASQLProviderIntf.OccurrencesSQL[lOccTypes];
        if FSettings.FilterSQL = '' then begin
          if (Pos('T', lOccTypes) >0) and (Pos('B', lOccTypes)>0) then lReportType := 'Place'
          else if Pos('T', lOccTypes) >0 then lReportType := 'Taxon'
          else lReportType := 'Biotope';
          raise EWizard.CreateNonCritical(Format(ResStr_IncompatibleReport, [
                   lReportType]));
        end;
      end;
      FReportVersion := REPORT_VERSION_3;

      if FWizard.dmWizard.ReportGenerator.PolygonSelection.count > 0 then
        GetSamplesFromPolygons(FWizard.dmWizard.ReportGenerator.PolygonSelection);

      UpdateResults;
    except
      on E: EWizard do
        if E.MEssage = ResStr_OldReportFile then
          LoadV2WizardFile(AFileName)
        else
          raise;
    end;
  finally
    DefaultCursor(lCursor);
  end;

  if (TemplateFile <> '') and (SnapshotFile = '') then
    actRunReportTemplateExecute(nil)
  else if (TemplateFile = '') and (SnapshotFile <> '') then
    actRunSnapshotExecute(nil)
  else if (TemplateFile <> '') and (SnapshotFile <> '') then
    with TdlgRunReportOptions.Create(nil) do
      try
        if ShowModal = mrOK then
          case rgOptions.ItemIndex of
            OPT_TEMPLATE_INDEX: actRunReportTemplateExecute(nil);
            OPT_SNAPSHOT_INDEX: actRunSnapshotExecute(nil);
          end;
      finally
        Free;
      end;
  pnlPreparingReport.Visible := False;
end;

{-------------------------------------------------------------------------------
}
function TfrmFilterResult.SaveReport: Boolean;
begin
  if ReportFile = '' then
    Result := SaveReportAs
  else
    Result := SaveReport(ReportFile);
  FReportSaved := FReportSaved or Result;
end;

{-------------------------------------------------------------------------------
}
function TfrmFilterResult.SaveReportAs: Boolean;
begin
  inherited;
  // If no template selected, ask if the user wants to carry on or not
  if ((TemplateFile <> '') or (SnapshotFile <> '')) or
     (((TemplateFile = '') and (SnapshotFile = '')) and
      (MessageDlg(ResStr_ReportDisplay, mtInformation, [mbOk, mbCancel], 0) = mrOk)) then
  begin
    if FileExists(ReportFile) then
      dlgSave.FileName := ReportFile
    else begin
      dlgSave.InitialDir := AppSettings.ReportPath;
      dlgSave.FileName := 'Untitled.wzd';
    end;

    if dlgSave.Execute then
      Result := SaveReport(dlgSave.FileName)
    else
      Result := False;
  end else
    Result := False;
end;  // SaveResultAs

{-------------------------------------------------------------------------------
}
function TfrmFilterResult.SaveReport(AFilename: String): Boolean;
var
  liIndex    : Integer;
  lXMLDoc : IXMLDocument;
  lXMLNode : IXMLNode;
  lXMLAttributeNode : IXMLNode;
  lFile : TextFile;
begin
  // Format the filename, path and extension.
  if ExtractFilePath(AFileName) = '' then
    AFileName := IncludeTrailingPathDelimiter(AppSettings.ReportPath) + AFileName;
  if ExtractFileExt(AFileName) = '' then
    AFileName := AFileName + '.wzd';

  if FReportVersion = REPORT_VERSION_3 then
  begin
    lXMLDoc := NewXMLDocument;
    lXMLDoc.AddChild('Report');
    With lXMLDoc.DocumentElement do
    begin
      lXMLNode := AddChild('SQL');
      lXMLNode.Text:=FSettings.FilterSQL;
      FSettings.SaveAdditionalFilters(
          lXMLDoc.DocumentElement,
          FWizard.dmWizard.ReportGenerator.PolygonSelection);
      lXMLNode := AddChild('Attributes');
      for liIndex := 0 to FWizard.dmWizard.ReportGenerator.Attributes.Count -1 do
        with TAttribute(FWizard.dmWizard.ReportGenerator.Attributes.Objects[liIndex]) do
          if Selected or (Sort <>ReportGenerator.stNone) then
          begin
            lXMLAttributeNode := lXMLNode.AddChild('Attribute');
            lXMLAttributeNode.Attributes['visible'] := Selected;
            lXMLAttributeNode.Attributes['type'] :=
                    IfThen((AttrType=atAttribute),   'Standard',
                    IfThen((AttrType=atMeasurement), 'Measurement',
                                                     'Designation'));
            lXMLAttributeNode.Attributes['key'] := Key;
            //storing the position of this column
            lXMLAttributeNode.Attributes['position'] := GetColumnIndex(Name);
            if AttrType= atMeasurement then
              lXMLAttributeNode.Attributes['context']:= MeasurementContextTable
            else if AttrType = atDesignation then
              lXMLAttributeNode.Attributes['parameters']:= DesignationParameters;
            //Sorting
            if (Sort <> ReportGenerator.stNone) then
            begin
              lXMLAttributeNode.Attributes['index'] := IntToStr(SortOrder);
              if Sort = stDesc then
                lXMLAttributeNode.Attributes['direction'] := 'desc';
            end;
          end;

      if (SnapshotFile<>'') or (TemplateFile <>'') then
      begin
        lXMLNode := AddChild('Output');
        if SnapshotFile<>'' then
        begin
          lXMLAttributeNode := lXMLNode.AddChild('Snapshot');
          lXMLAttributeNode.Attributes['file'] := SnapshotFile;
        end;
        if TemplateFile<>'' then
        begin
          lXMLAttributeNode := lXMLNode.AddChild('Template');
          lXMLAttributeNode.Attributes['file'] := TemplateFile;
        end;
      end;
    end;//with XMLDoc.ChildNode;
    lXMLDoc.SaveToFile(AFileName);
  end
  else
  begin //version 2 saving;
    if MessageDlg('This report will be saved in Recorder 2000 format.',
                  mtInformation, [mbOK, mbCancel], 0) = mrOK  then
    begin
      AssignFile(lFile, AFileName);
      try
        ReWrite(lFile);
        if TemplateFile <> '' then
        begin
          WriteLn(lFile, '<TEMPLATE>' );
          Writeln(lFile, TemplateFile);
          Writeln(lFile, '</TEMPLATE>');
        end;
        Writeln(lFile, '<SQL>');
        Writeln(lFile, FV2SQL);
        Writeln(lFile, '</SQL>');
      finally
        CloseFile(lFile);
      end;
    end;
  end; //if confirm version 2
  ReportFile := AFileName;
  FSettings.WizardFile := ReportFile;
  FReportSaved := True;
  // Report has been saved, set the flag for if user goes back in wizard.
  if Assigned(FWizard) then FWizard.ReportChanged := False;
  Result := True;
end;
{-------------------------------------------------------------------------------
  Gets the index of the given column name from dbgResults
}
function TfrmFilterResult.GetColumnIndex(AName: string): Integer;
var
  i : Integer;
begin
  Result := -1;
  for i := 0 to dbgResults.FieldCount - 1 do
    //If display name matches return its index
    if dbgResults.Fields[i].DisplayName = AName then
      Result := i;
end;
{-------------------------------------------------------------------------------
}
procedure TfrmFilterResult.SetReportFile(const Value: String);
var
  lstShortPathName : String;
  lstShortFileName : String;
begin
  FReportFile := Value;

  if Value <> '' then
  begin
    lstShortPathName := ExtractShortPathname(IncludeTrailingPathDelimiter(Appsettings.ReportPath));
    if (lstShortPathName <> '') and (ExtractShortPathName(IncludeTrailingPathDelimiter(ExtractFileDir(Value))) = lstShortPathname) then
      lstShortFileName := ExtractFileName(Value)
    else
      lstShortFileName := Value
  end else
    lstShortFileName := SFilterResult_Untitled;

  Caption := SFilterResult_Title + ' - ' + lstShortFileName;
end;

{-------------------------------------------------------------------------------
}
procedure TfrmFilterResult.dbgResultsCellClick(Column: TColumn);
var lTableName      : string;
    lKeyData        : TKeyData;
    i               : integer;
    lCurrentMousePos: TPoint;
    lDataset: TDataset;
begin
  inherited;
  lDataset := dbgResults.DataSource.DataSet;
  if not (lDataSet.Eof and lDataSet.Bof) then
  begin
    if IsHyperlink(Column.Field.DisplayText) then
      ExternalHyperlink(Column.Field.DisplayText)
    else if Assigned(FCRReportFile) then
      NavigateCustomReport(Column)
    else
      with dbgResults do
      begin
        // Find the table associated with the field in the clicked column
        { TODO : Need to implement obtaining the table name from the attributes }

        //Get the current position of the mouse
        lCurrentMousePos := dbgResults.ScreenToClient(Mouse.CursorPos);

        // If the mouse is in the same position as it was when it went was pressed,
        // interpret this as a click and swhow the results.
        if FtfMouseOverCell and
           (Abs(lCurrentMousePos.X - FdbgResultsMousePoint.X) < DRAG_THRESHOLD) and
           (Abs(lCurrentMousePos.Y - FdbgResultsMousePoint.Y) < DRAG_THRESHOLD) then
        begin
          if FReportVersion = REPORT_VERSION_3 then
          begin
            //special cases for some individuals
            dbgResults.SelectedRows.CurrentRowSelected := True;
            if SameText(Column.Title.Caption, 'Obs Determiner') then
              with dmGeneralData.qryAllPurpose do
              begin
                lTableName := IfThen(VarToStr(lDataSet.FieldValues['TYPE'])= 'T', 'Taxon', 'Biotope');
                SQL.Text := 'Select Determiner from ' + lTableName +
                            '_Determination where ' + lTablename +
                            '_Occurrence_Key = ' + QuotedStr(VarToStr(lDataSet.FieldValues['OCCURRENCE_KEY']));
                Open;
                try
                  DisplaySelectedName(Fields[0].AsString);
                finally
                  Close;
                end; //try .. finally
              end  //with dmGeneral.qryAllPurpose do
            else
            if SameText(Column.Title.Caption, 'Survey Run By') then
              with dmGeneralData.qryAllPurpose do
              begin
                SQL.Text := 'Select Run_By from ' +
                            ' Survey where ' +
                            ' Survey_Key = ' + QuotedStr(VarToStr(lDataSet.FieldValues['SURVEY_KEY']));
                Open;
                try
                  DisplaySelectedName(Fields[0].AsString);
                finally
                  Close;
                end; //try .. finally
              end //with dmGeneral.qryAllPurpose do
            else
            begin
              lKeyData := TKeydata.Create();
              try
                for i :=0 to FWizard.dmWizard.ReportGenerator.Attributes.Count - 1 do
                  with TAttribute(FWizard.dmWizard.ReportGenerator.Attributes.Objects[i]) do
                  begin
                    if Name = Column.Title.Caption then
                    begin
                      if SameText(Group, ResStr_GroupTaxon) or SameText(Group, ResStr_GroupBiotope) or
                         SameText(Group, ResStr_GroupObservation) then
                      begin
                        lKeyData.ItemKey := VarToStr(lDataSet.FieldValues['OCCURRENCE_KEY']);
                        lKeyData.ItemAdditional :=
                            IfThen(VarToStr(lDataSet.FieldValues['TYPE'])= 'T',
                                   'Taxon_Occurrence', 'Biotope_Occurrence');
                        DisplaySelectedOccurrence(rdOccurrence,lKeyData);
                      end
                      else if SameText(Group, ResStr_GroupSample) then
                      begin
                        lKeyData.ItemKey := VarToStr(lDataSet.FieldValues['SAMPLE_KEY']);
                        lKeyData.ItemAdditional := 'Sample';
                        DisplaySelectedOccurrence(rdSample,lKeyData);
                      end
                      else if SameText(Group, ResStr_GroupEvent) then
                      begin
                        lKeyData.ItemKey := VarToStr(lDataSet.FieldValues['SURVEY_EVENT_KEY']);
                        lKeyData.ItemAdditional := 'Survey_Event';
                        DisplaySelectedOccurrence(rdEvent,lKeyData);
                      end
                      else if SameText(Group, ResStr_GroupSurvey) then
                      begin
                        lKeyData.ItemKey := VarToStr(lDataSet.FieldValues['SURVEY_KEY']);
                        lKeyData.ItemAdditional := 'Survey';
                        DisplaySelectedOccurrence(rdSurvey,lKeyData);
                      end
                      else if SameText(Group, ResStr_GroupLocation) then
                      begin
                        DisplaySelectedLocation(VarToStr(lDataSet.FieldValues['SAMPLE_KEY']));
                      end;
                      Break;
                    end; //if
                  end; //with
              finally
                lKeyData.Free;
              end;
            end;
          end
          else if FReportVersion = REPORT_VERSION_2 then
          begin //Just show the occurrence
            lKeyData := TKeyData.Create;
            lKeyData.ItemKey := Fields[0].AsString;//key
            lKeyData.ItemAdditional := Fields[1].AsString; //Table
            DisplaySelectedOccurrence(rdOccurrence, lKeyData);
          end;
        end; // if FtfMouseOverCell and
      end; //    with dbgResults do
  end;
end;  // actResultViewExecute

{-------------------------------------------------------------------------------
  Clicking on a cell with a link to an external file - shell the file
}
procedure TfrmFilterResult.ExternalHyperlink(const ALink: string);
var
  lLink: string;
begin
  try
    if SameText(LeftStr(ALink, 6), 'img://') then
      lLink := Copy(ALink, 7, Length(ALink))
    else if SameText(LeftStr(ALink, 7), 'file://') then
      lLink := Copy(ALink, 8, Length(ALink))
    else
      lLink := ALink;
    ShellFile(lLink);
  except
    on E:Exception do
      raise EWizard.CreateNonCritical(E.Message);
  end;
end;

{-------------------------------------------------------------------------------
  When clicking a cell on a custom report, navigation is determined by the xml
      file.
}
procedure TfrmFilterResult.NavigateCustomReport(AColumn: TColumn);
var
  lKeyCol: string;
  lTableName: string;
begin
  try
    // are we on a navigable column?
    lKeyCol := FCRReportFile.ReportColumns.ColumnByName(
            AColumn.Field.FieldName).KeyColumn;
    if lKeyCol<>'' then begin
      if FCRReportFile.ReportColumns.ColumnByName(lKeyCol).TableName<>'' then
        lTableName := FCRReportFile.ReportColumns.ColumnByName(lKeyCol).TableName
      else
        lTableName := dbgResults.Datasource.Dataset.FieldByName(
            FCRReportFile.ReportColumns.ColumnByName(AColumn.Field.FieldName).TableField).AsString;
      // Use COM interface to display data
      with CreateOleObject('Recorder2000.AutoApplicationSettings') as IRecorder6 do
        DisplayDataFromSQL(lTableName,
            'SELECT ''' + dbgResults.Datasource.Dataset.FieldByName(lKeyCol).AsString + '''');
    end;
  except
    on EReportColumnsException do ; // column specification not available
  end;
end;

procedure TfrmFilterResult.dbgResultsMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  FdbgResultsMousePoint := Point(X, Y);
end;

//This function loads a version 2 file
procedure TfrmFilterResult.LoadV2WizardFile(AstFileName: String);
var
  lstrlFile : TStringList;
  lLine : integer;
  ltfOnOff : Boolean;
begin
  lstrlFile := TStringList.Create();
  try
    if ExtractFileDir(AstFileName) = '' then
      lstrlFile.LoadFromFile(IncludeTrailingPathDelimiter(Appsettings.ReportPath) + AstFileName)
    else
      lstrlFile.LoadFromFile(AstFileName);
    //Already know this is not an empty file;
    lLine := 0;
    try
      if SameText(lstrlFile[0], '<Template>') then
      begin
        if ExtractFilePath(lstrlFile[1]) = '' then
          TemplateFile := IncludeTrailingPathDelimiter(AppSettings.ReportTemplatePath) +
              lstrlFile[1]
        else
          TemplateFile := lstrlFile[1];
        lLine := 3;
      end;
      FV2SQL := '';
      if SameText(lstrlFile[lLine], '<SQL>') then
      begin
        Inc(lLine);
        while not SameText(lstrlFile[lLine], '</SQL>') do
        begin
          FV2SQL := FV2SQL + #13#10 + lstrlFile[lLine];
          Inc(lLine);
        end;
      end;
      //Don't bother with constraints: they are included in the SQL anyway.
      FReportVersion := REPORT_VERSION_2;
      ReportFile := AstFileName;
    except
      on EStringListError do
        raise TExceptionPath.CreateNonCritical(Format(ResStr_InvalidReportFile, [AstFileName]));
    end;
    with FWizard.dmWizard.qryResults do
    begin
      if Active then close;
      SQL.Text := FV2SQL;
      ParseSQL := true;
      Open;
      SetCountLabel(ST_OCCURRENCE, RecordCount);
      ltfOnOff := RecordCount>0;
    end;
    dbgResults.AllowFiltering := False;
    dbgResults.Datasource := FWizard.dmWizard.dsResults;

    ResizeColumns;

    dmFormActions.actExport.Enabled  := ltfOnOff;
    dmFormActions.actPrint.Enabled   := ltfOnOff;
    dmFormActions.actPreview.Enabled := ltfOnOff;
    actResultCopy.Enabled := ltfOnOff;
    actResultCopyAll.Enabled := ltfOnOff;
    actResultView.Enabled := ltfOnOff;
    mnuReportMap.Enabled  := ltfOnOff;
    actPrintGrid.Enabled  := ltfOnOff;
    mnuReportMap.Enabled  := ltfOnOff;
    pmOutputMap.Enabled   := ltfOnOff;
    actRunSnapshot.Enabled := False;
    actEditSnapshot.Enabled := False;
    actNewSnapshot.Enabled := False;
    actRunReportTemplate.Enabled := ltfOnOff and (TemplateFile <>'');
    actEditReportTemplate.Enabled := ltfOnOff and (TemplateFile <> '');
    actNewReportTemplate.Enabled := ltfOnOff;
    actEditSnapshot.Caption := ResStr_Cap_EditSnapshot;
    actRunSnapshot.Caption := ResStr_Cap_RunSnapshot;
    actEditReportTemplate.Caption := ResStr_Cap_EditReportTemp + ExtractFileName(TemplateFile);
    actRunReportTemplate.Caption := ResStr_Cap_RunReportTemplate + ExtractFileName(TemplateFile);
  finally
    lstrlFile.Free;
  end;
end;

//This function resizes the columns in dbgResults so that they aren't much wider
//than the first 5 rows of text
procedure TfrmFilterResult.ResizeColumns;
var col : integer;
  liNewWidth : integer;
  ltfNullConvert : boolean;
  lirow : integer;
begin
  ltfNullConvert := NullStrictConvert;
  NullStrictConvert := false;
  dbgResults.datasource.Dataset.DisableControls;
  try
  With dbgResults do
    begin
      Datasource.DataSet.First;
      dbgResults.Canvas.Font := dbgResults.TitleFont;
      for col := 0 to Columns.Count -1 do
        if (Columns[col].Visible) and (Columns[col].Title.Caption <> '') then
          Columns[col].Width := dbgResults.Canvas.TextWidth(
              Columns[col].Title.Caption) + 4;

      Canvas.Font := dbgResults.Font;
      lirow := 0;
      while not Datasource.DataSet.Eof do
      begin
        for col := 0 to Columns.Count -1 do
        if Columns[col].Visible then
          begin
            liNewWidth := Canvas.TextWidth(Fields[col].Value) + 4;
            // Increase the column width to fit the data if necessary, but don't go
            // over the width of the data grid (allowing room for a scrollbar)
            Columns[col].Width := Min(Max(liNewWidth, Columns[col].Width), Width - 50);
          end;
        inc(liRow);
        if liRow >30 then break;
        Datasource.DataSet.Next;
      end;
    end;
  finally
    NullStrictConvert := ltfNullCOnvert;
    dbgResults.datasource.Dataset.EnableControls;
    dbgResults.datasource.Dataset.First;
  end;
end;

{-------------------------------------------------------------------------------
  Gets and sets the column indexes as saved by the user during last session
}
procedure TfrmFilterResult.GetColumnIndexes;
var
  i, j, lCount: Integer;
begin
  //Loop to find the position of all the fields in the wizard's result set
  lCount := FWizard.dmWizard.dsResults.DataSet.FieldCount;
  for i := 0 to lCount - 1 do
    //Loop through all the attributes to find the one with position i
    for j := 0 to FWizard.dmWizard.ReportGenerator.Attributes.Count - 1 do
    //Check if this attribute is selected and has position equal to i
    //This ensures that the this field exists in the dataset of dbgResutls
    //Hence, we can use GetAttribtueField without breaking anything
      if (TAttribute (FWizard.dmWizard.ReportGenerator.Attributes.Objects[j]).Selected and
        (TAttribute (FWizard.dmWizard.ReportGenerator.Attributes.Objects[j]).Position = i)) then
        //Set the index of the field represented by this attribute
        //in datasource of dbgResults to the position of this attribute
        GetAttributeField(TAttribute (FWizard.dmWizard.ReportGenerator.Attributes.Objects[j]).Name).Index := i
end;
{-------------------------------------------------------------------------------
  Gets the field from the wizard's result set given its display name
}
function TfrmFilterResult.GetAttributeField(AName: string) : TField;
var
  i, lCount : Integer;
begin
  lCount := FWizard.dmWizard.dsResults.DataSet.FieldCount;
  Result := nil;
  for i := 0 to lCount - 1 do
  begin
    if FWizard.dmWizard.dsResults.DataSet.Fields[i].DisplayName = AName then
      Result := FWizard.dmWizard.dsResults.DataSet.Fields[i];
  end;
end;

{-------------------------------------------------------------------------------
  Description : Implements IReportResults.ReportConnection.  Returns the
              connection unique to this report so that addins can access
              the temp table
  Created : 07/03/2003 }
function TfrmFilterResult.Get_ReportConnection: _Connection;
begin
  Result := FConnection.ConnectionObject as AdoDb_Tlb._Connection;
end;

{-------------------------------------------------------------------------------
  Description : Implements IReportResults.ReportSQL.  Returns SQL used to fill
              the grid
  Created : 07/03/2003 }
function TfrmFilterResult.Get_ReportSQL: WideString;
begin
  Result := FWizard.dmWizard.qryResults.Sql.Text;
end;

{-------------------------------------------------------------------------------
}
procedure TfrmFilterResult.UpdateMapWindowSelector;
begin
  inherited;
  AppSettings.UpdateMapMenu(Self, mnuReportMap, False, SendToMapClick);
  AppSettings.UpdateMapMenu(Self, pmOutputMap, False, SendToMapClick);
end;

{-==============================================================================
    TPostProcessGrid
===============================================================================}
{-------------------------------------------------------------------------------
  Constructor. Pointers to the grid and the report are passed in so that they can be altered
      in the object created using this class. The purpose of this class is merely to tidy the
      code up - all of the implementation here could be done on the form itself.
}
constructor TPostProcessGrid.Create(AGrid: TDBGrid; const AReportFile: TReportFile);
begin
  FGrid := AGrid;
  FReportFile := AReportFile;
end;  // TPostProcessGrid.Create 

{-------------------------------------------------------------------------------
  Do all of the necessary alterations for the grid. 
}
procedure TPostProcessGrid.PostProcessGrid;
begin
  SortColumns;
  SetColumnWidth;
  SetColumnVisible;
  SetColumnCaptions;
end;  // TPostProcessGrid.PostProcessGrid

{-------------------------------------------------------------------------------
  Change the captions of the columns if the XML file specifies.
}
procedure TPostProcessGrid.SetColumnCaptions;
var
  i, j: Integer;
begin
  with FReportFile do
    for i := 0 to ReportColumns.Count - 1 do
      for j := 0 to FGrid.Columns.Count - 1 do
        if LowerCase(FGrid.Columns.Items[j].Title.Caption) =
                    LowerCase(TReportColumn(ReportColumns.Items[i]).Name) then
          FGrid.Columns.Items[j].Title.Caption := TReportColumn(
              ReportColumns.Items[i]).Caption;
end;  // TPostProcessGrid.SetColumnCaptions
{-------------------------------------------------------------------------------
  Set whether the columns should be visible or not.
}
procedure TPostProcessGrid.SetColumnVisible;
var
  i, j: Integer;
begin
  with FReportFile do
    for i := 0 to ReportColumns.Count - 1 do
      for j := 0 to FGrid.Columns.Count - 1 do
        if LowerCase(FGrid.Columns.Items[j].Title.Caption) =
                    LowerCase(TReportColumn(ReportColumns.Items[i]).Name) then
          FGrid.Columns.Items[j].Visible := TReportColumn(ReportColumns.Items[i]).Visible;
end;  // TPostProcessGrid.SetColumnVisible

{-------------------------------------------------------------------------------
  Set the width of the columns.
}

procedure TPostProcessGrid.SetColumnWidth;
var
  i, j: Integer;
begin
  with FReportFile do
    for i := 0 to ReportColumns.Count - 1 do
      for j := 0 to FGrid.Columns.Count - 1 do
        if LowerCase(FGrid.Columns.Items[j].Title.Caption) =
                    LowerCase(TReportColumn(ReportColumns.Items[i]).Name) then
          FGrid.Columns.Items[j].Width := TReportColumn(ReportColumns.Items[i]).Width;
end;  // TPostProcessGrid.SetColumnWidth

{-------------------------------------------------------------------------------
  Order the columns according to the XML file.
}
procedure TPostProcessGrid.SortColumns;
var
  i: Integer;
  lField: TField;
begin
  FReportFile.ReportColumns.Sort(ColumnCompare);
  for i := 0 to FReportFile.ReportColumns.Count - 1 do begin
    lField := FGrid.Datasource.Dataset.FindField(FReportFile.ReportColumns[i].Name);
    if Assigned(lField) then
      lField.Index := i
    else
      MessageDlg(Format(ResStr_ColumnNotFound, [FReportFile.ReportColumns[i].Name]),
                 mtWarning, [mbOk], 0);
  end;
end;  // TPostProcessGrid.SortColumns

{-------------------------------------------------------------------------------
  Cleanup the window if a report params dialog gets called back
}
procedure TfrmFilterResult.CancelReportCallback;
begin
  Close;
end;

{-------------------------------------------------------------------------------
  Drawing in the cells displays any file hyperlinks
}
procedure TfrmFilterResult.dbgResultsDrawColumnCell(Sender: TObject;
  const Rect: TRect; DataCol: Integer; Column: TColumn;
  State: TGridDrawState);
var
  lValue: string;
begin
  inherited;
  lValue := VarToStr(Column.Field.Text);

  if IsHyperlink(lValue) then begin
    with dbgResults.Canvas do begin
      FillRect(Rect);
      if gdSelected in State then
        Font.Color := clHighlightText
      else
        Font.Color := clBlue;
      Font.Style := [fsUnderline];
      TextOut(Rect.Left + 2, Rect.Top + 2, lValue);
    end; // with
  end;
end;

{-------------------------------------------------------------------------------
  Callback method that allows the non-modal params screen to trigger the final
    display of the report output grid
}
procedure TfrmFilterResult.DisplayGridCallback;
var
  lGridProcess: TPostProcessGrid;
begin
  // show the window as it was minimized
  if FWindowState<>wsMinimized then
    WindowState := FWindowState
  else
    WindowState := wsNormal;
  with FCRReportFile do
  try
    dbgResults.Datasource := Datasource;
    lGridProcess := TPostProcessGrid.Create(dbgResults, FCRReportFile);
    try
      lGridProcess.PostProcessGrid;
    finally
      lGridProcess.Free;
    end;
    if FClientFilteredColumns.Count = 0 then
      SetCountLabel(ST_RECORD, qryCustomReport.RecordCount)
    else
      SetCountLabel(ST_RECORD, CountRecords);

    UpdateSnapshotTemplateActions;
    EnableActionsDependingOnAvailableData;
    if Template<>'' then begin
      if ExtractFileDrive(Template)<>'' then
        TemplateFile := Template
      else
        TemplateFile := AppSettings.ReportTemplatePath + Template;
      actRunReportTemplateExecute(nil)
    end;
  finally
    DropTempTables;
  end; // try
end;

{-------------------------------------------------------------------------------
}
procedure TfrmFilterResult.EnableActionsDependingOnAvailableData;
var
  lGotData: Boolean;
begin
  lGotData :=
    (Assigned(FWizard) and (FWizard.dmWizard.qryResults.RecordCount > 0))
    or
    (Assigned(dbgResults.Datasource) and (dbgResults.Datasource.Dataset.RecordCount > 0));

  dmFormActions.actExport.Enabled  := lGotData;
  dmFormActions.actPrint.Enabled   := lGotData;
  dmFormActions.actPreview.Enabled := lGotData;
  actResultCopy.Enabled := lGotData;
  actResultCopyAll.Enabled := lGotData;
  actResultView.Enabled := lGotData;
  actPrintGrid.Enabled  := lGotData;
  // If there's no spatial reference, we can't export to map or google earth, so disable that option
  mnuReportMap.Enabled  := lGotData and gAssistor.HasSpatialFields(dbgResults.Datasource.Dataset);
  pmOutputExportGoogleEarth.Enabled := mnuReportMap.Enabled;
  mnuOutputExportSHP.Enabled := mnuReportMap.Enabled;
  pmOutputMap.Enabled   := mnuReportMap.Enabled;
  pmOutputRevalidateItems.Enabled := lGotData;
end;

{-------------------------------------------------------------------------------
  Test if a data value in a grid cell represents a hyperlink
}
function TfrmFilterResult.IsHyperlink(const AText: string): boolean;
begin
  Result := SameText(LeftStr(AText, 6), 'img://') or
            SameText(LeftStr(AText, 7), 'http://') or
            SameText(LeftStr(AText, 7), 'file://');
end;

{-------------------------------------------------------------------------------
  Run the SM Export Wizard dialog
}
procedure TfrmFilterResult.mnuOutputExportOtherClick(Sender: TObject);
begin
  GenericExport(TWizardExport);
end;

{-------------------------------------------------------------------------------
  Export direct to an XLS file (Excel application not required).
}
procedure TfrmFilterResult.mnuOutputExportExcelClick(Sender: TObject);
begin
  GenericExport(TExcelExport);
end;     

{-------------------------------------------------------------------------------
  Export direct to a KML file for Google Earth
}
procedure TfrmFilterResult.pmOutputExportGoogleEarthClick(Sender: TObject);
var
  lDialog: TdlgGoogleEarthExport;
begin
  lDialog := TdlgGoogleEarthExport.Create(self);
  with lDialog do begin
    Populate(dbgResults);
    if ShowModal = mrOK then begin
      // Hide and show grid so that user doesn't see it scrolling during DoExport
      dbgResults.Hide;
      try
        SaveExportMetaData(DoExport(dbgResults));
      finally
        dbgResults.Show;
      end;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Export direct to a SHP file
}
procedure TfrmFilterResult.mnuOutputExportSHPClick(Sender: TObject);
var
  lDialog: TdlgReportToSHPExport;
begin
  lDialog := TdlgReportToSHPExport.Create(Self);
  with lDialog do begin
    Populate(dbgResults);
    if ShowModal = mrOK then begin
      // Hide and show grid so that user doesn't see it scrolling during DoExport
      dbgResults.Hide;
      try
        SaveExportMetaData(DoExport(dbgResults));
      finally
        dbgResults.Show;
      end;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Generic handler for the export classes based on TBaseExport
}
procedure TfrmFilterResult.GenericExport(AExportClass: TBaseExportClass);
begin
  with AExportClass.Create do
    try
      ReportOutputPath := AppSettings.ReportPath + 'Output\';
      // export class needs a control to embed an invisible TRichEdit onto
      // for rtf->plaintext conversion
      WinControl := self;
      SaveExportMetaData(DoExport(dbgResults));
    finally
      free;
    end; // try
end;

{-------------------------------------------------------------------------------
  Accessor - set to true if the user is offered chance to save report when
    closing, and selects no.  This way we don't ask twice.
}
procedure TfrmFilterResult.SetReportSaveRejected(const Value: Boolean);
begin
  FReportSaveRejected := Value;
end;

{-------------------------------------------------------------------------------
}
procedure TfrmFilterResult.pmOutputRevalidateItemsClick(Sender: TObject);
begin
  inherited;
  dmFormActions.RevalidateNBNData(GetKeyList);
end;

{-------------------------------------------------------------------------------
}
procedure TfrmFilterResult.ApplySecurity;
begin
  pmOutputRevalidateItems.Visible := AppSettings.UserAccessLevel = ualAdmin;
end;

{-------------------------------------------------------------------------------
}
procedure TfrmFilterResult.dbgResultsColumnMoved(Sender: TObject; FromIndex, ToIndex: Integer);
var
  i: integer;
  val: integer;
begin
  inherited;
  FReportSaved := False;
  FColumnOrderChanged := True;
  // Shift the indexes stored for clientfilteredcolumns to reflect the column move
  for i := 0 to FClientFilteredColumns.Count-1 do begin
    val := StrToInt(FClientFilteredColumns.ValueFromIndex[i]);
    if val = FromIndex then
      FClientFilteredColumns.ValueFromIndex[i] := IntToStr(ToIndex)
    else if (FromIndex < ToIndex) and
        (val>FromIndex) and
        (val<=ToIndex) then
      FClientFilteredColumns.ValueFromIndex[i] := IntToStr(val - 1)
    else if (FromIndex > ToIndex) and
        (val<FromIndex) and
        (val>=ToIndex) then
      FClientFilteredColumns.ValueFromIndex[i] := IntToStr(val + 1)
  end;
end;

{-------------------------------------------------------------------------------
  Closes any instances of the parameter entry screens that may still be open.
}
procedure TfrmFilterResult.CloseParameterScreen;
var
  NextHandle: Hwnd;
  NextTitle: array[0..260] of char;
begin
  // Get the first window
  NextHandle := GetWindow(Application.Handle, GW_HWNDFIRST);
  while NextHandle > 0 do
  begin
    // retrieve its text
    GetWindowText(NextHandle, NextTitle, 255);
    if AnsiSameText(ResStr_Parameter_Entry_Screen_Caption, StrPas(NextTitle)) then
      PostMessage(NextHandle, WM_CLOSE, 0, 0);

    // Get the next window
    NextHandle := GetWindow(NextHandle, GW_HWNDNEXT);
  end;
end;

//==============================================================================
// Procedure Name: SaveExportMetaData
//     Parameters: ExportFileName - The full path and file name of the file that
//                                  contains the data being exported.
//        Purpose: Loads the XML template and populates it with details of the
//                 export, and saves a copy of this alongside the data file in
//                 the same directory, using the ".xml" extension.
//------------------------------------------------------------------------------
procedure TfrmFilterResult.SaveExportMetaData(const ExportFileName: String);
var
  TemplatePath: String;
  MetaXMLDocument: IXMLDocument;
  DateTimeNow: TDateTime;
begin
  DateTimeNow := Now;

  TemplatePath := ExtractFilePath(Application.ExeName) +
      'Help\template.metadata.xml';

  // If the template file isn't there, we simply don't save any metadata for
  // this export. Also cancel if the filename is empty (export was cancelled).
  if (ExportFileName <> '') and FileExists(TemplatePath) then
  begin
    MetaXMLDocument := TXMLDocument.Create(TemplatePath);
    MetaXMLDocument.LoadFromFile('');
    MetaXMLDocument.Active := True;

    MetaXMLDocument.Options := [doNodeAutoCreate, doNodeAutoIndent];

    if (MetaXMLDocument.DocumentElement.NodeName = 'ReportMetadata') then
    begin
      MetaXMLDocument.DocumentElement.ChildNodes.Nodes['RunBy'].Attributes[
          'userName'] := AppSettings.UserName;

      MetaXMLDocument.DocumentElement.ChildNodes.Nodes['RunOn'].Attributes[
          'date'] := DateToStr(DateTimeNow);
      MetaXMLDocument.DocumentElement.ChildNodes.Nodes['RunOn'].Attributes[
          'time'] := TimeToStr(DateTimeNow);

      MetaXMLDocument.DocumentElement.ChildNodes.Nodes['Database'].Attributes[
          'serverInstanceName'] := AppSettings.ServerName;
      MetaXMLDocument.DocumentElement.ChildNodes.Nodes['Database'].Attributes[
          'databaseName'] := AppSettings.DatabaseName;

      MetaXMLDocument.DocumentElement.ChildNodes.Nodes['SQL'].Text :=
          FSettings.FilterSQL;

      if FCustomReport then
      begin
        MetaXMLDocument.DocumentElement.ChildNodes.Nodes['XMLReport'].Attributes[
            'name'] := FCRReportFile.Title;
        MetaXMLDocument.DocumentElement.ChildNodes.Nodes['XMLReport'].Attributes[
            'description'] := FCRReportFile.Description;

        MetaXMLDocument.DocumentElement.ChildNodes.Nodes['SQL'].Text :=
            FCRReportFile.ReportSQL.SQL;
      end else
      begin
        MetaXMLDocument.DocumentElement.ChildNodes.Nodes['SQL'].Text :=
            FSettings.FilterSQL;

        // If the template contains this element, we actually remove it...
        // Otherwise SaveAdditionalFilters, below, will add a duplicate.
        MetaXMLDocument.DocumentElement.ChildNodes.Remove(
            MetaXMLDocument.DocumentElement.ChildNodes.Nodes['additional_filters']);

        FSettings.SaveAdditionalFilters(
            MetaXMLDocument.DocumentElement,
            FWizard.dmWizard.ReportGenerator.PolygonSelection);
      end;

      MetaXMLDocument.SaveToFile(ExportFileName + '.metadata.xml');
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Counts the records in the result set.

  This is necessary when client-side filtering is used (to filter on spatial
  references or vague dates) because the RecordCount property of the DataSet
  gives the number of records returned by the server.
}
function TfrmFilterResult.CountRecords: Integer;
var
  lBookmark: TBookmarkStr;
begin
  Result := 0;

  with dbgResults.DataSource.DataSet do
  begin
    lBookmark := Bookmark;

    // This workaround prevents a problem relating to buffers in DB.pas. See
    // TfrmFilterResult.UpdateResults for a full explanation.
    Fields[0].Value;

    First;

    while not Eof do
    begin
      Inc(Result);
      Next;
    end;
    Bookmark := lBookmark;
  end;
end;

{-------------------------------------------------------------------------------
  Determines which filter operators are available for which columns.
}
procedure TfrmFilterResult.dbgResultsFilterOperator(Sender: TObject;
  Column: Integer; const Operator: String; var Accept: Boolean);
var
  lField: TField;
begin
  lField := dbgResults.Columns[Column].Field;
  if IsVagueDateField(lField.FieldName)
      or (lField.DataType in [ftDate, ftDateTime])
  then
    Accept := Operator <> 'LIKE'
  else if IsSpatialReferenceField(lField.FieldName) then
    Accept := (Operator = '=') or (Operator = 'LIKE') or (Operator = 'IS NULL');
end;

{-------------------------------------------------------------------------------
  Responds to changes in the filter specified in the grid control by applying
  it to the result set.
}
procedure TfrmFilterResult.dbgResultsFilterChanged(Sender: TObject);
begin
  inherited;
  MemoriseColumns();
  FWindowState := WindowState;
  ApplyGridFilterToResults;
  // Restore the correct column widths.
  RecallColumns();
end;

{-------------------------------------------------------------------------------
  Constructs an SQL WHERE clause corresponding to the filter specified by the
  user in the grid control.

  Also binds an OnFilterRecord event handler to the dataset if there are any
  filters that cannot be performed in SQL (those on spatial reference and vague
  date columns).
}
function TfrmFilterResult.MakeCriteria: TStrings;
var
  I: Integer;
  lTerm: String;
  lFieldName: String;
  lFieldType: TFieldType;
  lOperator: String;
  lValue: String;

  function FieldTerm: String;
  var
    escaped: string;
  begin
    // make a valid SQL field name because it will be wrapped in ""
    escaped := StringReplace(lFieldName, '"','""', [rfReplaceAll]);
    // We don't always want to call ufn_RtfToPlaintext when checking if a field
    // is "null"; we only call it for columns containing text, and we check
    // whether the field is empty OR whether it is null. So for the "IS NULL"
    // operator we concatenate ufn_RtfToPlaintext elsewhere.
    if ((lFieldType = ftMemo) or (lFieldType = ftString))
        and (lOperator <> 'IS NULL')
    then
      Result := Concat('dbo.ufn_RtfToPlaintext("', escaped, '")')
    else
      Result := Concat('"', escaped, '"');
  end;

  function MakeLikePattern(const Value: String): String;
  begin
    // opening square brackets have a strange way of escaping...
    Result := AnsiReplaceStr(Value, '[', '[[]') + '%';
  end;

  function FormatDateValue(const LocalDate: String): String;
  begin
    try
      Result := FormatDateTime('yyyymmdd', StrToDate(LocalDate));
    except
      on EConvertError do Result := '';
    end;
  end;

  procedure PrepareValue;
  begin
    if lOperator = 'LIKE' then
      lValue := MakeLikePattern(lValue)
    else if lFieldType in [ftDate, ftDateTime] then
      lValue := FormatDateValue(lValue);

    if lValue = '' then
    begin
      Result.Clear;
      Result.Add('WHERE 1 = 0');
    end;
  end;

begin
  ResetClientFilter;

  Result := TStringList.Create;
  lTerm := 'WHERE';
  for I := 0 to dbgResults.Columns.Count - 1 do
    if dbgResults.ColumnFiltered[I] then
    begin
      with dbgResults.Columns[I] do
      begin
        lFieldName := FieldName;
        lFieldType := Field.DataType;
      end;
      lOperator := dbgResults.FilterOperator[I];
      lValue := dbgResults.FilterText[I];

      if IsVagueDateField(lFieldName) or IsSpatialReferenceField(lFieldName)
      then
        ClientFilterColumn(I)
      else
      begin
        if (lOperator = 'IS NULL') and
            ((lFieldType = ftString) or (lFieldType = ftMemo)) then
        begin
          // If the field type is string or memo, it could potentially contain
          // RTF-formatted text. Call the function to get the plain text. If
          // this leaves an empty string, we consider this to match "IS NULL".
          lTerm := Concat(lTerm, ' (', FieldTerm, ' ', lOperator);
          lTerm := Concat(lTerm, ' OR dbo.ufn_RtfToPlaintext(', FieldTerm, ') LIKE '''')');
        end else
        begin
          lTerm := Concat(lTerm, ' ', FieldTerm, ' ', lOperator);

          if lValue <> '' then
          begin
            PrepareValue;
            if lValue = '' then Exit; // user entered an invalid value
            lTerm := Concat(lTerm, ' ', QuotedStr(lValue));
          end;
        end;

        Result.Add(lTerm);
        lTerm := 'AND';
      end;
    end;
end;

{-------------------------------------------------------------------------------
  Clears any client-side filtering on the result set.
}
procedure TfrmFilterResult.ResetClientFilter;
var
  Source: TDataSource;
begin
  Source := dbgResults.DataSource;
  if Assigned(Source) then
  begin
    Source.DataSet.OnFilterRecord := nil;
    FClientFilteredColumns.Clear;
  end;
end;

{-------------------------------------------------------------------------------
  Sets up the result filter callback for a client-side filter on a column in
  the result set.
}
procedure TfrmFilterResult.ClientFilterColumn(Index: Integer);
begin
  FClientFilteredColumns.Add(
      Concat(dbgResults.Columns[Index].FieldName, '=', IntToStr(Index)));
  with dbgResults.DataSource.DataSet do
  begin
    OnFilterRecord := ResultsFilterRecord;
    Filtered := True;
  end;
end;

{-------------------------------------------------------------------------------
  Applies column filters entered by the user to spatial reference and vague
  date columns in the current record of the data set.
}
procedure TfrmFilterResult.ResultsFilterRecord(DataSet: TDataSet;
  var Accept: Boolean);
var
  lFields: TFields;
  I: Integer;
  lFieldName: String;
  lColumnIndex: Integer;
  lOperator: String;
  lRecordText: String;
  lFilterText: String;

  {-----------------------------------------------------------------------------
    Extracts the named vague date from the current record of the dataset.
  }
  function VagueDateFromField: TVagueDate;
  var
    P: String;
  begin
    P := LeftStr(lFieldName, Length(lFieldName) - Length('VAGUE_DATE_START'));
    if not DataSet.FieldByName(lFieldName).IsNull then
      Result.StartDate := DataSet.FieldByName(lFieldName).Value;
    if not DataSet.FieldByName(P + 'VAGUE_DATE_END').IsNull then
      Result.EndDate := DataSet.FieldByName(P + 'VAGUE_DATE_END').Value;
    Result.DateTypeString := DataSet.FieldByName(P + 'VAGUE_DATE_TYPE').Value;
  end;

  {-----------------------------------------------------------------------------
    Indicates whether filtering makes sense for the passed vague date value.
  }
  function CanApplyFilter(const AVagueDate: TVagueDate): Boolean;
  begin
    Result := (AVagueDate.DateTypeString <> 'U') and
        (AVagueDate.DateTypeString <> 'M') and
        (AVagueDate.DateTypeString <> 'S');
  end;

  {-----------------------------------------------------------------------------
    Applies the specified operator to the specified filter vague date and the
    vague date to which the filter should be applied.
  }
  function FilterByVagueDate(const AFieldDate, AFilterDate: TVagueDate;
      const AOperator: String): Boolean;
  begin
    Result := False;

    if CanApplyFilter(AFieldDate) and CanApplyFilter(AFilterDate) then
    begin

      if (AOperator = '=') then
      begin
        Result := CompareVagueDateToVagueDate(AFieldDate, AFilterDate) = 0;
      end
      else if (AOperator = '>') and
          (not AnsiEndsStr('-', AFilterDate.DateTypeString)) then
      begin
        Result := AnsiEndsStr('-', AFieldDate.DateTypeString) or
            (AFieldDate.EndDate > AFilterDate.EndDate);
      end
      else if (AOperator = '<') and
          (not AnsiStartsStr('-', AFilterDate.DateTypeString)) then
      begin
        Result := AnsiStartsStr('-', AFieldDate.DateTypeString) or
            (AFieldDate.StartDate < AFilterDate.StartDate);
      end
      else if AOperator = '>=' then
      begin
        Result := FilterByVagueDate(AFieldDate, AFilterDate, '>') or
            FilterByVagueDate(AFieldDate, AFilterDate, '=');
      end
      else if AOperator = '<=' then
      begin
        Result := FilterByVagueDate(AFieldDate, AFilterDate, '<') or
            FilterByVagueDate(AFieldDate, AFilterDate, '=');
      end;
    end;
  end;

  {-----------------------------------------------------------------------------
    Applies the operator to the vague date from the record and the value
    entered for the column's filter by the user.
  }
  function ApplyVagueDateOperator: Boolean;
  var
    lFieldVagueDate, lFilterVagueDate: TVagueDate;
  begin
    if lOperator = 'IS NULL' then
      Result := lRecordText = ''
    else
    begin
      try
        lFieldVagueDate := VagueDateFromField;
        lFilterVagueDate := StringToVagueDate(lFilterText);
      except
        Result := False;
        Exit;
      end;

      // If the filter date is contained by the field date, then the other
      // operators cannot be applied.
      if IsVagueDateInVagueDate(lFilterVagueDate, lFieldVagueDate) then
      begin
        Result := False
      end else
      begin
        Result := FilterByVagueDate(lFieldVagueDate,
            lFilterVagueDate, lOperator);
      end;
    end;
  end;

  {-----------------------------------------------------------------------------
    Applies the operator to the spatial reference from the record and the value
    entered for the column's filter by the user.
  }
  function ApplySpatialRefOperator: Boolean;
  begin
    if lOperator = 'LIKE' then
      Result := Like(lRecordText, lFilterText + '%')
    else if lOperator = '=' then
      Result := AnsiSameText(lFilterText, lRecordText)
    else if lOperator = 'IS NULL' then
      Result := lFilterText = ''
    else
      Result := False;
  end;

begin
  lFields := DataSet.Fields;
  for I := 0 to FClientFilteredColumns.Count - 1 do
  begin
    lFieldName := FClientFilteredColumns.Names[I];
    lColumnIndex := StrToInt(FClientFilteredColumns.Values[lFieldName]);

    lRecordText := lFields.FieldByName(lFieldName).Text;
    lOperator := dbgResults.FilterOperator[lColumnIndex];
    lFilterText := dbgResults.FilterText[lColumnIndex];
    if IsVagueDateField(lFieldName) then
      Accept := ApplyVagueDateOperator
    else
      Accept := ApplySpatialRefOperator;
    if not Accept then Exit;
  end;
end;

{-------------------------------------------------------------------------------
  Applies the filter specified in the grid control to the result set.
}
procedure TfrmFilterResult.ApplyGridFilterToResults;
var
  lCriteria: TStrings;
  lGridProcess: TPostProcessGrid;
begin

  // Improve performance and eliminate flickering by telling the grid not to
  // update its contents in real time.
  dbgResults.DataSource.DataSet.DisableControls;

  lCriteria := MakeCriteria;
  try
    if dbgResults.DataSource = DataSource then
    begin
      ApplyCriteriaToCustomReport(lCriteria);

      // When filtering is complete, get the grid to update its contents.
      dbgResults.DataSource.DataSet.EnableControls;

      // Restore the correct columns and their headings and widths.
      with FCRReportFile do
      begin
        lGridProcess := TPostProcessGrid.Create(dbgResults, FCRReportFile);
        try
          lGridProcess.PostProcessGrid;
        finally
          lGridProcess.Free;
        end;
      end;
    end
    else if dbgResults.DataSource = FWizard.dmWizard.dsResults then
    begin
      if FReportVersion <> REPORT_VERSION_2 then
      begin
        UpdateResults(lCriteria);
        // When filtering is complete, get the grid to update its contents.
        dbgResults.DataSource.DataSet.EnableControls;
      end;
    end;
  finally
    lCriteria.Free;
  end;
end;

{-------------------------------------------------------------------------------
  Applies the filter specified in the grid control to the result set for a
  custom report.
}
procedure TfrmFilterResult.ApplyCriteriaToCustomReport(ACriteria: TStrings);
var
  lSQL: TStrings;
  lCriteria: TStrings;

  {-----------------------------------------------------------------------------
    Removes the current filter (if any) from the query.
  }
  procedure RemoveWhereClause;
  var
    I: Integer;
  begin
    for I := 0 to lSQL.Count - 1 do
      if AnsiStartsStr('WHERE ', lSQL[I]) then
      begin
        while lSQL.Count > I do lSQL.Delete(I);
        Break;
      end;
  end;

begin
  lSQL := qryCustomReport.SQL;
  lSQL.BeginUpdate;
  try
    RemoveWhereClause;
    lCriteria := MakeCriteria;
    try
      lSQL.AddStrings(lCriteria);
    finally
      lCriteria.Free;
    end;
  finally
    lSQL.EndUpdate;
  end;
  qryCustomReport.Open;
  DisplayGridCallback;
end;

{-------------------------------------------------------------------------------
}
end.
