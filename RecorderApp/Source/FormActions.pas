//==============================================================================
//  Unit:        FormActions
//
//  Implements:  TdmFormActions
//               TComAction
//               TComHelpAction
//               TComRecordCardAction
//
//  Description: Implements and defines some application-wide actions and image
//               lists. Contains the code to start or show the various child
//               forms available in the application.
//               TComxxx: see comments before each class for more info.
//
//  Author:      John van Breda
//  Created:     8 Apr 1999
//
//  Changes:     Eric Salmon, 22 Jan 2002
//               Change to the ExecuteComAction procedure, as actions to take
//               care of non-window controls. Fixed the error.
//
//  Last Revision Details:
//    $Revision: 242 $
//    $Date: 29/10/09 16:38 $
//    $Author: Bonnerearle $
//
//  $History: FormActions.pas $
//
//  *****************  Version 242  *****************
//  User: Bonnerearle  Date: 29/10/09   Time: 16:38
//  Updated in $/JNCC/Development/Build/Source
//  VI 20019
//  Significant rewrite of the boundary import.
//  
//  It now doesn't destroy and created the import dialogues everytime a
//  location is matched.
//  
//  Instead the matching dialog is not modal, and links directly to the
//  location screen.
//  
//  Error handling has been improved so if you get one failure in the area
//  it means you can attempt to rerun the import.
//  
//  Actual error raised in incident was the control to search and link to
//  locations might not have been visible on the matching form but focus
//  was always set to it.
//  
//  TODO: BoundaryImportMatch form still uses a temporary table which
//  probably isn;t a good idea.
//  
//  Also made a minor change to the centring on a location (now jumps to
//  the last added boundary_location record, on the assumption they are
//  more likely to exist than older records) Existing defect that deleting
//  a polygon layer doesn;t clear out the linked boundary_location reords
//  
//  *****************  Version 241  *****************
//  User: Simonlewis   Date: 1/10/09    Time: 9:46
//  Updated in $/JNCC/Development/Build/Source
//  When the data export dialog is invoked from the Filter Result screen,
//  the procedure to save export metadata is now called, passing in the
//  file name of the exported file which is now exposed by the data export
//  dialog as a property.
//  
//  *****************  Version 240  *****************
//  User: Pauldavies   Date: 1/05/09    Time: 15:30
//  Updated in $/JNCC/Development/Build/Source
//  Incident 19103
//  
//  Now filters places for occurrences and occurrences for places reports
//  using User_Survey_Restriction.
//  
//  *****************  Version 239  *****************
//  User: Ericsalmon   Date: 21/04/09   Time: 10:31
//  Updated in $/JNCC/Development/Build/Source
//  Bug fix. Check the file specified for the wizard exists before trying
//  to use it!
//  
//  *****************  Version 238  *****************
//  User: Pauldavies   Date: 1/04/09    Time: 17:25
//  Updated in $/JNCC/Development/Build/Source
//  Incident 18957
//  CCN 321
//  
//  Changed the location matching in BoundaryLocationMatch to be more like
//  the import wizard. Also added arrows in the column headings to make it
//  clearer which columns are being sorted on.
//  
//  *****************  Version 237  *****************
//  User: Andrewkemp   Date: 16/02/09   Time: 11:14
//  Updated in $/JNCC/Development/Build/Source
//  VI 18006 (CCN 262) - done
//  Updated to use `GetProgramDataFolder' to determine default paths for
//  folders beneath "User Files".
//  
//  *****************  Version 236  *****************
//  User: Pauldavies   Date: 23/12/08   Time: 12:26
//  Updated in $/JNCC/Development/Build/Source
//  CCN 268
//  Incident 18367
//  
//  Changed UserConfig to allow administrators to choose which Surveys each
//  user has access to. Various other screens were modified to filter this
//  properly.
//  
//  *****************  Version 235  *****************
//  User: Johnvanbreda Date: 22/05/08   Time: 15:57
//  Updated in $/JNCC/Development/Build/Source
//  Compile fixes/cleanup
//  
//==============================================================================

unit FormActions;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ActnList, ImgList, StdCtrls, ComCtrls, Grids, Clipbrd, BaseFormUnit, Menus, Db,
  VagueDateEdit, ShellAPI, DbCtrls, BaseData, Recorder2000_TLB, JNCCDatasets,
  ExtCtrls, DataClasses, HierarchyNodes, QRPrntr, UserConfig, OnlineHelp, Printers,
  Constants, ExceptionForm, ActiveX, GeneralFunctions, ADODB, Contnrs, Map, ExternalFilter,
  ADODB_TLB, BatchUpdates, CRCommonClasses, Variants;

resourcestring
  ResStr_AddinActionFailed = 'An error occurred running an addin action.  '+
      'The error is described as:'#13#10'%s';
  ResStr_AddinKeyListNil = 'No data is available to export';
  ResStr_ADO = 'ADO';
  ResStr_ADOConnection = 'ADO Connection';
  ResStr_CloseAll = 'All screens must be closed before performing this action.';
  ResStr_ConnectError = 'An error occured trying to open a connection to the database.';
  ResStr_DatabaseBackupComplete = 'The database backup is complete';
  // NOTE - if the IWFileTypeFilter is changed, also update DEFAULT_FILTER_COUNT
  ResStr_IWFileTypeFilter =
        'NBN Data (*.xml)|*.xml|'
      + 'NBN Access Database (*.zip)|*.zip|'
      + 'CSV file (*.csv)|*.csv|'
      + 'Excel file (*.xlsx)|*.xlsx|'      
      + 'Excel 97 - 2003 file (*.xls)|*.xls|'
      + 'Text file (*.txt)|*.txt|'
      + 'DBase file (*.dbf)|*.dbf|'
      + 'Lotus 1-2-3 file (*.wk1)|*.wk1|'
      + 'Paradox file (*.db)|*.db|'
      + 'QuattroPro file (*.wq1)|*.wq1';
  ResStr_NoDataToReport = 'No data is selected for the report';
  ResStr_NoPrinter = 'There are no printers available.  Please make sure there is a default printer ' +
               'connected and try again.' ;
  ResStr_PrintOrPreviewNotAvailable = 'The %s facility is not available for the currently active screen.';
  ResStr_Print = 'Print';
  ResStr_Preview = 'Preview';
  ResStr_ReturnDataLinkInvalid = 'The return data link is invalid';

  ResStr_OSOutOfMemory =  'The operating system is out of memory or resources.';
  ResStr_FileNotFound = 'The specified file was not found.';
  ResStr_PathNotFound = 'The specified path was not found.';
  ResStr_BadFormat = 'The .EXE file is invalid (non-Win32 .EXE or error in .EXE image).';
  ResStr_AccessDenied = 'The operating system denied access to the specified file.';
  ResStr_AssocIncomplete = 'The filename association is incomplete or invalid.';
  ResStr_DDEBusy = 'The DDE transaction could not be completed because other DDE transactions were being processed.';
  ResStr_DDEFail = 'The DDE transaction failed.';
  ResStr_DDETimeOut = 'The DDE transaction could not be completed because the request timed out.';
  ResStr_DDLNotFound = 'The specified dynamic-link library was not found.';
  ResStr_NoAssoc = 'There is no application associated with the given filename extension.';
  ResStr_NotEnoughMem = 'There was not enough memory to complete the operation.';
  ResStr_SharingViolation = 'A sharing violation occurred.';

  ResStr_IncompleteAction = 'There is another incomplete action in progress.'+#10+
                            'Do you wish to Continue?';
  ResStr_BackupDatabase = 'You are about to backup the database. This may take some time.'#13#13 +
                          'Do you want to continue?';
  ResStr_CloseWindows = 'You must close all windows before attempting to restore the database.';
  ResStr_RestoreDatabase = 'Are you sure you want to restore the database?  This will overwrite '+
                           'existing data with the last backup you made.';
  ResStr_NoAccessMapFile = 'One or more essential map files cannot be accessed.'#13 +
                           'Do you want to reset the map dataset?';
  ResStr_BaseMapReset = 'The base map has been reset by another user and needs to be reset on ' +
                        'your system.'#13'Would you like to do it now?';
  ResStr_DatabaseRestored = 'The database has been restored';

  ResStr_RevalidateDatabase =
     'You are about to revalidate the whole NBN database. This may take some time.'#13#13
     + 'Do you want to continue?';
  ResStr_NoValidationError  = 'The database validation was successful.';

  //Status
  ResStr_BackingUpDB = 'Creating backup of the database...';
  ResStr_RestoringTheDB = 'Restoring the database...';
  ResStr_FormsInEditMode = 'You are currently editing data on the following screens.' + #13#10#13#10
      + '%s' + #13#10#13#10 + 'Please save or cancel your changes before running a batch update.';

  ResStr_ConfirmExportWithoutInvalidRecords =
      'There are invalid records in the data you are about to export.'#13#10
      + 'These records will not be exported. Do you wish to continue?';
  ResStr_NoMapsSetup =
      'Cannot display a map as you have not configured any base maps';
  ResStr_ObjectSheetFolderError =
      'The object sheet path on this workstation is not as expected.'#13#10
      + 'Go to Map Options to correct this.';

type
  { Class to store the interface pointer with an action for action lists }
  TComAction = class(TAction)
  private
    FClsID : TGUID;
  public
    { Overload the constructors so that we can use string or GUID }
    constructor Create(AOwner: TComponent; iActionGUID: String); reintroduce; overload;
    constructor Create(AOwner: TComponent; iActionGUID: TGUID); reintroduce; overload;
    property ClsID : TGUID read FClsID;
  end;

  { Class as above, but identifies the action as linking to an IHelp object.
      This sort of action has a menu option 'Help on Addin...' }
  TComHelpAction = class(TComAction);

  { And again - this time for new record card headers }
  TComRecordCardAction = class(TComAction);

  EFormActions = Class(TExceptionPath);

  TdmFormActions = class(TBaseDataModule)
    alForms: TActionList;
    actMapWindow: TAction;
    actTaxonDiction: TAction;
    actObservations: TAction;
    actLocations: TAction;
    actNames: TAction;
    ilMenuOn: TImageList;
    actFilter: TAction;
    actSpeciesForPlace: TAction;
    actBiotopeDiction: TAction;
    actTermsLists: TAction;
    actRuckSack: TAction;
    actSpeciesRecord: TAction;
    actDocuments: TAction;
    actImport: TAction;
    actConfigAddIn: TAction;
    actReportWizard: TAction;
    actCut: TAction;
    actCopy: TAction;
    actPaste: TAction;
    actFind: TAction;
    actSave: TAction;
    actPrint: TAction;
    actOpenReport: TAction;
    actAdminAreaDiction: TAction;
    actTransferData: TAction;
    dlgPrint: TPrintDialog;
    actViewReference: TAction;
    actExport: TAction;
    ilSampleTypes: TImageList;
    actHelp: TAction;
    pmRTF: TPopupMenu;
    pmRTFCut: TMenuItem;
    N1: TMenuItem;
    pmRTFBold: TMenuItem;
    pmRTFItalic: TMenuItem;
    pmRTFUnderline: TMenuItem;
    qrySampleType: TJNCCQuery;
    actMergeData: TAction;
    actNewTaxa: TAction;
    actBold: TAction;
    actItalic: TAction;
    actUnderline: TAction;
    actNewBiotopes: TAction;
    qryTaxonType: TJNCCQuery;
    ilTaxon: TImageList;
    pmRTFCopy: TMenuItem;
    pmRTFPaste: TMenuItem;
    qrySurvey: TJNCCQuery;
    dsSurvey: TDataSource;
    actPreview: TAction;
    actUserConfig: TAction;
    actMapOptions: TAction;
    actManageSchemes: TAction;
    actOccurrencesForPlacesReport: TAction;
    actPlacesForOccurrencesReport: TAction;
    ilButtons: TImageList;
    actManageExportFilters: TAction;
    actRestore: TAction;
    actBackup: TAction;
    actNewCustodian: TAction;
    actEditMetadata: TAction;
    actMapBrowser: TAction;
    dlgIWOpen: TOpenDialog;
    actEnhancedTermLists: TAction;
    actOptions: TAction;
    actLoadExternalFilter: TAction;
    dlgExternalFilterOpen: TOpenDialog;
    actDatabaseValidateAll: TAction;
    actDatabaseValidateSelected: TAction;
    actRunBatchUpdates: TAction;
    actCommitUpdates: TAction;
    actCancelUpdates: TAction;
    qryTaxonGroup: TJNCCQuery;
    dsTaxonGroup: TDataSource;
    procedure alFormsExecute(Action: TBasicAction; var Handled: Boolean);
    procedure actAboutExecute(Sender: TObject);
    procedure actAdminAreaDictionExecute(Sender: TObject);
    procedure actBackupExecute(Sender: TObject);
    procedure actBiotopeDictionExecute(Sender: TObject);
    procedure actBoldExecute(Sender: TObject);
    procedure actConfigAddInExecute(Sender: TObject);
    procedure actCopyExecute(Sender: TObject);
    procedure actCutExecute(Sender: TObject);
    procedure actDocumentsExecute(Sender: TObject);
    procedure actEditMetadataExecute(Sender: TObject);
    procedure actExportExecute(Sender: TObject);
    procedure actFilterExecute(Sender: TObject);
    procedure actFindExecute(Sender: TObject);
    procedure actHelpExecute(Sender: TObject);
    procedure actImportExecute(Sender: TObject);
    procedure actItalicExecute(Sender: TObject);
    procedure actLocationsExecute(Sender: TObject);
    procedure actManageExportFiltersExecute(Sender: TObject);
    procedure actManageSchemesExecute(Sender: TObject);
    procedure actMapBrowserExecute(Sender: TObject);
    procedure actMapOptionsExecute(Sender: TObject);
    procedure actMapWindowExecute(Sender: TObject);
    procedure actMergeDataExecute(Sender: TObject);
    procedure actNamesExecute(Sender: TObject);
    procedure actNewBiotopesExecute(Sender: TObject);
    procedure actNewCustodianExecute(Sender: TObject);
    procedure actNewTaxaExecute(Sender: TObject);
    procedure actObservationsExecute(Sender: TObject);
    procedure actOccurrencesForPlacesReportExecute(Sender: TObject);
    procedure actOpenReportDesignExecute(Sender: TObject);
    procedure actOpenReportExecute(Sender: TObject);
    procedure actPasteExecute(Sender: TObject);
    procedure actPlacesForOccurrencesReportExecute(Sender: TObject);
    procedure actPreviewExecute(Sender: TObject);
    procedure actPrintExecute(Sender: TObject);
    procedure actReportWizardExecute(Sender: TObject);
    procedure actRestoreExecute(Sender: TObject);
    procedure actRuckSackExecute(Sender: TObject);
    procedure actSaveExecute(Sender: TObject);
    procedure actSpeciesForPlaceExecute(Sender: TObject);
    procedure actSpeciesRecordExecute(Sender: TObject);
    procedure actTaxonDictionExecute(Sender: TObject);
    procedure actTermsListsExecute(Sender: TObject);
    procedure actTransferDataExecute(Sender: TObject);
    procedure actUnderlineExecute(Sender: TObject);
    procedure actUserConfigExecute(Sender: TObject);
    procedure actViewReferenceExecute(Sender: TObject);
    procedure dlgIWOpenTypeChange(Sender: TObject);
    procedure actOptionsExecute(Sender: TObject);
    procedure actEnhancedTermListsExecute(Sender: TObject);
    procedure actPasteUpdate(Sender: TObject);
    procedure actCopyUpdate(Sender: TObject);
    procedure actLoadExternalFilterExecute(Sender: TObject);
    procedure actDatabaseValidateAllExecute(Sender: TObject);
    procedure actDatabaseValidateSelectedExecute(Sender: TObject);
    procedure actRunBatchUpdatesExecute(Sender: TObject);
    procedure actCommitUpdatesExecute(Sender: TObject);
    procedure actCancelUpdatesExecute(Sender: TObject);
  private
    FSampleTypeList : TSampleTypeList;
    FTaxonTypeList : TTaxonTypeList;
    FStandardActionCount: integer;  // number of actions before we started adding COM ones
    FNeverImported: Boolean;
    FBatchUpdate: TBatchUpdate;
    function CurrentRichText(const AControl:TControl): TTextAttributes;
    procedure ToggleStyle(iAction: TAction; iStyle: TFontStyle);
    function MapFilesExist(const ABaseMapKey: TKeyString): Boolean;
    function MapNeedReset(const ABaseMapKey: TKeyString): Boolean;
    procedure PopulateAddinImportTypes;
    function DoComImport(ADialog: TOpenDialog): boolean;
    function CheckComputerMap: boolean;
    function GetMasterObjectSheet: string;

  protected
    function CheckForForm( iFormClass : TFormClass ) : integer;
    procedure DisplaySubForm( iFormClass : TFormClass; Sender : TObject );
    function CanDoActions(actionComponent: TComponent): boolean;
    procedure PopulateSampleTypes;
    procedure PopulateTaxonTypes;
    procedure ClearSampleTypeList;
    procedure DoComHelpContent(iAction : TComHelpAction);
    procedure DoComRecordCard(iAction : TComRecordCardAction);
    procedure EmptyList(iList : TTypeList);
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    procedure ApplySecurity;
    function GetFormInstance(const iFormClass : TFormClass): TForm;
    procedure InitBatchUpdate(AFileName: String; AKeyType: TKeyType = ktDefault;
        const AItemKey: String = '');
    function TrimText(const AText:string):string;
    function GetSampleImageIndex(const iSampleTypeKey: TKeyString): integer;
    property SampleTypeList : TSampleTypeList read FSampleTypeList;
    property TaxonTypeList : TTaxonTypeList read FTaxonTypeList;
    function DisplayForm( iFormClass : TFormClass ) : TForm;
    procedure ExecuteComAction( Sender : TObject );
    function HasUnsavedChanges: Boolean;
    function DefaultMapWindow: TfrmMap;
    function MapWindow(const ABaseMapKey: TKeyString; AForceCreate: Boolean = False): TfrmMap;
    procedure MapWindowMenuClick(Sender: TObject);
    procedure UpdateSampleTypeList;
    procedure UpdateRTFMenu(const InRTF:boolean);
    procedure ExportUsingFilterClick(Sender: TObject);
    procedure RunAction(AAction: IInterface);
    procedure SetActionVisibility(const AAction : TAction; AVisible : boolean);
    procedure RevalidateNBNData(const keyList: TKeyList = nil); overload;
    procedure RevalidateNBNData(KeyList: TKeyList; keyListFilter: TEditableKeyList;
        validationMessages: TStringList); overload;
    procedure SetValidateSelectedAction(const state: Boolean);
    property StandardActionCount : integer read FStandardActionCount;
    property BatchUpdate: TBatchUpdate read FBatchUpdate write FBatchUpdate;

  end;

var
  dmFormActions: TdmFormActions;

//==============================================================================
implementation

{$R *.DFM}

uses
  MaintBar, Observations, SurveyDetails, FilterResult, ReportPreview,
  IndOrg, PlaceCard, SpeciesCard, AddIn, ComObj, About, Filter, Rucksack,
  Wizard, References, Options, Locations, TermLists, OpenReport, MergeData,
  DataExport, CompositeComponent, BaseDragFormUnit, BaseExportableUnit,
  ApplicationSettings, OLEContainer, DialogContainer, OLETools, BaseChildUnit,
  GeneralData, Reports, ReportDesigner, MapOptions, MapData, ReportsData,
  TaxonDictBrowser, TaxonDictEditor, BiotopeDictBrowser, BiotopeDictEditor,
  AdminAreaDictBrowser, SchemeManager, FilterManager, ExportFilters, CustodyTransfer,
  DatabaseAccessADO, Metadata, MapBrowser, ImportWizard, ADOConnStringEditor,
  EnhancedTermLists, ComAddinUnit, ComClasses, ValidationResults, CRConstants,
  DefaultPaths, BoundaryLocationMatch, MapClasses;

const

  DEFAULT_FILTER_COUNT=10;

//==============================================================================
{TComAction}
//==============================================================================

{ Constructor - store the GUID for an INewAction com object }
constructor TComAction.Create(AOwner: TComponent; iActionGUID: TGUID);
begin
  inherited Create(AOwner);
  FClsID := iActionGUID;
end;

{ Overloaded constructor to accept strings rather than GUIDs }
constructor TComAction.Create(AOwner: TComponent; iActionGUID: string);
begin
  Create(AOwner, StringToGUID(iActionGUID));
end;

//==============================================================================
constructor TdmFormActions.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSampleTypeList := TSampleTypeList.Create;
  FTaxonTypeList  := TTaxonTypeList.Create;
  PopulateSampleTypes;
  PopulateTaxonTypes;
  ApplySecurity;
  FStandardActionCount := alForms.ActionCount;
  FNeverImported := True;

  qrySurvey.Parameters.ParamByName('UserID').Value := AppSettings.UserID;
end;  // Create

//==============================================================================
destructor TdmFormActions.Destroy;
begin
  ClearSampleTypeList;
  FSampleTypeList.Free;
  FSampleTypeList := nil;
  EmptyList(FTaxonTypeList);
  FTaxonTypeList.Free;
  FTaxonTypeList := nil;
  inherited Destroy;
end;  // Destroy

{===============================================================================
 Description : Applies any security lockouts of actions according to the login
 Created : 15/1/2003}
procedure TdmFormActions.ApplySecurity;
begin
  actBackup.Visible := (AppSettings.UserAccessLevel = ualAdmin);
  if actBackup.Visible then
    actBackup.Visible := dmDatabase.BackupDeviceAvailable;
  actRestore.Visible := actBackup.Visible;
  actEnhancedTermlists.Visible := (AppSettings.UserAccessLevel >= ualAddOnly);
end;

//==============================================================================
procedure TdmFormActions.EmptyList(iList : TTypeList);
var lIndex : Integer;
begin
  for lIndex := 0 to iList.Count -1 do
    TBaseType(iList.Items[lIndex]).Free;
  iList.Clear;
end;  // EmptyList

//==============================================================================
function TdmFormActions.CheckForForm(iFormClass:TFormClass):integer;
var i: Integer;
begin
  Result := -1;
  { If a place card, don't return it as they probably want a new one }
  if iFormClass <> TfrmPlaceCard then
    for i := 0 to frmMain.MDIChildCount - 1 do
      { Check for normal forms }
      if frmMain.MDIChildren[i] is iFormClass then begin
        Result := i;
        Break; // from loop
      end else
      // and check for COM replaced forms
      if frmMain.MDIChildren[i] is TfrmMDIContainer then
        if TfrmMDIContainer(frmMain.MDIChildren[i]).FormName = iFormClass.ClassName then begin
          Result := i;
          Break; // from loop
        end
end;  // CheckForForm

//==============================================================================
{ If a form of the required type already exists, then display it.  If not,
    create one and display it.  Also handles checks for COM addin replacement
    forms }
function TdmFormActions.DisplayForm( iFormClass : TFormClass ) : TForm;
var
  lStrClsID : string;
  lAddinName : string;
  lFoundForm:TForm;
  iFormIndex:integer;
  lClsID : TGUID;
  lCursor:TCursor;
  lReplaceForm: boolean;
begin
  Result:=nil;
  iFormIndex:=CheckForForm(iFormClass);
  if iFormIndex=-1 then
  begin
    { Is it a replacement form? }
    lReplaceForm := False; // default
    if Assigned(AppSettings.ComAddins) then
      if AppSettings.ComAddins.ReplacementForms.IndexOfName(iFormClass.ClassName)<>-1 then
        lReplaceForm := True;
    if lReplaceForm then begin // must replace form with Com form
      lClsID := StringToGUID(AppSettings.ComAddins.ReplacementForms.Values[iFormClass.Classname]);
      TfrmMDIContainer.Create(Application, lClsId);
    end else
    begin // show a normal form
      { Special case - for overloaded constructors }
      //Some code to ensure that the form is opened cleanly.
      frmMain.Refresh;
      lfoundForm:=nil;
      lCursor := HourglassCursor;
      try
        LockWindowUpdate(frmMain.handle);
        try
          if iFormClass=TfrmPlaceCard then
          //** idr start of modifications to loading a placecard with a COM header
          begin
            { Is it a com header form?
              If so the alternative Constructor must be called }
            If dmGeneralData.GetPlaceCardCOMClassID( AppSettings.PlaceCardToLoad,
                                                     lAddinName, lStrClsID ) then
            { Double check that the place card hasn't got a normal header }
            begin
              If CompareText( lAddinName, PLACECARD_STANDARD_HEADER_NAME ) <> 0 then
              { Call the com PlaceCard constructor }
              begin
                lFoundForm := TfrmPlaceCard.Create(frmMain, StringToGuid(lStrClsid));
                lFoundForm.Show;
              end;
            end
            else
              { For a standard Place Card, call the default constructor }
              lFoundForm := TfrmPlaceCard.Create(frmMain);
          end
          else
            lFoundForm:=iFormClass.Create(frmMain);
        finally
          LockWindowUpdate(0);
        end;
        lFoundForm.Show;
        Result := lFoundForm;
      finally
        DefaultCursor(lCursor)
      end;
    end;
  end else begin
    Result := frmMain.MDIChildren[iFormIndex];
    Result.Show;
  end;
  if Assigned(Result) and Assigned(frmMain.XPMenu) then begin
    frmMain.XPMenu.InitComponent(Result);
    frmMain.RefreshXPMenu;
  end;
end;  // DisplayForm

//==============================================================================
{ In this case, a new form is always created }
procedure TdmFormActions.DisplaySubForm( iFormClass : TFormClass; Sender : TObject );
var lForm : TForm;
    lClsID : TGUID;
begin
  { Is it a replacement form? }
    if AppSettings.ComAddins.ReplacementForms.IndexOfName(iFormClass.ClassName)<>-1 then
    begin // must replace form with Com form
      lClsID := StringToGUID(AppSettings.ComAddins.ReplacementForms.Values
            [iFormClass.Classname]);
      TfrmMDIContainer.Create(Application, lClsId);
    end else
    begin // show a normal form
      lForm:=iFormClass.Create(Sender as TForm);
      lForm.Show;
    end;
end;  // DisplaySubForm

//==============================================================================
{ ---------------------Individual actions for each form----------------------- }

procedure TdmFormActions.actMapWindowExecute(Sender: TObject);
begin
  DefaultMapWindow;
end;  // actMapWindowExecute

//==============================================================================
procedure TdmFormActions.actObservationsExecute(Sender: TObject);
begin
  DisplayForm(TfrmObservations);
end;  // actObservationsExecute

//==============================================================================
procedure TdmFormActions.actLocationsExecute(Sender: TObject);
begin
  DisplayForm(TfrmLocations);
end;  // actLocationsExecute

//==============================================================================
procedure TdmFormActions.actNamesExecute(Sender: TObject);
begin
  DisplayForm(TfrmIndOrg);
end;  // actIndividualsExecute

//==============================================================================
procedure TdmFormActions.actAboutExecute(Sender: TObject);
begin
  with TdlgAbout.Create(nil) do
    try
      ShowModal;
    finally
      Release;
    end;
end;  // actAboutExecute

//==============================================================================
procedure TdmFormActions.actSpeciesForPlaceExecute(Sender: TObject);
begin
  if AppSettings.UserAccessLevel>=ualAdmin then
    frmMain.mnuDataEntryNewCardClick(Sender);
end;  // actSpeciesForPlaceExecute

//==============================================================================
procedure TdmFormActions.actSpeciesRecordExecute(Sender: TObject);
begin
  if (Sender is TMenuItem) and (TMenuItem(Sender)<>frmMain.mnuDataEntryPlaces) then begin
    // fiddle the name to convert && to & and remove the & used for hotkeys.
    frmSpeciesCardFileToLoad := StringReplace(TMenuItem(Sender).Caption, '&&', '`', [rfReplaceAll]);
    frmSpeciesCardFileToLoad := StringReplace(frmSpeciesCardFileToLoad, '&', '', [rfReplaceAll]);
    frmSpeciesCardFileToLoad := StringReplace(frmSpeciesCardFileToLoad, '`', '&', [rfReplaceAll]) + '.xml';
  end
  else
    frmSpeciesCardFileToLoad := '';
  DisplayForm(TfrmSpeciesCard);
end;  // actSpeciesRecordExecute

//==============================================================================
// Dictionaries
//------------------------------------------------------------------------------
procedure TdmFormActions.actTaxonDictionExecute(Sender: TObject);
begin
//  DisplayForm(TfrmTaxonDictionary);
  DisplayForm(TfrmTaxonDictBrowser);
end;  // actTaxonBrowserExecute

//------------------------------------------------------------------------------
procedure TdmFormActions.actBiotopeDictionExecute(Sender: TObject);
begin
//  DisplayForm(TfrmBiotopeDictionary);
  DisplayForm(TfrmBiotopeDictBrowser);
end;  // actBiotopeDictionExecute

//------------------------------------------------------------------------------
procedure TdmFormActions.actAdminAreaDictionExecute(Sender: TObject);
begin
//  DisplayForm(TfrmAdminAreaDictionary);
  DisplayForm(TfrmAdminAreaDictBrowser);
end;  // actAdminAreaDictionExecute

//------------------------------------------------------------------------------
procedure TdmFormActions.actNewTaxaExecute(Sender: TObject);
begin
//  DisplayForm(TfrmTaxonDictDetails);
  DisplayForm(TfrmTaxonDictEditor);
end;  // actNewTaxaExecute

//------------------------------------------------------------------------------
procedure TdmFormActions.actNewBiotopesExecute(Sender: TObject);
begin
//  DisplayForm(TfrmBiotopeDictDetails);
  DisplayForm(TfrmBiotopeDictEditor);
end;  // actNewBiotopesExecute

//==============================================================================
procedure TdmFormActions.actRuckSackExecute(Sender: TObject);
begin
  DisplayForm(TfrmRuckSack);
end;  // actRuckSackExecute

//==============================================================================
procedure TdmFormActions.actDocumentsExecute(Sender: TObject);
begin
  DisplayForm(TfrmReferences);
end;  // actDocumentsExecute

//==============================================================================
procedure TdmFormActions.actFilterExecute(Sender: TObject);
begin
  // Filter.
end;  // actFilterExecute

//==============================================================================
procedure TdmFormActions.actImportExecute(Sender: TObject);
var
  lExt: String;
begin
  if GetFormInstance(TfrmImportWizard) <> nil then
  begin
    GetFormInstance(TfrmImportWizard).Show;
    Exit;
  end;

  dlgIWOpen.InitialDir := ExtractFilePath(Application.ExeName);
  if FNeverImported then
  begin
    dlgIWOpen.Filter := ResStr_IWFileTypeFilter;
    PopulateAddinImportTypes;
    // Add ADO Connection to the very end of the list
    dlgIWOpen.Filter := dlgIWOpen.Filter + '|' + ResStr_ADOConnection;
    FNeverImported   := False;
  end;

  dlgIWOpen.FilterIndex := AppSettings.FileImportTypeIndex;
  if dlgIWOpen.Execute then
  begin
    lExt := LowerCase(ExtractFileExt(dlgIWOpen.FileName));
    AppSettings.FileImportTypeIndex := dlgIWOpen.FilterIndex;
    if not DoComImport(dlgIWOpen) then
      with TfrmImportWizard.Create(Self, lExt, dlgIWOpen.FileName) do
      begin
        Show;
        frmMain.RefreshXPMenu;
      end;
  end;
end;  // TdmFormActions.actImportExecute

{-------------------------------------------------------------------------------
  If the filter selected in the open dialog is for an addin, then run the
      addin's import routine.  Otherwise, return false.
}
function TdmFormActions.DoComImport(ADialog: TOpenDialog): boolean;
begin
  Result := (ADialog.FilterIndex > DEFAULT_FILTER_COUNT) and
            (ADialog.FilterIndex < DEFAULT_FILTER_COUNT + AppSettings.ComAddins.ImportFilters.Count + 1);
  if Result then
    with AppSettings.ComAddins.ImportFilters do
      (CreateComObject(StringToGuid(Values[Names[ADialog.FilterIndex - DEFAULT_FILTER_COUNT - 1]]))
          as IImportFilter).ImportFile(ADialog.FileName);
end;

{-------------------------------------------------------------------------------
}
procedure TdmFormActions.PopulateAddinImportTypes;
var
  i: integer;
  lComObj: IUnknown;
  lFilter: string;
begin
  with AppSettings.ComAddins.ImportFilters do
    for i := 0 to Count - 1 do begin
      lComObj := CreateComObject(StringToGuid(Values[Names[i]])) as IUnknown;
      lFilter := '*.' + (lComObj as IFilter).DefaultFileExtension;
      dlgIWOpen.Filter := dlgIWOpen.Filter + '|' + (lComObj as IRecorderAddin).Name
           + ' (' + lFilter + ')|' + lFilter;
    end;
end;  // TdmFormActions.PopulateAddinImportTypes

{-------------------------------------------------------------------------------
}
procedure TdmFormActions.dlgIWOpenTypeChange(Sender: TObject);
var
  lResult: Boolean;
  lConnString: String;
  lcnTest: TADOConnection;
begin
  // Selected ADOConnection, close dialog and get Connection building dialog up.
  if dlgIWOpen.FilterIndex = DEFAULT_FILTER_COUNT + AppSettings.ComAddins.ImportFilters.Count + 1 then
  begin
    EndDialog(GetParent(dlgIWOpen.Handle), 0);
    with TdlgConnStringEditor.Create(nil) do
      try
        lResult := False;
        if ShowModal = mrOk then begin
          lConnString := ConnectionString;
          lResult := True;
        end;
      finally
        Free;
      end;
    if lResult then begin
      lcnTest := TADOConnection.Create(nil);
      try
        lcnTest.ConnectionString := lConnString;
        lcnTest.LoginPrompt := false;
        try
          lcnTest.Open;
          lResult := true;
        except on EOLEException do
          lResult := false;
        end;
      finally
        lcnTest.Free;
      end;
      if lResult then
        with TfrmImportWizard.Create(Self, ResStr_ADO, lConnString) do begin
          Show;
          frmMain.RefreshXPMenu;
        end
      else
        MessageDlg(ResStr_ConnectError, mtWarning, [mbOk], 0);
    end;
  end;
end;  // TdmFormActions.dlgIWOpenTypeChange

//==============================================================================
procedure TdmFormActions.actConfigAddInExecute(Sender: TObject);
begin
  with TdlgAddIn.Create(frmMain) do
    try
      ShowModal;
    finally
      Release;
    end;
end;  // actConfigToolsExecute

//==============================================================================
procedure TdmFormActions.actTermsListsExecute(Sender: TObject);
begin
  DisplayForm(TfrmTermLists);
end;  // actTermListsExecute

//==============================================================================
procedure TdmFormActions.actReportWizardExecute(Sender: TObject);
begin
  DisplayForm(TfrmFilterResult);
end;  // actReportWizardExecute

//==============================================================================
procedure TdmFormActions.actSaveExecute(Sender: TObject);
begin
  // Save
end;  // actSaveExecute

//==============================================================================
procedure TdmFormActions.actPreviewExecute(Sender: TObject);
var tfResult:boolean;
begin
  tfResult:=true;
  // For ReportDesigner, the PrinterSetup dialog has no effect
  // Use the Report/Options menu instead
  if not (frmMain.ActiveMDIChild is TfrmReportDesigner) then
    tfResult:=dlgPrint.Execute;

  if tfResult then
    if (frmMain.ActiveMDIChild is TBaseChild) then
      try
        TBaseChild(frmMain.ActiveMDIChild).PreviewScreen;
      except
        on EAbstractError do
          // TBaseChild.Printscreen not implemented for the active form
          ShowInformation(Format(ResStr_PrintOrPreviewNotAvailable, [ResStr_Preview]));
      end;
end;  // actPreviewExecute

//==============================================================================
procedure TdmFormActions.actPrintExecute(Sender: TObject);
begin
  if Printer.Printers.Count = 0 then begin
    ShowInformation(ResStr_NoPrinter);
    Exit;
  end;
  if frmMain.ActiveMDIChild is TfrmReportPreview then
    TfrmReportPreview(frmMain.ActiveMDIChild).QRPreviewPrinter.Print
  else
  if dlgPrint.Execute then
    if (frmMain.ActiveMDIChild is TBaseChild) then
      try
        TBaseChild(frmMain.ActiveMDIChild).PrintScreen;
      except
        on EAbstractError do
          // TBaseChild.Printscreen not implemented for the active form
          ShowInformation(Format(ResStr_PrintOrPreviewNotAvailable, [ResStr_Print]));
      end;
end;  // actPrintExecute

//==============================================================================
function TdmFormActions.CurrentRichText(const AControl:TControl):TTextAttributes;
begin
  if AControl is TCustomRichEdit then
    Result:=TCustomRichEdit(AControl).SelAttributes
  else
    raise EInvalidOperation.Create(ResStr_NotRTFControl);
end;  // CurrentRichText

//==============================================================================
procedure TdmFormActions.actBoldExecute(Sender: TObject);
begin
  ToggleStyle( actBold, fsBold );
end;  // actBoldExecute

//==============================================================================
procedure TdmFormActions.actItalicExecute(Sender: TObject);
begin
  ToggleStyle( actItalic, fsItalic );
end;  // actItalicExecute

//==============================================================================
procedure TdmFormActions.actUnderlineExecute(Sender: TObject);
begin
  ToggleStyle( actUnderline, fsUnderline );
end;  // actUnderlineExecute


//==============================================================================
{ Generic method to toggle a given style in the selection of the currently
     selected RTF control.  Also toggles the state of the action as appropriate }
procedure TdmFormActions.ToggleStyle( iAction : TAction; iStyle : TFontStyle );
var lStyles:TFontStyles;
begin
  with frmMain.ActiveMDIChild do begin
    lStyles:=CurrentRichText(ActiveControl).Style;
    if iStyle in lStyles then
      Exclude(lStyles,iStyle)
    else
      Include(lStyles,iStyle);
    CurrentRichText(ActiveControl).Style:=lStyles;
  end;
  iAction.Checked := iStyle in lStyles;
end;

//==============================================================================
procedure TdmFormActions.actCutExecute(Sender: TObject);
begin
  with frmMain.ActiveMDIChild do
    if Assigned(ActiveControl) then
      if ActiveControl is TCustomEdit then
        TCustomEdit(ActiveControl).CutToClipboard
      else
      if ActiveControl.Owner is TBaseDragForm then
        TBaseDragForm(ActiveControl.Owner).ExecuteCopy(ActiveControl, True);
end;  // actCutExecute

//==============================================================================
procedure TdmFormActions.actCopyExecute(Sender: TObject);
var
  lForm: TBaseDragForm;
    function FindOwnerForm(AComponent: TComponent): TBaseDragform;
    begin
      if Assigned(AComponent.Owner) then begin
        if AComponent.Owner is TBaseDragform then
          Result := TBaseDragform(AComponent.Owner)
        else
          Result := FindOwnerForm(AComponent.Owner);
      end
      else
        raise EFormActions.Create(ResStr_NotBaseForm + frmMain.ActiveMDIChild.ActiveControl.Owner.Name);
    end;

begin
  with frmMain.ActiveMDIChild do
    if Assigned(ActiveControl) then begin
      lForm := FindOwnerForm(ActiveControl);
      lForm.ExecuteCopy(ActiveControl);
    end;
end;  // actCopyExecute

//==============================================================================
procedure TdmFormActions.actPasteExecute(Sender: TObject);
begin
  with frmMain.ActiveMDIChild do
    { Note - can't use active mdi child because of sub-forms }
    if ActiveControl.Owner is TBaseDragform then
      TBaseDragform(ActiveControl.Owner).ExecutePaste(ActiveControl)
    else
    { Check for composite controls on a base form }
    if ActiveControl.Owner is TCompositeComponent then
    begin
      if ActiveControl.Owner.Owner is TBaseDragform then
        TBaseDragform(ActiveControl.Owner.Owner).ExecutePaste(ActiveControl)
      else
        SendMessage(ActiveControl.Handle, WM_PASTE, 0, 0);
    end else
      SendMessage(ActiveControl.Handle, WM_PASTE, 0, 0);
end;  // actPasteExecute

//==============================================================================
procedure TdmFormActions.actTransferDataExecute(Sender: TObject);
begin
  PostMessage(frmMain.ActiveMDIChild.Handle, WM_DORETURNDATA, 0, 0);
end;  // actTransferDataExecute

//==============================================================================
procedure TdmFormActions.actFindExecute(Sender: TObject);
begin
  // Find
end;  // actFindExecute

//==============================================================================
procedure TdmFormActions.actHelpExecute(Sender: TObject);
begin
  OHelp.ShowHelp(IDH_OVERVIEW);
end;  // actHelpExecute

//==============================================================================
procedure TdmFormActions.actViewReferenceExecute(Sender: TObject);
var szCmdString:array[0..255] of char;
    RetCode    :longint;
    stMessage  :string;
    iIndex     :integer;
begin
  inherited;
  with frmMain.ActiveMDIChild do
    for iIndex:=0 to ComponentCount-1 do
      if (Components[iIndex] is TListBox) and (Components[iIndex].Name='lbExternalRefs') then begin
        with TListBox(Components[iIndex]) do begin
          if ItemIndex>-1 then begin
            StrPCopy(szCmdString, Items[ItemIndex]);
            RetCode:=ShellExecute(Handle,'open',szCmdString,'','',SW_SHOW);
            stMessage:='';
            case RetCode of
              0                      : stMessage:=  ResStr_OSOutOfMemory;
              ERROR_FILE_NOT_FOUND   : stMessage:=  ResStr_FileNotFound;
              ERROR_PATH_NOT_FOUND   : stMessage:=  ResStr_PathNotFound;
              ERROR_BAD_FORMAT       : stMessage:=  ResStr_BadFormat;
              SE_ERR_ACCESSDENIED    : stMessage:=  ResStr_AccessDenied;
              SE_ERR_ASSOCINCOMPLETE : stMessage:=  ResStr_AssocIncomplete;
              SE_ERR_DDEBUSY         : stMessage:=  ResStr_DDEBusy;
              SE_ERR_DDEFAIL         : stMessage:=  ResStr_DDEFail;
              SE_ERR_DDETIMEOUT      : stMessage:=  ResStr_DDETimeOut;
              SE_ERR_DLLNOTFOUND     : stMessage:=  ResStr_DDLNotFound  ;
              SE_ERR_NOASSOC         : stMessage:=  ResStr_NoAssoc;
              SE_ERR_OOM             : stMessage:=  ResStr_NotEnoughMem;
              SE_ERR_SHARE           : stMessage:=  ResStr_SharingViolation;
            end;
            if stMessage<>'' then  MessageDlg(stMessage,mtWarning,[mbOk],0);
          end;
        end;
        Break;
      end;
end;  // actViewReferenceExecute

//==============================================================================
procedure TdmFormActions.alFormsExecute(Action: TBasicAction;
  var Handled: Boolean);
begin
  if not CanDoActions(Action.ActionComponent) then begin
    if Action.Name <> 'actTransferData' then begin
      if MessageDlg(ResStr_IncompleteAction, mtWarning,[ mbYes, mbNo],0) = mrNo then
      begin
        Handled := True;
      end
      else
      begin
        // Cancels the boundary import if the user selects another action.
        // as you can only open one import there should only be one import
        // matching screen open at once
        TdlgBoundaryLocationMatch(GetFormInstance(TdlgBoundaryLocationMatch)).Cancel();
      end;
    end else
      inherited;
  end else
    inherited;
end;  // alFormsExecute

//==============================================================================
{ Returns an instance of the form class requested, if one exists already.
     Otherwise, returns nil }
function TdmFormActions.GetFormInstance(const iFormClass : TFormClass): TForm;
var lIndex:integer;
begin
  lIndex := CheckForForm( iFormClass );
  if lIndex<>- 1 then Result:=frmMain.MDIChildren[lIndex]
                 else Result:=nil;
end;  // GetFormInstance

//==============================================================================
{ Returns true if actions are in their 'normal' mode.  If the user is accessing
      the wizard, getting data etc then the actions should not be accessable,
      so this function returns false }
function TdmFormActions.CanDoActions(actionComponent: TComponent): boolean;
var lForm:TForm;
begin
  Result := True;
  lForm := GetFormInstance(TfrmFilterResult);
  if lForm <> nil then
    Result := not TfrmFilterResult(lForm).AccessingMapFromWizard;
  if Result then begin
    // as you can only open one import there should only be one import
    // matching screen open at once
    // this checks that the instigator of the action wasn't the boundary location import 
    lForm := GetFormInstance(TdlgBoundaryLocationMatch);
    if lForm <> nil then
    begin
      Result := lForm = actionComponent;
    end;
  end;
end;  // CanDoActions

//==============================================================================
procedure TdmFormActions.actExportExecute(Sender: TObject);
var lKeyList, lValidationKeyList : TKeyList;
    lHideCheckBox: Boolean;
    lCursor      : TCursor;
begin
  if (frmMain.ActiveMdiChild is TBaseExportable) or
     (frmMain.ActiveMDIChild is TfrmMDIContainer) then
  begin
    lHideCheckbox := False;
    lCursor := HourglassCursor;
    lKeyList := nil;
    lValidationKeyList := nil;
    try
      try
        try
          if frmMain.ActiveMDIChild is TfrmRucksack then begin
            lValidationKeyList := TfrmRucksack(frmMain.ActiveMdiChild).GetCompleteKeyList;
            lKeyList           := TfrmRucksack(frmMain.ActiveMdiChild).GetCompleteKeyList;
          end
          else begin
            lValidationKeyList := TBaseForm(frmMain.ActiveMdiChild).GetKeyListForValidation;
            lKeyList           := TBaseForm(frmMain.ActiveMdiChild).GetKeyList;
          end;
        except
          on EKeyListCreateError do
            raise EFormActions.CreateNonCritical(ResStr_AddinKeylistNil);
        end; // try
      finally
        DefaultCursor(lCursor);
      end;

      if not Assigned(lKeyList) then
        raise EFormActions.CreateNonCritical(ResStr_AddinKeylistNil);

      if (frmMain.ActiveMdiChild is TfrmObservations) or
        (frmMain.ActiveMdiChild is TfrmFilterResult) then lHideCheckbox := True;

      with TdlgDataExport.Create(nil, lKeyList, lValidationKeyList, lHideCheckbox) do
        try
          ShowModal;
          if (frmMain.ActiveMdiChild is TfrmFilterResult) then
            TfrmFilterResult(
                frmMain.ActiveMdiChild).SaveExportMetaData(FullFilePath);
        finally
          Release;
        end;
    finally
      lKeyList.Free;
      lValidationKeyList.Free;
    end;
  end;
end;  // actExportExecute

//==============================================================================
function TdmFormActions.TrimText(const AText:string):string;
var i:integer;
    stBuffer:string;
begin
  stBuffer:='';
  for i:=1 to Length(AText) do
    if AText[i]<>'&' then stBuffer:=stBuffer+AText[i];

  if Copy(stBuffer,Length(stBuffer)-2,3)='...' then
    stBuffer:=Copy (stBuffer,1,Length(stBuffer)-3);
  TrimText:=stBuffer;
end;  // TrimText;

//==============================================================================
// Clear objects from SampleTypeList
procedure TdmFormActions.ClearSampleTypeList;
var iCount:integer;
begin
  for iCount:= 0 to FSampleTypeList.Count - 1 do
    TSampleType(FSampleTypeList.Items[iCount]).Free;
  FSampleTypeList.Clear;
  ilSampleTypes.Clear;
end;  // ClearSampleTypeList

//==============================================================================
// Fill ilSampleTypes and SampleTypeList with information read from the database
procedure TdmFormActions.PopulateSampleTypes;
var lImage     :TImage;
    lSampleType:TSampleType;
    lResult    :integer;
begin
  with qrySampleType do begin
    lImage := TImage.Create(nil);
    try
      Open;
      First;
      while not Eof do begin
        // Get the picture from the table and add it to the image list
        if not FieldByName('Image').IsNull then begin
          lImage.Picture.Assign(FieldByName('Image'));
          lResult:=ilSampleTypes.Add(lImage.Picture.Bitmap, nil);
          // Store information about the sample type, key and image index
          lSampleType           := TSampleType.Create;
          lSampleType.ShortName := Fieldbyname('Short_Name').asstring;
          lSampleType.ItemKey   := FieldByName('Sample_Type_Key').AsString;
          lSampleType.ImageIndex:= lResult;
          // Add new sample to SampleTypeList
          FSampleTypeList.Add(lSampleType);
        end;
        Next;
      end;
      Close;
    finally
      lImage.Free;
    end; // try..finally
  end;
end;  // PopulateSampleTypes

//==============================================================================
// Fill ilTaxonTypes and TaxonTypeList with information read from the database
procedure TdmFormActions.PopulateTaxonTypes;
var lImage     :TImage;
    lUnknownImage : TImage;
    lTaxonType:TTaxonType;
    lResult    :integer;
begin
  ilTaxon.Clear;
  with qryTaxonType do begin
    lImage := TImage.Create(nil);
    lUnknownImage := TImage.Create(nil);
    try
      Open;
      First;
      while not Eof do begin
        // Get the picture from the table and add it to the image list
        if not FieldByName('IMAGE').IsNull then
        begin
          if FieldByName('SEQUENCE').IsNull then
            lUnknownImage.Picture.Assign(FieldByName('IMAGE'));
          lImage.Picture.Assign(FieldByName('IMAGE'));
          lResult:=ilTaxon.Add(lImage.Picture.Bitmap, nil);
        end
        else
          lResult:=ilTaxon.Add(lUnknownImage.Picture.Bitmap, nil);  // default to unknown image

        // Store information about the sample type, key and image index
        lTaxonType           :=TTaxonType.Create;
        lTaxonType.Sequence  :=Fieldbyname('SEQUENCE').AsInteger;
        lTaxonType.ShortName :=Fieldbyname('SHORT_NAME').asstring;
        lTaxonType.ItemKey   :=FieldByName('TAXON_RANK_KEY').AsString;
        lTaxonType.ItalicFlag:=FieldByName('LIST_FONT_ITALIC').AsBoolean;
        lTaxonType.ImageIndex:=lResult;
        // Add new sample to TaxonTypeList
        FTaxonTypeList.Add(lTaxonType);
        Next;
      end;
      Close;
    finally
      lImage.Free;
      lUnknownImage.Free;
    end; // try..finally
  end;
end;  // PopulateTaxonTypes

//==============================================================================
// Procedure added by PT to test hierarchynode objects - can remove if necessary!
function TdmFormActions.GetSampleImageIndex(const iSampleTypeKey : TKeyString) : integer;
var iCount : integer;
begin
  Result:=-1;
  with SampleTypeList do
    for iCount:=0 to Count - 1 do
      if TSampleType(Items[iCount]).ItemKey = iSampleTypeKey then //found it!
      begin
        Result := TSampleType(Items[iCount]).ImageIndex;
        Break;
      end;  //if
end;  // GetSampleImageIndex

//==============================================================================
procedure TdmFormActions.actMergeDataExecute(Sender: TObject);
begin
  DisplayForm(TfrmMergeData);
end;  // actMergeDataExecute

//==============================================================================
{ Executes actions which represent added COM objects. }
{$HINTS OFF}  // Don't want hint on lDummy not used. We know that!!
procedure TdmFormActions.ExecuteComAction(Sender: TObject);
begin
  if not (Sender is TComAction) then
    raise EFormActions.Create(ResStr_NotCOMAction);
  if Sender is TComHelpAction then
    DoComHelpContent(TComHelpAction(Sender))
  else
  if Sender is TComRecordCardAction then
    DoComRecordCard(TComRecordCardAction(Sender))
  else
    // Get the COM started, don't know what it is yet
    RunAction(CreateCOMObject(TComAction(Sender).ClsID));
end;  // ExecuteComAction
{$HINTS ON}

{-------------------------------------------------------------------------------
  Runs an action declared by an INewAction or IDynamicMenu COM addin }
procedure TdmFormActions.RunAction(AAction: IUnknown);
var
  lOleProxy : TOleProxy;
  lDummy : IDialog;
  ldlgForm : TdlgContainer;
begin
  { Depending on what the new action implements, we create a dialog, mdi child. }
  if Assigned(AAction) then
    try
      (AAction as IExecuteAction).Execute;  // Try as an action first
    except
      on EIntfCastError do // not an Executable action
      begin
        // Get the OLE stuff going, for windowed controls
        lOleProxy := TOleProxy.Create(Application, AAction as IOleObject);
        try // is it a dialog
          lDummy := lOleProxy.ControlInterface as IDialog;  // Try a cast
          ldlgForm := TdlgContainer.Create(nil, lOleProxy);
          try
            ldlgForm.ShowModal;
          finally
            ldlgForm.Free;
          end; // try..finally
        except
          on EIntfCastError do // not a Dialog
            TfrmMDIContainer.Create(Application, lOleProxy);
        end; // try
      end; // on EIntfCastError
      on E:Exception do begin
        ShowInformation(Format(ResStr_AddinActionFailed, [E.Message]));
      end; // on exception
    end; // try..except
end;

//==============================================================================
{ Displays help contents for a COM help system }
procedure TdmFormActions.DoComHelpContent(iAction: TComHelpAction);
var
  lHelpIntf : IHelp;
begin
  lHelpIntf := CreateComObject(iAction.ClsID) as IHelp;
  lHelpIntf.DoHelpContents;
end;  // DoComHelpContent

//==============================================================================
{ Displays a record card with a com header }
procedure TdmFormActions.DoComRecordCard(iAction: TComRecordCardAction);
var
  lForm : TfrmPlaceCard;
begin
  lForm := TfrmPlaceCard.Create(frmMain, iAction.ClsID);
  lForm.Show;
end;  // DoComRecordCard

//==============================================================================
procedure TdmFormActions.UpdateSampleTypeList;
var lForm:TForm;
begin
  ClearSampleTypeList;
  PopulateSampleTypes;
  // Make sure the ImageIndex of the samples stay in synch
  lForm:=frmMain.GetForm(TfrmObservations);
  if lForm<>nil then TfrmObservations(lForm).RefreshSamples;
end;  // UpdateSampleTypeList

//==============================================================================
procedure TdmFormActions.UpdateRTFMenu(const InRTF:boolean);
begin
  actCut.Enabled      :=InRTF;
  actBold.Enabled     :=InRTF;
  actItalic.Enabled   :=InRTF;
  actUnderline.Enabled:=InRTF;
end;  // UpdateRTFMenu


//==============================================================================
procedure TdmFormActions.actUserConfigExecute(Sender: TObject);
begin
  DisplayForm(TfrmUserConfig);
end;  // actUserConfigExecute

//==============================================================================
procedure TdmFormActions.actMapOptionsExecute(Sender: TObject);
begin
  with TdlgMapOptions.Create(nil) do
    try
      ShowModal;
    finally
      Free;
    end; // finally
end;  // actMapOptionsExecute


{ Display the report for a single selected location in the hierarchy }
procedure TdmFormActions.actOccurrencesForPlacesReportExecute(Sender: TObject);
var
  lfrmReports : TfrmReports;
  lKeyList : TKeyList;
begin
  if (frmMain.ActiveMDIChild is TBaseForm) then begin
    lfrmReports := TfrmReports.Create(nil);
    lKeyList := TBaseForm(frmMain.ActiveMDIChild).GetKeyList;
    if lKeyList = nil then
      raise EFormActions.CreateNonCritical(ResStr_NoDataToReport);
    try
      lfrmReports.RunOccurrencesForPlaceReport(lKeyList, AppSettings.UserID)
    finally
      lfrmReports.Free;
      lKeyList.Free;
    end; // try
  end;
end;


{ Initiate the occurrences report according to the selection in the current screen. }
procedure TdmFormActions.actPlacesForOccurrencesReportExecute(
  Sender: TObject);
var
  lfrmReports : TfrmReports;
  lKeyList : TKeyList;
begin
  if (frmMain.ActiveMDIChild is TBaseForm) then begin
    lfrmReports := TfrmReports.Create(nil);
    lKeyList := TBaseForm(frmMain.ActiveMDIChild).GetKeyList;
    if lKeyList = nil then
      raise EFormActions.CreateNonCritical(ResStr_NoDataToReport);
    try
      lfrmReports.RunPlacesForOccurrencesReport(lKeyList, AppSettings.UserID)
    finally
      lfrmReports.Free;
      lKeyList.Free;
    end; // try
  end;
end;


//==============================================================================
{ From inside the report designer, load a report }
procedure TdmFormActions.actOpenReportDesignExecute(Sender: TObject);
var
  lfrmReportDesigner : TfrmReportDesigner;
begin
  lfrmReportDesigner := GetFormInstance(TfrmReportDesigner) as TfrmReportDesigner;
  if Assigned(lfrmReportDesigner) then
    lfrmReportDesigner.OpenReport;
end;


//==============================================================================
{ Display the Recording Schemes manager }
procedure TdmFormActions.actManageSchemesExecute(Sender: TObject);
begin
  inherited;
  with TdlgSchemeManager.Create(nil) do
    try
      if ShowModal = mrOK then
        frmMain.SchemeManager.UpdateRecordingSchemes;
      TExportFilter.UpdateFilterList;
    finally
      Free;
    end; // finally
end;

{ Display the Export Filter Manager dialogue box }
procedure TdmFormActions.actManageExportFiltersExecute(Sender: TObject);
var k: Integer;
begin
  k := ComponentCount - 1;
  while (k >= 0) and not (Components[k] is TfrmFilterManager) do Dec(k);
  if k = -1 then with TfrmFilterManager.Create(Self) do Show
  else TfrmFilterManager(Components[k]).Show;
end;

{ Brings up the Export Data dialogue box ready to export using the relevant filter }
procedure TdmFormActions.ExportUsingFilterClick(Sender: TObject);
var lFilter: TExportFilter;
begin
  lFilter := TExportFilter.CreateFromDatabase(TExportFilterMenuItem(Sender).Key.Key);
  try
    with TdlgDataExport.Create(nil, lFilter) do
      try
        // turn off the implicit identification of occurrences to export, since
        // the filter explicitly lists exactly what to export.
        cbIncludeObservations.Checked := False;
        ShowModal;
      finally
        Free
      end;
  finally lFilter.Free end;
end;

{Transfer Custody of all data over which the current Site ID has Custody.}
procedure TdmFormActions.actNewCustodianExecute(Sender: TObject);
begin
  if frmMain.MDIChildCount <> 0 then
    raise TExceptionPath.CreateNonCritical(ResStr_CloseAll);

  with TdlgCustodyTransfer.Create(nil) do
    try ShowModal;
    finally Free end;
end;

procedure TdmFormActions.actOpenReportExecute(Sender: TObject);
  var lfrmFilterResult : TfrmFilterResult;
begin
  inherited;
  with TdlgOpenReport.Create(frmMain) do
    try
      if ShowModal = mrOK then
      begin
        lfrmFilterResult := TfrmFilterResult(GetFormInstance(TfrmFilterResult));
        if Assigned(lfrmFilterResult) then
          lfrmFilterResult.RunReport(FileName)
        else
          TfrmFilterResult.CreateAndRun(frmMain, FileName)
      end;
    finally
      Free;
    end;
end;

{===============================================================================
 Description : Backup the database by calling the database module
 Created : 15/1/2003 }
procedure TdmFormActions.actBackupExecute(Sender: TObject);
begin
  inherited;
  if MessageDlg(ResStr_BackupDatabase,
                mtConfirmation, [mbYes, mbNo], 0) = mrYes then
  begin
    frmMain.SetStatus( ResStr_BackingUpDB);
    try
      dmDatabase.DoBackup;
    finally
      frmMain.TaskFinished;
    end; // try
    ShowInformation(ResStr_DatabaseBackupComplete);
  end;
end;

{===============================================================================
 Description : Restore the database by calling the database module
 Created : 15/1/2003 }
procedure TdmFormActions.actRestoreExecute(Sender: TObject);
begin
  inherited;
  if frmMain.MDIChildCount > 0 then
    ShowInformation(ResStr_CloseWindows)
  else
    if MessageDlg(ResStr_RestoreDatabase,
                  mtConfirmation, [mbOk, mbCancel], 0) = mrOk then
    begin
      frmMain.SetStatus(ResStr_RestoringTheDB);
      try
        // drop the connection for batch updates so we can restore
        if Assigned(AppSettings.BatchUpdateConnection) then begin
          AppSettings.BatchUpdateConnection.Close;
          AppSettings.BatchUpdateConnection.Free;
          AppSettings.BatchUpdateConnection := nil;
        end;
        dmDatabase.DoRestore;
      finally
        frmMain.TaskFinished;
      end; // try
      // Cleanup main map menu, potentially out of sync.
      AppSettings.UpdateMapWindowSelectors;
      ShowInformation(ResStr_DatabaseRestored);
    end;
end;

{-------------------------------------------------------------------------------
  Description : Display the edit metadata dialog.
  Created : 26/06/2003 }
procedure TdmFormActions.actEditMetadataExecute(Sender: TObject);
begin
  with TdlgMetadata.Create(nil, True) do
    try
      // when called outside of the export process, the button captions are different
      bbContinue.Caption := ResStr_Cap_Ok;
      bbCancel.Caption := ResStr_Cap_Cancel;
      ShowModal;
    finally
      Free;
    end;
end;

{-------------------------------------------------------------------------------
  Description : Controlled accessor method to change the visible state of an
      action.  This enforces security levels so actions don't accidentally
      become visible.
  Created : 14/02/2003 }
procedure TdmFormActions.SetActionVisibility(const AAction: TAction;
  AVisible: boolean);
var
  lVisibilityToApply : boolean;
begin
  // Set lVisibility to apply to false if no security rights to the action
  if  (AAction=actEditMetadata) or (AAction=actImport) or
      (AAction=actExport) or (AAction=actManageSchemes) or
      (AAction=actManageExportFilters) then
    lVisibilityToApply := AVisible and (AppSettings.UserAccessLevel >= ualFullUser)
  else
    lVisibilityToApply := AVisible;
  AAction.Visible := lVisibilityToApply;
end;

{-------------------------------------------------------------------------------
}
procedure TdmFormActions.MapWindowMenuClick(Sender: TObject);
var
  lMapWindow: TfrmMap;
  lBaseMapKey: TKeyString;
begin
  lBaseMapKey := AppSettings.AvailableMaps[TMenuItem(Sender).Tag].BaseMapKey;
  // Check the Map window isn't up yet
  lMapWindow := MapWindow(lBaseMapKey, True);
  // Allow several Map windows, but each should have different dataset.
  if Assigned(lMapWindow) then begin
    lMapWindow.BringToFront;
    lMapWindow.SetFocus;
  end;
end;  // TfrmMain.MapBrowserMenuClick

{-------------------------------------------------------------------------------
}
function TdmFormActions.MapWindow(const ABaseMapKey: TKeyString; AForceCreate: Boolean = False): TfrmMap;
var
  i: Integer;
  lBaseMapKey: TKeyString;
  lMapFileMissing, lMapNeedReset: Boolean;
  defaultMap: TAvailableMap;

begin
  Result := nil;
  { If the object sheet path for this workstation is not the same as that of the master //
   workstation then no further map access is possible }
  if CheckComputerMap then begin
    for i := 0 to frmMain.MDIChildCount - 1 do
      if frmMain.MDIChildren[i] is TfrmMap then
        // If no BaseMapKey, return first Map window found.
        if (ABaseMapKey = '') or (TfrmMap(frmMain.MDIChildren[i]).BaseMapKey = ABaseMapKey) then
        begin
          Result := TfrmMap(frmMain.MDIChildren[i]);
          Exit;
        end;
    // Get here if no map window displayed, or requested one not up.
    // Check if really want a map window displayed.
    if AForceCreate then begin
      defaultMap := AppSettings.AvailableMaps.DefaultMap;
      if not assigned(defaultMap) then
        raise EFormActions.CreateNonCritical(ResStr_NoMapsSetup);
      // If no specific base map requested, select default.
      if ABaseMapKey = '' then lBaseMapKey := defaultMap.BaseMapKey
                          else lBaseMapKey := ABaseMapKey;
      // If a file is missing from the object sheet file set...
      lMapFileMissing := not MapFilesExist(lBaseMapKey);
      lMapNeedReset   := MapNeedReset(lBaseMapKey);
      if lMapFileMissing or lMapNeedReset then
        if (lMapFileMissing and
           (MessageDlg(ResStr_NoAccessMapFile,
              mtConfirmation, [mbYes, mbNo], 0) <> mrYes)) or
           (lMapNeedReset and
             (MessageDlg(ResStr_BaseMapReset,
                mtConfirmation, [mbYes, mbNo], 0) <> mrYes)) then
        Exit
      else
        actMapOptionsExecute(nil);
      // If we get here, all should be ok.
      Result := TfrmMap.Create(frmMain, lBaseMapKey);
      Result.Caption := Format(ResStr_Cap_MapWindow,[AppSettings.AvailableMaps.ItemsByKey[lBaseMapKey].DisplayName]);
    end;
  end else
    MessageDlg(ResStr_ObjectSheetFolderError,mtInformation, [mbOK], 0);

end;  // TdmFormActions.MapWindow

{-------------------------------------------------------------------------------
}
function TdmFormActions.DefaultMapWindow: TfrmMap;
begin
  if not Assigned(AppSettings.AvailableMaps.DefaultMap) then begin
    // No default map specified, for some reason. So return first one found in list instead.
    if AppSettings.AvailableMaps.Count > 0 then begin
      Result := MapWindow(AppSettings.AvailableMaps[0].BaseMapKey, True);
      if Assigned(Result) then begin
        Result.BringToFront;
        Result.SetFocus;
      end;
    end else
      Result := nil;  // If we get here, something very wrong has happened to the logic.
  end else begin
    Result := MapWindow(AppSettings.AvailableMaps.DefaultMap.BaseMapKey, True);
    if Assigned(Result) then begin
      Result.BringToFront;
      Result.SetFocus;
    end;
  end;
end;  // TdmFormActions.DefaultMapWindow

{-------------------------------------------------------------------------------
}
function TdmFormActions.MapFilesExist(const ABaseMapKey: TKeyString): Boolean;
var
  lDSFileName: String;
begin
  Result := True; // default
  // Select the File_Name, as it can differ from the key value, especially after
  // a data transfer.
  with dmDatabase.ExecuteSQL('SELECT Dataset_Sheet_FileName FROM Map_Sheet WHERE Sheet_Type=3 ' +
                             'AND New_Data=0 AND Base_Map_Key=''' + ABaseMapKey + '''', True) do
  begin
    while not Eof do begin
      // Get Filename with path to where it should be found.
      lDSFileName := AppSettings.ObjectSheetFilePath +
                     ExtractWithoutExt(Fields['Dataset_Sheet_FileName'].Value);
      // If one file does not exist, stop now, as map potentially won't work.
      if not (FileExists(lDSFileName + '.gsf') and FileExists(lDSFileName + '.gix') and
              FileExists(lDSFileName + '.mdx') and FileExists(lDSFileName + '.gdb')) then
      begin
        Result := False;
        Exit;
      end;
      MoveNext;
    end;
    Close;
  end;
end;

{-------------------------------------------------------------------------------
}
function TdmFormActions.MapNeedReset(const ABaseMapKey: TKeyString): Boolean;
begin
  with dmDatabase.ExecuteSQL(
      'SELECT BM.Reset_Index AS BaseIndex, CM.Reset_Index AS ComputerIndex FROM Base_Map BM ' +
      'JOIN Computer_Map CM ON CM.Base_Map_Key = BM.Base_Map_Key ' +
      'AND CM.Computer_ID = Host_Name() WHERE BM.Base_Map_Key = ''' + ABaseMapKey + '''', True) do
    try
      Result := Fields['BaseIndex'].Value <> Fields['ComputerIndex'].Value;
    finally
      Close;
    end;
end;

{-------------------------------------------------------------------------------
}
procedure TdmFormActions.actMapBrowserExecute(Sender: TObject);
begin
  inherited;
  // Find or create MapBrowser window
  MapBrowserWindow(True).SetFocus;
end;

procedure TdmFormActions.actOptionsExecute(Sender: TObject);
var
  oldRefSystem: String;
  oldSurveyTag: Boolean;
begin
  with TdlgOptions.Create(nil) do
    try
      if Sender = frmMain.pmMainToolbarConfigure then
        pcOptionsPages.Activepage := tsMainToolbar;
      oldRefSystem := AppSettings.SpatialRefSystem;
      oldSurveyTag := AppSettings.OrganiseSurveysByTag;

      if ShowModal = mrOk then begin
        frmMain.ReapplyOptions;
        if AppSettings.SpatialRefSystem <> oldRefSystem then
          //Send message to all open forms that the spatial reference system has changed
          frmMain.BroadcastMessage(WM_REFRESH_SPATIAL_REF_SYSTEM);
        if AppSettings.OrganiseSurveysByTag <> oldSurveyTag then
          frmMain.BroadcastMessage(WM_REFRESH_OBSERVATIONS);
      end;
    finally
      Free;
    end;
end;

{-------------------------------------------------------------------------------
}
procedure TdmFormActions.actEnhancedTermListsExecute(Sender: TObject);
begin
  DisplayForm(TfrmEnhancedTermLists);
end;

{-------------------------------------------------------------------------------
  To enable addins tabs to receive Ctrl-C & Ctrl-V, disable the shortcuts when
    the action is disabled otherwise no message gets through
}
procedure TdmFormActions.actPasteUpdate(Sender: TObject);
begin
  if actPaste.Enabled then
    actPaste.ShortCut := ShortCut(Word('V'), [ssCtrl])
  else
    actPaste.ShortCut := 0;
end;

{-------------------------------------------------------------------------------
  To enable addins tabs to receive Ctrl-C & Ctrl-V, disable the shortcuts when
    the action is disabled otherwise no message gets through
}
procedure TdmFormActions.actCopyUpdate(Sender: TObject);
begin
  if actCopy.Enabled then
    actCopy.ShortCut := ShortCut(Word('C'), [ssCtrl])
  else
    actCopy.ShortCut := 0;              
end;

{-------------------------------------------------------------------------------
  Displays an open file dialog to load an external filter.
}
procedure TdmFormActions.actLoadExternalFilterExecute(Sender: TObject);
var
  lFilter: TFileFilter;
  lForm: TForm;
begin
  inherited;
  lForm := frmMain.ActiveMDIChild;
  dlgExternalFilterOpen.InitialDir := GetProgramDataFolder(PATH_USER_FILES);
  if dlgExternalFilterOpen.Execute then begin
    lFilter := TFileFilter.Create;
    try
      if lFilter.loadFilter(dlgExternalFilterOpen.FileName) then begin
        lFilter.ApplyFilter;
        if Assigned(lForm) then lForm.BringToFront;
      end;
    finally
      lFilter.Free;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Runs the NBN database through validators.
}
procedure TdmFormActions.actDatabaseValidateAllExecute(Sender: TObject);
begin
  inherited;
  if ConfirmYesNo(ResStr_RevalidateDatabase) = mrYes then
    RevalidateNBNData;
end;

{-------------------------------------------------------------------------------
  Runs the records on current form through validators.
}
procedure TdmFormActions.actDatabaseValidateSelectedExecute(Sender: TObject);
var
  currentForm: TForm;
  keyList: TKeyList;
begin
  inherited;
  // allow the screen to redraw immediately
  Application.ProcessMessages;
  currentForm := frmMain.ActiveMDIChild;
  if Assigned(currentForm) and
     ((currentForm is TBaseExportable) or (currentForm is TfrmReferences)) then
  begin
    keyList := TBaseForm(currentForm).GetKeyListForValidation;
    if Assigned(keyList) then begin
      RevalidateNBNData(keyList);
      keyList.Free;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Runs the NBN database, or records provided in key list through validators.
}
procedure TdmFormActions.RevalidateNBNData(const keyList: TKeyList);
var
  keyListFilter: TEditableKeyList;
  validationMessages: TStringList;
  currentForm: TForm;
  lCursor: TCursor;
begin
  lCursor := HourglassCursor;
  currentForm := frmMain.ActiveMDIChild;
  keyListFilter := TEditableKeyList.Create();
  keyListFilter.SetTable(MIXED_DATA);
  validationMessages := TStringList.Create;
  try
    RevalidateNBNData(keyList, keyListFilter, validationMessages);

    if keyListFilter.Header.ItemCount > 0 then begin
      // Apply filter
      with TListFilter.Create do
        try
          if LoadFilter(keyListFilter, validationMessages) then begin
            ApplyFilter;
            if Assigned(currentForm) then currentForm.BringToFront;
          end;
        finally
          Free;
        end;
      DefaultCursor(lCursor);
      with TdlgValidationResults.Create(nil) do
        try
          PartialValidation := Assigned(keyList);
          SetKeyList(keyListFilter, validationMessages);
          ShowModal;
        finally
          Free;
        end;
    end else
      MessageDlg(ResStr_NoValidationError, mtInformation, [mbOk], 0);
  finally
    validationMessages.Free;
    keyListFilter.Free;
    DefaultCursor(lCursor);
  end;
end;

{-------------------------------------------------------------------------------
  Runs the NBN database, or records provided in key list through validators.
}
procedure TdmFormActions.RevalidateNBNData(KeyList: TKeyList; keyListFilter: TEditableKeyList;
  validationMessages: TStringList);
var
  idxAddin, idxFailed, countInvalid: Integer;
  guid: TGUID;
  intfValidation: IUnknown;
  keyListFailed: TEditableKeyList;
  comKeyList: TComKeyList;
  tableName: String;
  cursor: TCursor;
  isMixed: Boolean;
begin
  cursor := AppStartCursor;
  try
    comKeyList := TComKeyList.Create(keyList);

    // Loop through each addin
    for idxAddin := 0 to AppSettings.ComAddins.Validators.Count - 1 do
    begin
      guid := StringToGuid(AppSettings.ComAddins.Validators[idxAddin]);
      intfValidation := CreateComObject(guid);
      // Only available on IValidation6
      if not Supports(intfValidation, IID_IValidation6) then Continue;

      // Run validation, either selected record(s), or whole database
      if Assigned(keyList) then
        countInvalid := (intfValidation as IValidation6).ValidateKeyList(
            dmDatabase.LocalDatabase.ConnectionObject as ADODB_TLB._Connection,
            comKeyList)
      else
        countInvalid := (intfValidation as IValidation6).ValidateAll(
            dmDatabase.LocalDatabase.ConnectionObject as ADODB_TLB._Connection);

      if countInvalid > 0 then
      begin
        keyListFailed := TEditableKeyList.Create((intfValidation as IValidation6).KeyList);
        try
          // Gather all in one list, in case of multiple validators.
          tableName := keyListFailed.Header.TableName;
          isMixed   := tableName = MIXED_DATA;
          for idxFailed := 0 to keyListFailed.Header.ItemCount - 1 do begin
            if isMixed then tableName := keyListFailed.Items[idxFailed].KeyField2;
            keyListFilter.AddItem(keyListFailed.Items[idxFailed].KeyField1, tableName);
            validationMessages.Add((intfValidation as IValidation6).ErrorString[idxFailed]);
          end;
        finally
          keyListFailed.Free;
        end;
      end;
    end;
  finally
    DefaultCursor(cursor);
  end;
end;

{-------------------------------------------------------------------------------
  Show/hide the "Revalidate Record" action for forms allowing specific records
  to be revalidated (Observations, Locations, References, Names).
}
procedure TdmFormActions.SetValidateSelectedAction(const state: Boolean);
var
  i: Integer;
  guid: TGUID;
  intf: IUnknown;
begin
  for i := 0 to AppSettings.ComAddins.Validators.Count - 1 do begin
    guid := StringToGuid(AppSettings.ComAddins.Validators[i]);
    intf := CreateComObject(guid);

    // Only need to find one with IValidation6
    if Supports(intf, IID_IValidation6) then begin
      SetActionVisibility(
          actDatabaseValidateSelected,
          state and (AppSettings.UserAccessLevel = ualAdmin));
      Break;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Show the batch updates dialog.
}
procedure TdmFormActions.actRunBatchUpdatesExecute(Sender: TObject);
begin
  inherited;
  if not HasUnsavedChanges then begin
    with TdlgBatchUpdate.Create(nil) do begin
      try
        if ShowModal = mrOK then
          InitBatchUpdate(FileName);
      finally
        Free;
      end;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Create a batch update object and run the batch update.
}
procedure TdmFormActions.InitBatchUpdate(AFileName: String; AKeyType: TKeyType = ktDefault;
    const AItemKey: String = '');
begin
  FBatchUpdate := TBatchUpdate.Create;
  try
    FBatchUpdate.RunBatchUpdate(AFileName, AKeyType, AItemKey);
  finally
    // If we don't need to keep the batch update object for later, free it now.
    if FBatchUpdate.Finished then
      FreeAndNil(FBatchUpdate);
  end;
end;

{-------------------------------------------------------------------------------
  Commit batch updates after reviewing an external filter.
}
procedure TdmFormActions.actCommitUpdatesExecute(Sender: TObject);
begin
  inherited;
  if Assigned(FBatchUpdate) then
    FBatchUpdate.CommitUpdate;
  FreeAndNil(FBatchUpdate);
end;

{-------------------------------------------------------------------------------
  Cancel batch updates after reviewing an external filter
}
procedure TdmFormActions.actCancelUpdatesExecute(Sender: TObject);
begin
  inherited;
  if Assigned(FBatchUpdate) then
    FBatchUpdate.CancelUpdate;
  FreeAndNil(FBatchUpdate);
end;

{-------------------------------------------------------------------------------
  If a form has unsaved changes, make the user save or cancel before letting them
  run a batch update.
}
function TdmFormActions.HasUnsavedChanges: Boolean;
var
  i: Integer;
  lScreens: String;
begin
  Result := False;
  for i := 0 to frmMain.MDIChildCount - 1 do
    if frmMain.MDIChildren[i] is TBaseChild then
      if (TBaseChild(frmMain.MDIChildren[i]).EditMode <> emView) then begin
        Result := True;
        if lScreens = '' then
          lScreens := frmMain.MDIChildren[i].Caption
        else
          lScreens := lScreens + #13#10 + frmMain.MDIChildren[i].Caption;
      end;
  if Result then
    ShowInformation(Format(ResStr_FormsInEditMode, [lScreens]));
end;

function TdmFormActions.CheckComputerMap: boolean;
begin
  dmDatabase.ExecuteSQL(Format('UPDATE COMPUTER_MAP SET OBJECT_SHEET_FOLDER = ''%s'' WHERE ' +
                       ' COMPUTER_ID = host_name()', [AppSettings.ObjectSheetFilePath]));
  Result := CompareText(GetMasterObjectSheet,AppSettings.ObjectSheetFilePath) = 0;
end;

function TdmFormActions.GetMasterObjectSheet: string;
begin
  Result:= AppSettings.ObjectSheetFilePath;
  with dmDatabase.ExecuteSQL('Select Object_Sheet_Folder FROM COMPUTER_MAP WHERE  ' +
                                'MASTER = 1', true) do
  try
    if not eof then Result := Fields['Object_Sheet_Folder'].Value;
  finally
     Close;
  end;
end;

end.
