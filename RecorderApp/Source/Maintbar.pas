//==============================================================================
//  Unit:        Maintbar
//
//  Implements:  TfrmMain
//
//  Description: Main form of the application. MDI form containing the main
//               menu and toolbar
//
//  Author:      John van Breda
//  Created:     8 April 1999
//
//  Last Revision Details:
//    $Revision: 226 $
//    $Date: 8/06/10 9:59 $
//    $Author: Andrewkemp $
//
//==============================================================================

unit Maintbar;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Buttons, ExtCtrls, Menus, Map, ComCtrls, ToolWin, ActnList, StdCtrls,
  Registry, BaseFormUnit, UserConfig, QRPrntr, QuickRpt, OnlineHelp, FileCtrl,
  OleTools, TaskProgress, DBListCombo, DataClasses, ExceptionForm, Constants,
  RecordingSchemes, ApplicationSettings, XPMenu, XPToolButton, ImgList, Contnrs,
  DatabaseAccessADO, Recorder2000_Tlb, ADODB, CRCommonClasses, RapTree,
  CRReportIndex;

resourcestring
  ResStr_PreparingReport  = 'Preparing Report';
  ResStr_ReportWindowOpen = 'User Reports Not available - report already open';
  ResStr_About            = 'About %s...';
  ResStr_Dictionaries     = 'Di&ctionaries'; // XP gives it i as shortcut

  ResStr_GroupRebuildingTime = 'Rebuilding the Taxon Group Index is likely to take a long time.'#13#10#13#10 +
                               'Are you sure you want to proceed?';

  ResStr_NameRebuildingTime = 'Rebuilding the Taxon Name Index is likely to take a long time.'#13#10#13#10 +
                               'Are you sure you want to proceed?';

  ResStr_ClearOldIndex = 'Clearing old group index...';
  ResStr_IndexRebuild = 'Taxon group index rebuild is complete.';
  ResStr_NameIndexRebuildComplete = 'Taxon name index rebuild is complete.';

  ResStr_SynonymRebuildingTime =  'Rebuilding the Taxon Synonym Index is likely to take a long time.' + #10#10 +
                                  'Are you sure you want to proceed?';

  ResStr_DesignationRebuildingTime =  'Rebuilding the Taxon Designation Index is likely to take a long time.' + #10#10 +
                                  'Are you sure you want to proceed?';

  ResStr_DesignationRebuildingStatus = 'Rebuilding Taxon Designations Index...';
  ResStr_DictionaryUpdateStatus = 'Upgrading Dictionary...';
  ResStr_DictionaryUpdateCounting = 'Counting Records to be updated...';
  ResStr_DesignationIndexRebuildComplete = 'Taxon designation index rebuild is complete.';

  ResStr_SynonymIndexRebuildComplete = 'Taxon synonym index rebuild is complete.';
  ResStr_NoBackupDevice = 'Can''t find backup device NBNData_backup';

  ResStr_CannotMoveOldBackup =  'You have selected to change the backup position but this will not move any old backup files. ' +
                                'This means that you will not be able to restore from previous backups. ' +
                                'Select OK to proceed or Cancel to abort the operation.';
  ResStr_HaveUncommittedUpdates = 'There are pending updates as part of a batch update. ' +
                                  'Do you want to commit these updates before closing the application?';
  ResStr_SpatialReferenceSystemLabel = 'Spatial Reference System:';
  ResStr_HelpUrlNotFound = 'The setting for the Online Help website is not available. Please check that your ' +
      'Recorder 6 installation has been correctly upgraded.';
  ResStr_InsufficientPermissons = 'You do not have sufficient permissiosn to run this option';
  ResStr_DictionaryRebuilding = 'Dictionary update. The last update processed was %s.sql. '#13#10#13#10  +
                                'Select OK to proceed or Cancel to exit.';

 Type
  EMainFormError = class(TExceptionPath);

  TPreviewOwner = (poApplication, poActiveForm);

  // To have a custom preview be used as the default preview,
  // you first define an interface class.  You will provide two
  // functions for this class, Show, and ShowModal.
  TQRReportPreviewInterface = class(TQRPreviewInterface)
  protected
    function GetWindow(AQRPrinter : TQRPrinter) : TWinControl;
  public
    function Show(AQRPrinter : TQRPrinter) : TWinControl; override;
    function ShowModal(AQRPrinter : TQRPrinter): TWinControl; override;
  end;

  TfrmMain = class(TForm)
    mnuMain: TMainMenu;
    mnuFile: TMenuItem;
    mnuData: TMenuItem;
    mnuDictionary: TMenuItem;
    mnuMainMap: TMenuItem;
    mnuHelp: TMenuItem;
    mnuFileExit: TMenuItem;
    mnuDictionaryTaxon: TMenuItem;
    mnuDictionaryBiotope: TMenuItem;
    mnuMapWindow: TMenuItem;
    mnuTools: TMenuItem;
    mnuToolsOptions: TMenuItem;
    mnuDataEntrySurvey: TMenuItem;
    mnuHelpContents: TMenuItem;
    mnuHelpAbout: TMenuItem;
    mnuDataEntryLocations: TMenuItem;
    mnuDataEntryIndOrg: TMenuItem;
    N1: TMenuItem;
    mnuToolsTermLists: TMenuItem;
    mnuDataEntrySpecies: TMenuItem;
    mnuToolsRucksack: TMenuItem;
    mnuHelpContext: TMenuItem;
    mnuDataEntryPlaces: TMenuItem;
    mnuToolsUserConfig: TMenuItem;
    mnuWindow: TMenuItem;
    mnuWindowsCascade: TMenuItem;
    mnuDataEntryReferences: TMenuItem;
    N2: TMenuItem;
    mnuToolsPassword: TMenuItem;
    Status: TStatusBar;
    mnuToolsSep1: TMenuItem;
    mnuToolsInstall: TMenuItem;
    mnuFileSaveAs: TMenuItem;
    mnuFileClose: TMenuItem;
    mnuFileCloseAll: TMenuItem;
    N5: TMenuItem;
    mnuFilePrint: TMenuItem;
    N3: TMenuItem;
    N6: TMenuItem;
    mnuReports: TMenuItem;
    mnuReportsOpen: TMenuItem;
    mnuReportsWizard: TMenuItem;
    mnuToolsSep2: TMenuItem;
    mnuToolsSep3: TMenuItem;
    mnuToolsImport: TMenuItem;
    mnuToolsSep4: TMenuItem;
    mnuToolsInstallModule: TMenuItem;
    mnuDictionaryAdminArea: TMenuItem;
    mnuToolsDatabase: TMenuItem;
    mnuToolsDatabaseBackup: TMenuItem;
    mnuToolsDatabaseRestore: TMenuItem;
    pmMainToolbar: TPopupMenu;
    pmMainToolbarConfigure: TMenuItem;
    mnuHelpSummary: TMenuItem;
    mnuFileSave: TMenuItem;
    mnuDataEntryCardSplitter: TMenuItem;
    mnuDataEntryNewCard: TMenuItem;
    mnuToolsMergeDataItems: TMenuItem;
    N12: TMenuItem;
    mnuDictionariesNewTaxa: TMenuItem;
    mnuDictionariesNewBiotopes: TMenuItem;
    mnuToolsDatabaseExport: TMenuItem;
    mnuPrintPreview: TMenuItem;
    pmRecordCards: TPopupMenu;
    pmRecordCardsSplitter: TMenuItem;
    pmRecordCardsNew: TMenuItem;
    mnuMapOptions: TMenuItem;
    mnuToolsEditMetadata: TMenuItem;
    mnuReportsLocationReport: TMenuItem;
    mnuReportsTaxonOccurrences: TMenuItem;
    mnuFileOpen: TMenuItem;
    mnuToolsExportManagement: TMenuItem;
    mnuToolsManageRecordingSchemes: TMenuItem;
    mnuToolsDatabaseRebuildGroupIndex: TMenuItem;
    mnuWindowSeparator: TMenuItem;
    CoolBarMain: TCoolBar;
    tbMainToolbar: TXPToolBar;
    tbContext: TXPToolBar;
    ilMDI: TImageList;
    pnlMenuContainer: TPanel;
    tbMenu: TXPToolBar;
    tbtnMnuFile: TXPToolbutton;
    tbtnMnuMerge1: TXPToolbutton;
    tbtnMnuDataEntry: TXPToolbutton;
    tbtnMnuMerge2: TXPToolbutton;
    tbtnMnuDictionaries: TXPToolbutton;
    tbtnMnuMap: TXPToolbutton;
    tbtnMnuReports: TXPToolbutton;
    tbtnMnuTools: TXPToolbutton;
    tbtnMnuWindow: TXPToolbutton;
    tbtnMnuHelp: TXPToolbutton;
    tbMDIButtons: TXPToolBar;
    tbtnMDIMinimise: TXPToolButton;
    tbtnMDINormalise: TXPToolButton;
    tbtnMDIClose: TXPToolButton;
    mnuToolsContribute: TMenuItem;
    mnuToolsManageExportFilters: TMenuItem;
    mnuToolsExportUsingFilter: TMenuItem;
    mnuToolsDatabaseRebuildNameIndex: TMenuItem;
    mnuToolsDatabaseRebuildSynonymIndex: TMenuItem;
    N4: TMenuItem;
    N7: TMenuItem;
    mnuNewCustodian: TMenuItem;
    mnuToolsChangeBackupLocation: TMenuItem;
    mnuReportsQuickReport: TMenuItem;
    N8: TMenuItem;
    pmMapWindow: TPopupMenu;
    mnuMapBrowser: TMenuItem;
    mnuEnhancedTermlists: TMenuItem;
    mnuToolsLoadExternalFilter: TMenuItem;
    mnuToolsRevalidateDatabase: TMenuItem;
    mnuToolsRevalidateSelectedRecords: TMenuItem;
    mnuToolsRunBatchUpdates: TMenuItem;
    mnuToolsBatchUpdates: TMenuItem;
    mnuToolsCommitUpdates: TMenuItem;
    mnuToolsCancelUpdates: TMenuItem;
    mnuEmpty: TMenuItem;
    mnuToolsDatabaseRebuildDesignationIndex: TMenuItem;
    mnuDataEntryCustomSpeciesCards: TMenuItem;
    MnuToolsUpdateDictionary1: TMenuItem;
    procedure mnuFileExitClick(Sender: TObject);
    procedure mnuToolsPasswordClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure mnuToolsInstallAddInClick(Sender: TObject);
    procedure mnuFileCloseClick(Sender: TObject);
    procedure mnuFileCloseAllClick(Sender: TObject);
    procedure mnuWindowsCascadeClick(Sender: TObject);
    procedure mnuFileSaveAsClick(Sender: TObject);
    procedure pmMainToolbarConfigureClick(Sender: TObject);
    procedure mnuHelpSummaryClick(Sender: TObject);
    procedure mnuDataEntryNewCardClick(Sender: TObject);
    procedure mnuRecordingCardClick(Sender: TObject);
    procedure StatusResize(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure mnuFileSaveClick(Sender: TObject);
    procedure mnuHelpContextClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure mnuToolsDatabaseRebuildGroupIndexClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure mnuWindowClick(Sender: TObject);
    procedure tbtnMDIMinimiseClick(Sender: TObject);
    procedure tbtnMDINormaliseClick(Sender: TObject);
    procedure tbtnMDICloseClick(Sender: TObject);
    procedure FormShortCut(var Msg: TWMKey; var Handled: Boolean);
    procedure ToolbarResize(Sender: TObject);
    procedure mnuHelpAboutClick(Sender: TObject);
    procedure mnuToolsDatabaseRebuildNameIndexClick(Sender: TObject);
    procedure mnuToolsDatabaseRebuildSynonymIndexClick(Sender: TObject);
    procedure mnuFileOpenClick(Sender: TObject);
    procedure mnuToolsChangeBackupLocationClick(Sender: TObject);
    procedure mnuReportsQuickReportClick(Sender: TObject);
    procedure mnuToolsBatchUpdatesClick(Sender: TObject);
    procedure mnuToolsDatabaseRebuildDesignationIndexClick(
      Sender: TObject);
    procedure mnuOnLineHelpClick(Sender: TObject);
    procedure MnuToolsUpdateDictionary1Click(Sender: TObject);
  private
    FCurrentForm :TForm;
    FClosing: boolean;
    FClientInstance : TFarProc;
    FPrevClientProc : TFarProc;
    FProgressBar    : TTaskProgressBar;
    FPreviewOwner   : TPreviewOwner;
    FCOMProgressBar : TProgressBar;
    FLastVisibleAddinToolbar: TOleProxy;
    FSchemeManager : TSchemeManager;
    FXPMenu : TXPMenu;
    procedure SetCurrentForm(const Value: TForm);
    procedure ClientWndProc(var Message: TMessage);
    procedure WMTipWindowMsg(var Msg:TMessage); message WM_SHOW_TIP_WINDOW;
    procedure WMLastSessionWindows(var Msg:TMessage); message WM_LAST_SESSION_WINDOWS;
    procedure WMUpdateMenuIcons(var Msg:TMessage); message WM_UPDATE_MENU_ICONS;
    procedure WMCheckMDIButtons(var Msg:TMessage); message WM_CHECK_MDI_BUTTONS;
    // Close QuickReport Previews
    procedure ReadMainSettings;
    procedure WriteMainSettings;
    procedure InitProgressBar;
    procedure OpenScreens;
    function Authorise(RequiredLevel: TUserAccessLevel): Boolean;
    procedure OnDropDownButtonClick(Sender: TObject);
    procedure SetPreviewOwner(const Value: TPreviewOwner);
    procedure SetLastVisibleAddinToolbar(const Value: TOleProxy);
    procedure SetBandWidths;
    procedure InitApplicationTitle;
    procedure AddQuickReportItems(const ASubMenu: TMenuItem);
    procedure QuickReportClick(Sender: TObject);
    procedure FormShowHint(var HintStr: String; var CanShow: Boolean;
      var HintInfo: THintInfo);
    procedure WMVScroll(var Message: TWMVScroll); message WM_VSCROLL;
    procedure AddButtonToToolbar(AToolbar: TToolbar; const AStyle: TToolButtonStyle; const
        AImageIndex: integer; const IsGrouped: boolean; const AHint: string; AAction: TBasicAction;
        AClickEvent: TNotifyEvent; APopupMenu: TPopupMenu = nil);
    procedure AddCustomReportMenuItems(const ASubMenu: TMenuItem;
      const ADataType: TKeyType; const AKey: string; AReportIndex: TBaseReportIndex;
      AQuickReport: Boolean);
    procedure AddWizardReportMenuItem(const AReportFile: string; ASubMenu:
        TMenuItem);
    procedure CustomReportClick(Sender: TObject);
    procedure BatchUpdateClick(Sender: TObject);  
    procedure PopulateReportSubMenu(const ASubMenu : TMenuItem; AMenuEnabled: Boolean;
              AReportIndex: TBaseReportIndex; const ADataTypes: TKeyTypes = [ktDefault];
              const AKey: string=''; AQuickReports: Boolean = False);
    procedure LoadCustomSpeciesCards;
    function  GetOnLineHelpUrl : string;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ApplySecurity;
    procedure SetStatus(const iStatusString : String); overload;
    procedure SetStatus(const iStatusString : String; iPrcMsgs: Boolean);
        overload; deprecated;
    procedure SetReferenceSystemText;
    procedure SetProgress(iProgress: Integer); overload;
    procedure SetProgress(const iProgress: Integer; iPrcMsgs: Boolean);
        overload; deprecated;
    procedure TaskFinished;
    procedure SetMainToolbar;
    procedure ClearContextToolbar(const tfShowHelp: boolean);
    procedure SetContextToolbar(Sender:TObject; mnuSource: TMenuItem;
      const iLastIndex:integer; Popups:array of TPopupMenu);
      overload;
    //overloaded version allows you to choose any part of a menu
    procedure SetContextToolbar(Sender:TObject; mnuSource:TMenuItem;
      const iFirstIndex,iLastIndex:integer; Popups:array of TPopupMenu);
      overload;
    procedure UpdateRecordingCards;
    procedure DisplayRecordingCard(APath:String; ASampleType: TKeyString;
      ASpatialRef, ALocation: String);
    function GetForm(const AFormClass: TFormClass; AForceCreate: boolean = false;
      ABringToFront: boolean=true): TForm;
    procedure BroadcastMessage(const AMessage:integer);
    procedure CreateFavouriteReportMenuOptions;
    procedure EnableCOMProgressBar;
    procedure DisableCOMProgressBar;
    procedure ReapplyOptions;
    procedure SetCOMProgress(AProgress: Integer);
    procedure SetMDIButtonVisible(iShow : Boolean);
    procedure PopulateBatchUpdateSubMenu(const ASubMenu: TMenuItem;
              const ADataType: TKeyType = ktDefault; const AKey: string=''); overload;
    procedure PopulateBatchUpdateSubMenu(const ASubMenu: TMenuItem;
              const ADataTypes: TKeyTypes; const AKey: string=''); overload;
    procedure PopulateBatchUpdateSubMenu(ANode: TFlyNode; const ASubMenu: TMenuItem); overload;
    procedure PopulateQuickReportSubMenu(const ASubMenu: TMenuItem;
              const ADataType: TKeyType=ktDefault; const AKey: string=''); overload;
    procedure PopulateQuickReportSubMenu(const ASubMenu: TMenuItem;
              const ADataTypes: TKeyTypes; const AKey: string=''); overload;
    procedure PopulateQuickReportSubMenu(ANode: TFlyNode; const ASubMenu: TMenuItem); overload;
    procedure UpdateMapWindowSelector;
    procedure RefreshXPMenu;
    property CurrentForm : TForm read FCurrentForm write SetCurrentForm;
    property ProgressBar : TTaskProgressBar read FProgressBar;
    property COMProgressBar : TProgressBar read FCOMProgressBar;
    property Closing: boolean read FClosing;
    property PreviewOwner : TPreviewOwner read FPreviewOwner write SetPreviewOwner;
    property LastVisibleAddinToolbar : TOleProxy read FLastVisibleAddinToolbar write SetLastVisibleAddinToolbar;
    property SchemeManager : TSchemeManager read FSchemeManager;
    property XPMenu : TXPMenu read FXPMenu;
  end;

  {----------------------------------------------------------------------------}
  TCustomReportMenuItem = class(TMenuItem)
  private
    FKey: string;
    FDataType: TKeyType;
    FFileName: string;
    procedure SetKey(const Value: string);
    procedure SetDataType(const Value: TKeyType);
    procedure SetFileName(const Value: string);
  public
    property DataType: TKeyType read FDataType write SetDataType;
    property FileName: string read FFileName write SetFileName;
    property Key: string read FKey write SetKey;
  end;

var
  frmMain: TfrmMain;

//==============================================================================

implementation

{$R *.DFM}
uses
  FormActions, Options, TaxonDictBrowser, BiotopeDictBrowser, ChangePassword, Welcome,
  AddIn, Rucksack, RucksackSave, NewPlaceCard, ReportPreview, PlaceCard, FilterResult,
  ReportDesigner, GeneralData, ValidationData, ComObj, DatabaseUtilities, Locations,
  GeneralFunctions, BaseChildUnit, About, ExportFilters, Snapshot, ServerFolders,
  StrUtils, OLEContainer, MapBrowser, HierarchyNodes, BatchUpdates,
  SpatialRefFuncs, DictionaryUpgrade;

type

  {----------------------------------------------------------------------------}
  TFileMenuItem = class(TMenuItem)
  private
    FOriginalFileName: string;
    procedure SetOriginalFileName(const Value: string);
  public
    property OriginalFileName: string read FOriginalFileName write SetOriginalFileName;
  end;

//==============================================================================
constructor TfrmMain.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  InitApplicationTitle;
  FCLosing := False;
  FPreviewOwner := poActiveForm;

  AppSettings.ReadDatabaseSettings;

  ReadMainSettings;  // Read position of the main window
  { Create a progress bar and stick it on the status panel }
  InitProgressBar;
  FClientInstance := Classes.MakeObjectInstance(ClientWndProc);
  FPrevClientProc := Pointer(GetWindowLong(ClientHandle, GWL_WNDPROC));
  SetWindowLong(ClientHandle, GWL_WNDPROC, LongInt(FClientInstance));

  // Get the toolbar settings
  AppSettings.ReadMainBandSettings(Self, tbMainToolbar, dmFormActions.alForms);
  AppSettings.ReadContextBandSettings(Self, tbContext);

  Application.OnShowHint:= FormShowHint;
  CoolBarMain.ShowHint  := AppSettings.ShowToolTips;
  ShowHint              := AppSettings.ShowToolTips;
  tbMainToolbar.Visible := AppSettings.ShowMainToolbar;
  if (tbMainToolbar.ButtonCount = 0) and AppSettings.ShowMainToolbar then
    SetMainToolbar;
    
  FProgressBar.TaskPosition := 0;
  LastVisibleAddinToolbar   := nil;

  UpdateRecordingCards;
  LoadCustomSpeciesCards;
  FSchemeManager := TSchemeManager.Create(mnuToolsContribute);
  TExportFilter.SetExportFilterMenu(mnuToolsExportUsingFilter, dmFormActions.ExportUsingFilterClick);

  AppSettings.UpdateMapWindowSelectors;

  //Register custom report preview window
  RegisterPreviewClass(TQRReportPreviewInterface);

  //Apply security
  ApplySecurity;

  // Initialise XP Appearance for menus
  FXPMenu := TXPMenu.Create(Self);
  with FXPMenu do begin
    XPControls := [xcMainMenu, xcPopupMenu];
    Active     := AppSettings.ShowMenuIcons;
    Gradient   := AppSettings.GraduatedMenus;
  end;
  SendMessage(Handle, WM_UPDATE_MENU_ICONS, 0, 0);
  tbtnMnuDictionaries.Caption := ResStr_Dictionaries;

  //Setup Help
  OHelp.AppHandle := Self.Handle;
  HelpContext := IDH_OVERVIEW;
  mnuData.HelpContext:=IDH_DATAENTRYMENU;
  mnuDictionary.HelpContext:=IDH_DICTIONARIESMENU;
  mnuMainMap.HelpContext:=IDH_MAPMENU;
  mnuHelp.HelpContext:=IDH_HELPMENU;
  mnuTools.HelpContext:=IDH_TOOLSMENU;
  mnuFile.HelpContext:=IDH_FILEMENU;
  mnuReports.HelpContext:=IDH_REPORTSMENU;
  mnuWindow.HelpContext:=IDH_WINDOWMENU;
  FormResize(nil);  // ensure correct MDI button positions
  CreateFavouriteReportMenuOptions;
end;  //Create

//==============================================================================
{ Initialise the progress bar and attach it to the panel }
procedure TfrmMain.InitProgressBar;
begin
  FProgressBar := TTaskProgressBar.Create(Self);
  FProgressBar.Parent := Status;
  FProgressBar.Smooth := True;
  StatusResize(Self);
end;  // InitProgressBar

{===============================================================================
 Description : Initialises any captions that depend on the application title
 Created : 06/02/2003 }
procedure TfrmMain.InitApplicationTitle;
begin
  Caption := Application.Title;
  mnuHelpAbout.Hint := Format(ResStr_About, [Application.Title]);
  mnuHelpAbout.Caption := '&' + mnuHelpAbout.Hint;
end;

//==============================================================================
procedure TfrmMain.ReadMainSettings;
begin
  with AppSettings do begin
    Left  :=MainWindowPos.Left;
    Top   :=MainWindowPos.Top;
    Width :=MainWindowPos.Right;
    Height:=MainWindowPos.Bottom;
    WindowState:=MainWindowState;
  end;
end;  // ReadMainSettings

//==============================================================================
procedure TfrmMain.WriteMainSettings;
begin
  with AppSettings do begin
    MainWindowState:=WindowState;
    MainWindowPos  :=Rect(Left,Top,Width,Height);
  end;
end;  // WriteMainSettings

//==============================================================================
procedure TfrmMain.OpenScreens;
var lForms: TStringList;
    i: Integer;

  procedure CheckAndOpen(AScreenName: String);
  begin
    if CompareText(AScreenName, 'AdminAreaDictBrowser') = 0 then
      dmFormActions.actAdminAreaDiction.Execute
    else if CompareText(AScreenName, 'BiotopeDictEditor') = 0 then
      dmFormActions.actNewBiotopes.Execute
    else if CompareText(AScreenName, 'BiotopeDictBrowser') = 0 then
      dmFormActions.actBiotopeDiction.Execute
    else if CompareText(AScreenName, 'IndOrg') = 0 then
      dmFormActions.actNames.Execute
    else if CompareText(AScreenName, 'Locations') = 0 then
      dmFormActions.actLocations.Execute
    else if CompareText(AScreenName, 'Map') = 0 then
      dmFormActions.actMapWindow.Execute
    else if CompareText(AScreenName, 'MapBrowser') = 0 then
      dmFormActions.actMapBrowser.Execute
    else if CompareText(AScreenName, 'Observations') = 0 then
      dmFormActions.actObservations.Execute
    else if CompareText(AScreenName, 'References') = 0 then
      dmFormActions.actDocuments.Execute
    else if CompareText(AScreenName, 'RuckSack') = 0 then
      dmFormActions.actRuckSack.Execute
    else if CompareText(AScreenName, 'TaxonDictEditor') = 0 then
      dmFormActions.actNewTaxa.Execute
    else if CompareText(AScreenName, 'TaxonDictBrowser') = 0 then
      dmFormActions.actTaxonDiction.Execute
    else if CompareText(AScreenName, 'TermLists') = 0 then
      dmFormActions.actTermsLists.Execute;
  end;

begin
  lForms := TStringList.Create;
  with TRegistry.Create do
    try
      if OpenKey(REG_KEY_FORMS, False) then begin
        lForms.Clear;
        GetKeyNames(lForms);
        CloseKey;
        for i := 0 to lForms.Count - 1 do begin
          OpenKey(REG_KEY_FORMS + '\' + lForms[i], False);
          if ValueExists('Opened') and ReadBool('Opened') then
            CheckAndOpen(lForms[i]);
          CloseKey;
        end;
      end;
    finally
      lForms.Free;
      Free;
    end;
end;  // OpenScreens

//==============================================================================
procedure TfrmMain.FormCreate(Sender: TObject);
begin
  inherited;
  // Initialise before message gets posted. If some addins fail to start, a message box
  // comes up, and the WM_LAST_SESSION_WINDOWS will be dealt with while the box is still up,
  // and will cause errors when trying to open those windows.
  AppSettings.InitComAddins;
  SetReferenceSystemText;
  // display all forms that are required
  PostMessage(Handle, WM_LAST_SESSION_WINDOWS, 0, 0);
end;  // FormCreate

//==============================================================================
procedure TfrmMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if AppSettings.HasUncommittedUpdates then
    case MessageDlg(ResStr_HaveUncommittedUpdates, mtWarning, mbYesNoCancel, 0) of
      mrYes: dmFormActions.actCommitUpdatesExecute(nil);
      mrNo: dmFormActions.actCancelUpdatesExecute(nil);
      mrCancel: CanClose := False;
    end;
  if CanClose then begin
    FClosing := True;
    WriteMainSettings;  // Save main window position
    AppSettings.WriteMainBandSettings(tbMainToolbar);
    AppSettings.WriteContextBandSettings(tbContext);
    mnuFileCloseAllClick(nil);
  end;
end;  // FormCloseQuery

//==============================================================================
procedure TfrmMain.WMLastSessionWindows(var Msg:TMessage);
begin
  if AppSettings.ShowLastSessionWindows then OpenScreens;
  if AppSettings.ShowWelcomeAtStart then
    PostMessage(Handle, WM_SHOW_TIP_WINDOW, 0, 0);
  RefreshXPMenu;
end;  // WMLastSessionWindows

//==============================================================================
procedure TfrmMain.WMTipWindowMsg(var Msg: TMessage);
var SelectedOption:byte;
begin
  with TdlgWelcome.Create(nil) do
    try
      ShowModal;
      SelectedOption:=QuickStartOption;
    finally
      Free;
    end;
  if SelectedOption = 1 then mnuDataEntryNewCardClick(Self) else
  if SelectedOption = 2 then dmFormActions.actLocations.Execute else
  if SelectedOption = 3 then dmFormActions.actReportWizard.Execute;
end;  // WMTipWindowMsg

//==============================================================================
procedure TfrmMain.mnuFileExitClick(Sender: TObject);
begin
  Close;
end;  // Exit1Click

//==============================================================================
procedure TfrmMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  // There are messages flying around in Recorder, so process them, or try to,
  // before terminating.
  Application.ProcessMessages;

  AppSettings.DiscardComAddins;
  AppSettings.WriteDatabaseSettings;
  AppSettings.WriteRegistrySettings;
  inherited;
end;  // FormClose

//==============================================================================
destructor TfrmMain.Destroy;
begin
  // No need to set this one to nil.
  if MapBrowserWindow <> nil then MapBrowserWindow.Free;

  FSchemeManager.Free;
  FSchemeManager := nil;

  FProgressBar.Free;
  FProgressBar := nil;
  
  if MDIChildCount > 0 then begin
    mnuFileCloseAllClick(nil);
    Application.ProcessMessages;
  end;

  inherited;
end;  // Destroy

//==============================================================================
procedure TfrmMain.SetCurrentForm(const Value: TForm);
begin
  FCurrentForm := Value;
end;  // SetCurrentForm

//==============================================================================
procedure TfrmMain.mnuToolsPasswordClick(Sender: TObject);
begin
  with TdlgChangePassword.Create(nil) do
    try
      ShowModal;
    finally
      Free;
    end;
end;  // mnuToolsPasswordClick

//==============================================================================
procedure TfrmMain.pmMainToolbarConfigureClick(Sender: TObject);
begin
  dmFormActions.actOptionsExecute(pmMainToolbarConfigure);
end;  // pmMainToolbarConfigureClick

//==============================================================================
procedure TfrmMain.mnuToolsInstallAddInClick(Sender: TObject);
begin
  with TdlgAddIn.Create(nil) do
    try
      ShowModal;
    finally
      Free;
    end;
end;  // mnuToolsInstallAddInClick

//==============================================================================
procedure TfrmMain.mnuFileCloseClick(Sender: TObject);
begin
  if ActiveMDIChild<>nil then ActiveMDIChild.Close;
end;  // mnuFileCloseClick

//==============================================================================
procedure TfrmMain.mnuFileCloseAllClick(Sender: TObject);
var iCount:integer;
begin
  for iCount:=MDIChildCount-1 downto 0 do MDIChildren[iCount].Close;
end;  // mnuFileCloseAllClick

//==============================================================================
procedure TfrmMain.mnuWindowsCascadeClick(Sender: TObject);
begin
  Cascade;
end;  // mnuWindowsCascadeClick

//==============================================================================
// This handles the painting of the background
procedure TfrmMain.ClientWndProc(var Message: TMessage);
var FormDC         : hDC;
    TileRow,TileCol: Word;
    lPic           : TPicture;
begin
  if not Application.Terminated then
    with Message do
      case Msg of
        WM_ERASEBKGND:
          begin
            FormDC := TWMEraseBkGnd(Message).DC;
            if AppSettings.PlainBackground then
              Result := CallWindowProc(FPrevClientProc, ClientHandle, Msg, wParam, lParam)
            else begin
              lPic := AppSettings.BackImage;
              for TileRow := 0 to ClientHeight div lPic.Height do
                for TileCol := 0 to ClientWidth div lPic.Width do
                  BitBlt(FormDC,
                         TileCol * lPic.Width,
                         TileRow * lPic.Height,
                         lPic.Width,
                         lPic.Height,
                         lPic.Bitmap.Canvas.Handle, 0, 0, SRCCOPY);
              Result := 1;
            end;
          end;
      else
        Result := CallWindowProc(FPrevClientProc, ClientHandle, Msg, wParam, lParam);
      end;
end;  // ClientWndProc

//==============================================================================
procedure TfrmMain.WMVScroll(var Message: TWMVScroll);
begin
  inherited;
  InvalidateRect(ClientHandle,Nil,True);
end;  // WMVScroll

//==============================================================================
procedure TfrmMain.OnDropDownButtonClick(Sender:TObject);
var lIdx:integer;
begin
  with TToolButton(Sender).DropDownMenu do
    for lIdx:=0 to Items.Count-1 do
      if Items[lIdx].Default then begin
        if Items[lIdx].Action = nil then
          // If the item doens't have an associated action, call its OnClick handler
          Items[lIdx].OnClick(Sender)
        else
          // Otherwise, call the Action execute handler
          Items[lIdx].Action.OnExecute(Sender);
        Break;
      end;
end;  // OnDropDownButtonClick

//==============================================================================
procedure TfrmMain.AddButtonToToolbar(AToolbar: TToolbar; const AStyle: TToolButtonStyle; 
    const AImageIndex: integer; const IsGrouped: boolean; const AHint: string; AAction:
    TBasicAction; AClickEvent: TNotifyEvent; APopupMenu: TPopupMenu = nil);
var tbtnNew:TXPToolButton;
begin
  tbtnNew := TXPToolButton.Create(AToolbar);
  with tbtnNew do begin
    OnClick := AClickEvent;
    Style   := AStyle;
    ImageIndex := AImageIndex;
    if AStyle = tbsSeparator then
      Width := 8
    else
    if AStyle = tbsDropDown  then begin
      Width := 39;
      DropdownMenu := APopupMenu;
      // This event will look for a default item on the associated popupmenu
      // and the appropriate Click or Action.Execute will be triggered
      OnClick := OnDropDownButtonClick;
    end;
    Hint := AHint;
    if Assigned(AAction) then
      // This will set all properties like caption, Hint, ImageIndex
      Action := AAction;
    // put the button on the toolbar
    Parent := AToolbar;
    Grouped := True; //tfGrouped;
    AllowAllUp := True; // not tfGrouped;
  end;
end;  // AddButtonToToolbar

//==============================================================================
{ Sets up the main toolbar with its default list of buttons }
procedure TfrmMain.SetMainToolbar;
  //----------------------------------------------------------------------------
  procedure AddMainButton(mnuItem:TMenuItem);
  begin
    with mnuItem do
      AddButtonToToolbar(tbMainToolbar,tbsButton,ImageIndex,False,Hint,Action,OnClick);
  end;  // AddMainButton
  //----------------------------------------------------------------------------
begin
  // Always add buttons on a toolbar in reverse order as they are added at the
  // start of the toolbar
  AddMainButton(mnuToolsRucksack);
  AddButtonToToolbar(tbMainToolbar, tbsSeparator, -1, False, '', nil, nil);
  AddButtonToToolbar(tbMainToolbar, tbsDropDown, -1, False, '',
                     dmFormActions.actMapWindow, nil, pmMapWindow);
  AddButtonToToolbar(tbMainToolbar, tbsSeparator, -1, False, '', nil, nil);
  AddMainButton(mnuDictionaryAdminArea);
  AddMainButton(mnuDictionaryBiotope);
  AddMainButton(mnuDictionaryTaxon);
  AddButtonToToolbar(tbMainToolbar, tbsSeparator, -1, False, '', nil, nil);
  AddMainButton(mnuDataEntryReferences);
  AddMainButton(mnuDataEntryIndOrg);
  AddMainButton(mnuDataEntryLocations);
  AddMainButton(mnuDataEntrySurvey);
  SetBandWidths;
end;  // SetMainToolbar

//==============================================================================
procedure TfrmMain.SetContextToolbar(Sender:TObject; mnuSource:TMenuItem;
  const iLastIndex:integer; Popups:array of TPopupMenu);
begin
  // call overloaded method, specifying count to start at
  SetContextToolbar(Sender, mnuSource, mnuSource.Count - 1, iLastIndex, Popups);
  SetBandWidths;
end;  // SetContextToolbar

//==============================================================================
procedure TfrmMain.ClearContextToolbar(const tfShowHelp: boolean);
var i: integer;
begin
//  Application.ProcessMessages;
      // get any toolbar button "mouse up" messages out of the queue before
      // the associated controls are destroyed (otherwise Bad Things happen)

  LockWindowUpdate(Handle);
  for i := tbContext.ButtonCount - 1 downto 0 do
    tbContext.Buttons[i].Free;
  tbContext.Visible := tfShowHelp;

  if tfShowHelp then begin  // Add Help button if not already on Main Toolbar
    i := tbMainToolbar.ButtonCount - 1;
    // Look for Help button on Main Toolbar
    while (i >= 0) and (tbMainToolbar.Buttons[i].Action<>dmFormActions.actHelp) do Dec(i);

    if i = -1 then begin  // if not found, add it, and a separator
      with dmFormActions do
        AddButtonToToolbar(tbContext, tbsButton, actHelp.ImageIndex, False, actHelp.Hint, actHelp, nil);
      AddButtonToToolbar(tbContext, tbsSeparator, -1, False, '', nil, nil);
    end;
  end;
  SetBandWidths;  
end;  // ClearContextToolbar

//==============================================================================
procedure TfrmMain.mnuHelpSummaryClick(Sender: TObject);
begin
  PostMessage(Handle, WM_SHOW_TIP_WINDOW, 0, 0);
end;  // mnuHelpSummaryClick

//==============================================================================
//==============================================================================
{ Create a Preview window, owned by the correct form }
function TQRReportPreviewInterface.GetWindow(AQRPrinter: TQRPrinter): TWinControl;
begin
  case frmMain.PreviewOwner of
    poApplication :
      Result := TfrmReportPreview.CreatePreview(Application, AQRPrinter);
    poActiveForm  :
      Result := TfrmReportPreview.CreatePreview(frmMain.ActiveMDIChild, AQRPrinter);
  else
      Result:=nil;
  end;
end;  // TQRReportPreviewInterface.GetWindow

//------------------------------------------------------------------------------
function TQRReportPreviewInterface.Show(AQRPrinter : TQRPrinter) : TWinControl;
begin
  Result := GetWindow(AQRPrinter);
  // You can set options for your preview here
  TfrmReportPreview(Result).Show;
end;  // TQRReportPreviewInterface.Show

//==============================================================================
function TQRReportPreviewInterface.ShowModal(AQRPrinter : TQRPrinter) : TWinControl;
begin
  Result := GetWindow(AQRPrinter);
  // You can set options for your preview here
  TfrmReportPreview(Result).ShowModal;
end;  // TQRReportPreviewInterface.ShowModal

//==============================================================================
{ Set the status bar text }
procedure TfrmMain.SetStatus(const iStatusString: String);
begin
  Status.Panels[0].text := iStatusString;
  Status.Refresh;
end;  // SetStatus

{ THIS OVERLOAD IS DEPRECATED }
procedure TfrmMain.SetStatus(const iStatusString: string; iPrcMsgs: Boolean);
begin
  Status.Panels[0].text := iStatusString;
  // ensure it is updated
  if iPrcMsgs then
    Application.ProcessMessages;
end;  // SetStatus

//==============================================================================
{ Show the currently selected spacial reference system in the status bar }
procedure TfrmMain.SetReferenceSystemText();
var
  lReferenceSystemCode, lStatusBarText: String;

  i: Integer;
  lSpatialSystem: TSpatialSystemName;
  lAddin: IRecorderAddin;
  lNamedSystem: INamedSpatialReference;
  lBaseMapFormat: IBaseMapFormat;
  intf: IInterface;
begin
  lReferenceSystemCode := GetCurrentSpatialRefSystem;
  if lReferenceSystemCode = 'OSGB' then
    lStatusBarText := ResStr_GBTitle
  else if lReferenceSystemCode = 'OSNI' then
    lStatusBarText := ResStr_NITitle
  else if lReferenceSystemCode = 'LTLN' then
    lStatusBarText := ResStr_LatLongTitle
  else if lReferenceSystemCode = 'UTM' then
    lStatusBarText := ResStr_UTMTitle
  else
  begin
    for i := 0 to AppSettings.ComAddins.SpatialSystemInterfaces.Count - 1 do
    begin
      intf := AppSettings.ComAddins.SpatialSystemInterfaces[i];

      lSpatialSystem := TSpatialSystemName.Create;
      lSpatialSystem.Name := (intf as ISpatialReference).SpatialRefSystem;
      if lSpatialSystem.Name = lReferenceSystemCode then
      begin
        if Supports(intf, IID_INamedSpatialReference, lNamedSystem) then
        begin
          if not Supports(intf, IID_IBaseMapFormat, lBaseMapFormat) then
            lStatusBarText := (intf as INamedSpatialReference).SpatialSystemName;
        end else
        if Supports(intf, IID_IRecorderAddin, lAddin) then
          lStatusBarText := (intf as IRecorderAddin).Name
        else
          lStatusBarText := lSpatialSystem.Name;
      end;
    end;
  end;

  Status.Panels[1].Text :=
      ResStr_SpatialReferenceSystemLabel + ' ' + lStatusBarText;

  StatusResize(nil);
  Status.Refresh;
end;  //SetReferenceSystemText

//==============================================================================
{ When a task is finished, clean up the progress/status bar }
procedure TfrmMain.TaskFinished;
begin
  SetStatus('');
  ProgressBar.Reset;
end;  // TaskFinished

//==============================================================================
procedure TfrmMain.mnuDataEntryNewCardClick(Sender: TObject);
begin
  with TdlgNewPlaceCard.Create(nil) do
    try
      if ShowModal=mrOk then
        UpdateRecordingCards;
    finally
      Free;
    end;
end;  // mnuDataEntryNewCardClick

//==============================================================================
procedure TfrmMain.UpdateRecordingCards;
var i: Integer;
    mnuItem, popupItem: TMenuItem;
begin
  //Delete all existing recording card Items, keep splitter and New card Items
  with mnuDataEntrySpecies do
    for i := Count - 3 downto 0 do
    begin
      // Remove from Main menu
      mnuItem := Items[i];
      Delete(i);
      mnuItem.Free;
      // Remove from Popup menu
      popupItem := pmRecordCards.Items[i];
      pmRecordCards.Items.Delete(i);
      popupItem.Free;
    end;

  // Refresh list of recording cards
  AppSettings.GetRecordingCards;

  // Add new list of recording cards
  with AppSettings.RecordingCards do
    for i := Items.Count - 1 downto 0 do
    begin
      // Setup main menu
      mnuItem            := TMenuItem.Create(mnuMain);
      mnuItem.Caption    := DuplicateCharacters(Items[i].Caption, '&');
      mnuItem.ImageIndex := Items[i].ImageIndex;
      mnuItem.OnClick    := mnuRecordingCardClick;
      //Set security
		  mnuItem.Enabled    := Authorise(ualRecorder);
      mnuDataEntrySpecies.Insert(0, mnuItem);

      // setup popup menu
      popupItem := TMenuItem.Create(pmRecordCards);
      popupItem.Caption    := DuplicateCharacters(Items[i].Caption, '&');
      popupItem.ImageIndex := Items[i].ImageIndex;
      popupItem.OnClick    := mnuRecordingCardClick;
      //Set security
		  popupItem.Enabled    := Authorise(ualRecorder);
      pmRecordCards.Items.Insert(0, popupItem);
    end;

  // Show splitter only if needed
  mnuDataEntryCardSplitter.Visible := mnuDataEntrySpecies.Count > 2;
  if pmRecordCardsSplitter <> nil then
    pmRecordCardsSplitter.Visible := mnuDataEntrySpecies.Count > 2;

  // Get Observations to refresh teh Sample context menu.
  BroadcastMessage(WM_UPDATE_SAMPLE_LOAD_CARD);

  RefreshXPMenu;
end;  // UpdateRecordingCards

(**
  Loads any custom species recording cards defined in XML files
*)
procedure TfrmMain.LoadCustomSpeciesCards;
var
  lSearchRec: TSearchRec;
  lNewItem: TMenuItem;

  procedure AddItem;
  begin
    if SameText(ExtractFileExt(lSearchRec.Name), '.xml') then begin
      //Add the file to the menu
      lNewItem:= TMenuItem.Create(mnuDataEntryCustomSpeciesCards);
      lNewItem.Caption:= DuplicateCharacters(copy(lSearchRec.Name,0,length(lSearchRec.Name) - 4), '&');
      lNewItem.OnClick := dmFormActions.actSpeciesRecordExecute;
      mnuDataEntryCustomSpeciesCards.Add(lNewItem);
    end;
  end;

begin
  mnuDataEntryCustomSpeciesCards.Visible := false;
  //Find the first file.
  if FindFirst(AppSettings.CustomSpeciesCardPath + '*.xml', 0, lSearchRec) = 0 then
  begin
    mnuDataEntryCustomSpeciesCards.Visible := true;
    AddItem;

    //Add a new menu item for each remaining file
	  while FindNext(lSearchRec) = 0 do
	    AddItem;

    //Free the search results
	  FindClose(lSearchRec);
  end;
end;

//==============================================================================
procedure TfrmMain.mnuRecordingCardClick(Sender: TObject);
var cardPath: String;
    ampPos: Integer;
begin
  //Show the recording card and set the mode
  if AppSettings.RecordingCardPath = '' then
    cardPath := ExtractFilePath(Application.ExeName)
  else
    cardPath := AppSettings.RecordingCardPath;
  cardPath := cardPath + StringReplace(TMenuItem(Sender).Caption, '&&', '&', [rfReplaceAll]) + '.crd';

  ampPos := Pos('&', cardPath);
  if ampPos > 0 then
    cardPath := Copy(cardPath, 1, ampPos - 1) + Copy(cardPath, ampPos + 1, 255);

  AppSettings.PlaceCardToLoad := cardPath;
  DisplayRecordingCard(cardPath, '', '', '');
end;  // mnuRecordingCardClick

//==============================================================================
procedure TfrmMain.DisplayRecordingCard(APath: String; ASampleType: TKeyString;
  ASpatialRef, ALocation: String);
begin
  dmFormActions.DisplayForm(TfrmPlaceCard);
  with TfrmPlaceCard(frmMain.ActiveMDIChild) do begin
    RecordingCardPath := APath;
    SampleType        := ASampleType;
    PassedSpatialRef  := ASpatialRef;
    PassedLocation    := ALocation;
  end;
end;  // DisplayRecordingCard

//==============================================================================
{ Handle resize of the status bar, especially the progress bar }
procedure TfrmMain.StatusResize(Sender: TObject);
begin
  // The canvas font is normally only set by TCustomStatusBar when it is
  // drawing items; we set it explicitly here to make sure that we always
  // have the correct font when calculating the text width below.
  Status.Canvas.Font := Status.Font;

  { Make the spatial reference information panel only as wide as it needs to be,
    and allocate any saved space to the message panel. }
  Status.Panels[1].Width := Min(Width div 8 * 3,
    Status.Canvas.TextWidth(Status.Panels[1].Text) + 5);

  Status.Panels[0].Width := (Width div 8 * 3 - Status.Panels[1].Width) +
    (Width div 8 * 3 - 20);// 20 to allow for the 'handle'

  Status.Panels[2].Width := Width div 4;
  { The following arbitrary numbers were chosen just to give a nice consistent
      border around the progress bar }
  FProgressBar.Left   := Status.Left + Status.Panels[0].Width + Status.Panels[1].Width + 4;
  FProgressBar.Width  := Status.Panels[2].Width - 5;
  FProgressBar.Height := Status.Height - 6;
  FProgressBar.Top    := 4;
end;  // StatusResize

//==============================================================================
function TfrmMain.GetForm(const AFormClass: TFormClass; AForceCreate: boolean = false;
    ABringToFront: boolean = true): TForm;
var i: Integer;
begin
  Result := nil;
  for i := 0 to MDIChildCount - 1 do
    if MDIChildren[i] is AFormClass then begin
      Result := TForm(MDIChildren[i]);
      if ABringToFront then
        Result.BringToFront;
      Exit;
    end;
  if AForceCreate then
    Result := dmFormActions.DisplayForm(AFormClass);
end;  // GetForm

//==============================================================================
procedure TfrmMain.mnuFileSaveClick(Sender: TObject);
begin
  if ActiveMDIChild is TfrmFilterResult then
    // Always do SaveAs for Wizard result
    TfrmFilterResult(ActiveMDIChild).SaveReport
  else if ActiveMDIChild is TfrmReportDesigner then
    TfrmReportDesigner(ActiveMDIChild).SaveDesign
  else if ActiveMDIChild is TfrmRucksack then
    TfrmRucksack(ActiveMDIChild).SaveRucksack(AppSettings.CurrentRucksack.FileName)
  else if ActiveMDIChild is TfrmMap then
    TfrmMap(ActiveMDIChild).SaveMapImage(AppSettings.MapImageFilePath, True)
  else if ActiveMDIChild is TfrmSnapshot then
    TfrmSnapshot(ActiveMDIChild).SaveSnapshot;
end;  // mnuFileSaveClick

//==============================================================================
procedure TfrmMain.mnuFileSaveAsClick(Sender: TObject);
begin
  if ActiveMDIChild is TfrmFilterResult then
    TfrmFilterResult(ActiveMDIChild).SaveReportAs
  else if ActiveMDIChild is TfrmReportDesigner then
    TfrmReportDesigner(ActiveMDIChild).SaveDesignAs
  else if ActiveMDIChild is TfrmRucksack then
    TfrmRucksack(ActiveMDIChild).SaveRucksackAs
  else if ActiveMDIChild is TfrmMap then
    TfrmMap(ActiveMDICHild).SaveMapImageAs
  else if ActiveMDIChild is TfrmSnapshot then
    TfrmSnapshot(ActiveMDIChild).SaveSnapshotAs;
end;  // mnuFileSaveAsClick

//==============================================================================
procedure TfrmMain.ApplySecurity;
var lRecorder, lAddOnly, lAdmin, lFullUser:boolean;
begin
  lRecorder:=Authorise(ualRecorder);
  lAddOnly :=Authorise(ualAddOnly);
  lFullUser := Authorise(ualFullUser);
  lAdmin   :=Authorise(ualAdmin);

	mnuDataEntrySpecies.Visible      := lRecorder; // Data Entry menu
  mnuDataEntryNewCard.Visible      := lFullUser; // New Recording Card
  pmRecordCardsNew.Visible         := lFullUser;
	mnuToolsInstall.Visible          := lAdmin;    // Install menu
	mnuToolsDatabase.Visible         := lAdmin;    // Database Tools menu
  mnuToolsExportManagement.Visible := lFullUser;
  mnuToolsBatchUpdates.Visible     := lAdmin;

  with dmFormActions do begin
    SetActionVisibility(actEditMetadata, True);
    SetActionVisibility(actImport, True);
    SetActionVisibility(actManageSchemes, True);
    SetActionVisibility(actManageExportFilters, True);
    actSpeciesRecord.Visible := lRecorder;  // Species Record
    actRuckSack.Visible      := lRecorder;  // Rucksack
    actUserConfig.Visible    := lAdmin;     // User Config
    actConfigAddIn.Visible   := lAdmin;     // Add-in
    actMergeData.Visible     := lAdmin;     // Merge data
    actNewTaxa.Visible       := lAddOnly;   // New Taxa
    actNewBiotopes.Visible   := lAddOnly;   // New Biotopes
    actRunBatchUpdates.Visible := lAdmin;
  end;
end;  // ApplySecurity

//==============================================================================
function TfrmMain.Authorise(RequiredLevel: TUserAccessLevel): Boolean;
begin
  //Result is True if the current users access level is greater than or equal to Required
  Result:= AppSettings.UserAccessLevel >= RequiredLevel;
end;  // Authorise

//==============================================================================
procedure TfrmMain.WMUpdateMenuIcons(var Msg: TMessage);
begin
  if AppSettings.ShowMenuIcons then mnuMain.Images:=dmFormActions.ilMenuOn
                               else mnuMain.Images:=nil;
  FXPMenu.Active := AppSettings.ShowMenuIcons;
  FXPMenu.Gradient := AppSettings.GraduatedMenus;
  RefreshXPMenu;
end;  // WMUpdateMenuIcons

//==============================================================================
procedure TfrmMain.mnuHelpContextClick(Sender: TObject);
var
  lHelpIntf: IHelp;
begin
  if ActiveMDIChild = nil then
    OHelp.ShowContext(Self.HelpContext)
  else if ActiveMDIChild.ActiveControl is TOleProxy then begin
    if Supports(TOleProxy(ActiveMDIChild.ActiveControl).OleObject, IHelp, lHelpIntf) then
      lHelpIntf.DoHelpContents
    else
      OHelp.ShowContext(ActiveMDIChild.HelpContext);
  end else
    OHelp.ShowContext(ActiveMDIChild.HelpContext);
end;  // mnuHelpContextClick

//==============================================================================
procedure TfrmMain.BroadcastMessage(const AMessage: integer);
var i:integer;
begin
  for i := 0 to MDIChildCount - 1 do
    PostMessage(MDIChildren[i].Handle, AMessage, 0, 0);
end;  // BroadcastMessage

//==============================================================================
{ Preview owner property indicates if created previews should be application or
    form owned }
procedure TfrmMain.SetPreviewOwner(const Value: TPreviewOwner);
begin
  FPreviewOwner := Value;
end;  // SetPreviewOwner

//==============================================================================
{ Frees COM progress bar and sets it to nil }
procedure TfrmMain.DisableCOMProgressBar;
begin
  FCOMProgressBar.Free;
  FCOMProgressBar := nil;
  FProgressBar.Visible := true;
end; // DisableCOMProgressBar

//==============================================================================
{ Creates a progress bar for addins to use and overlay it on the main form's progress bar }
procedure TfrmMain.EnableCOMProgressBar;
begin
  FCOMProgressBar := TProgressBar.Create(Self);
  with FCOMProgressBar do begin
    Parent := Status;
    Smooth := True;
    SetBounds(FProgressBar.Left,
              FProgressBar.Top,
              FProgressBar.Width,
              FProgressBar.Height);
    Anchors:= FProgressBar.Anchors;
    DoubleBuffered := true;
    FProgressBar.Visible := False;
  end;
end; // EnableCOMProgressBar

//==============================================================================
{ Allows COM addins to update progress in COM status bar }
procedure TfrmMain.SetCOMProgress(AProgress: Integer);
begin
  if Assigned(FCOMProgressBar) and (FComProgressBar.Position <> AProgress) then
    FCOMProgressBar.Position := AProgress;
end; // SetCOMProgress

//==============================================================================
procedure TfrmMain.SetContextToolbar(Sender: TObject; mnuSource: TMenuItem;
  const iFirstIndex, iLastIndex: integer; Popups: array of TPopupMenu);
var
  i: Integer;
begin
  try
    // Buttons are added at the front of a toolbar, so go at it in reverse.
    for i := iFirstIndex downto iLastIndex do begin
      if mnuSource.Items[i].ImageIndex <> -1 then
        { if a valid popup menu supplied, then attach to button }
        if (i <= High(Popups)) and  Assigned(Popups[i]) then
          AddButtonToToolbar(tbContext, tbsDropDown,
                             mnuSource.Items[i].ImageIndex, False,
                             mnuSource.Items[i].Hint,
                             mnuSource.Items[i].Action, nil, Popups[i])
        else if mnuSource.Items[i].RadioItem then
          AddButtonToToolbar(tbContext, tbsCheck,
                             mnuSource.Items[i].ImageIndex, True,
                             mnuSource.Items[i].Hint,
                             mnuSource.Items[i].Action,
                             mnuSource.Items[i].OnClick)
        else
          AddButtonToToolbar(tbContext, tbsButton,
                             mnuSource.Items[i].ImageIndex, False,
                             mnuSource.Items[i].Hint,
                             mnuSource.Items[i].Action,
                             mnuSource.Items[i].OnClick)
      else
      // Don't want trailing/leading/doubled separators in toolbar
      if (mnuSource.Items[i].Caption = '-') and (tbContext.ButtonCount > 0) and
         (tbContext.Buttons[0].Style <> tbsSeparator) and (i <> iLastIndex) then
        AddButtonToToolbar(tbContext, tbsSeparator, -1, False, '', nil, nil);
    end;
    tbContext.Visible := AppSettings.ShowContextToolbar;
    SetBandWidths;
  finally
    LockWindowUpdate(0);
  end;
end;  // SetContextToolbar

//==============================================================================
{ Accessor method for the last visible ole toolbar.  This allows forms to hide
    it when they are activated }
procedure TfrmMain.SetLastVisibleAddinToolbar(const Value: TOleProxy);
begin
  FLastVisibleAddinToolbar := Value;
end;  // SetLastVisibleAddinToolbar

//==============================================================================
procedure TfrmMain.mnuToolsDatabaseRebuildGroupIndexClick(Sender: TObject);
var
  lCursor : TCursor;
begin
  if MessageDlg(ResStr_GroupRebuildingTime, mtWarning, [mbYes, mbNo],0)= mrYes then
  begin
    lCursor := HourglassCursor;
    try
      SetStatus(ResStr_ClearOldIndex);
      ClearSystemTaxonGroupIndex(dmGeneralData.qryAllPurpose);
      PopulateTaxonGroupIndex(dmGeneralData.qryAllPurpose,
                               SetStatus, SetProgress);
      ShowInformation(ResStr_IndexRebuild);
    finally
      DefaultCursor(lCursor);
      TaskFinished;
    end;
  end;
end;  // mnuToolsDatabaseRebuildGroupIndexClick

//==============================================================================
procedure TfrmMain.mnuToolsDatabaseRebuildNameIndexClick(Sender: TObject);
var lCursor: TCursor;
begin
  if MessageDlg(ResStr_NameRebuildingTime, mtWarning, [mbYes,mbNo], 0) = idYes then
  begin
    lCursor := HourglassCursor;
    try
      RebuildIndexTaxonName(dmGeneralData.qryAllPurpose, SetStatus, SetProgress);
      ShowInformation(ResStr_NameIndexRebuildComplete);
      TaskFinished;
    finally
      DefaultCursor(lCursor);
    end;
  end;
end;  // mnuToolsDatabaseRebuildNameIndexClick


{ Callback used to receive progress info when updating index tables }
procedure TfrmMain.SetProgress(iProgress: Integer);
begin
  if ProgressBar.TaskPosition <> iProgress then begin
    ProgressBar.TaskPosition := iProgress;
    ProgressBar.Refresh;
  end;
end;

{ THIS OVERLOAD IS DEPRECATED }
procedure TfrmMain.SetProgress(const iProgress: Integer; iPrcMsgs: Boolean);
begin
  SetProgress(iProgress);
end;


//==============================================================================
procedure TfrmMain.mnuToolsDatabaseRebuildSynonymIndexClick(Sender: TObject);
var lCursor: TCursor;
begin
  if MessageDlg(ResStr_SynonymRebuildingTime, mtWarning, [mbYes,mbNo], 0) = idYes then
  begin
    lCursor := HourglassCursor;
    try
      RebuildIndexTaxonSynonym(dmGeneralData.qryAllPurpose, SetStatus, SetProgress);
      ShowInformation(ResStr_SynonymIndexRebuildComplete);
      TaskFinished;
    finally
      DefaultCursor(lCursor);
    end;
  end;
end;  // mnuToolsDatabaseRebuildSynonymIndexClick

//==============================================================================
{ Ensure MDI toolbar is locked to right of form by setting the panel size }
procedure TfrmMain.FormResize(Sender: TObject);
begin
  pnlMenuContainer.Width := Width-17;
  // Also set width of menu bar band, so nothing else can go on the same line. 
  CoolbarMain.Bands.FindBand(pnlMenuContainer).MinWidth := pnlMenuContainer.Width;
end;

//==============================================================================
{ Ensure that Window numbering is correct on the Window menu.  It must always
    sequence from 1, so this corrects for windows that are subsequently
    destroyed}
procedure TfrmMain.mnuWindowClick(Sender: TObject);
var
  i : integer;
begin
  // iterate only through the Child window items, copy all text after first space
  // in caption.
  for i := 2 to mnuWindow.Count-1 do
    mnuWindow.Items[i].Caption := '&' + IntToStr(i-1) + StringReplace(StringReplace(Copy(mnuWindow.Items[i].Caption, Pos(' ', mnuWindow.Items[i].Caption), 255),'&&', '&',[]), '&', '&&', []);
end;

//==============================================================================
{ Procedure to turn on or off the 3 MDI buttons in the corner }
procedure TfrmMain.SetMDIButtonVisible(iShow: Boolean);
begin
  tbMDIButtons.Visible := iShow;
  FormResize(nil); // ensure buttons go to correct place
end;

//==============================================================================
{ Implement MDI Minimise funcionality }
procedure TfrmMain.tbtnMDIMinimiseClick(Sender: TObject);
begin
  if ActiveMDIChild <> nil then
    ActiveMDIChild.WindowState := wsMinimized;
  SetMDIButtonVisible(False);
end;

//==============================================================================
{ Implement MDI Normalise funcionality }
procedure TfrmMain.tbtnMDINormaliseClick(Sender: TObject);
begin
  if ActiveMDIChild <> nil then
    ActiveMDIChild.WindowState := wsNormal;
  SetMDIButtonVisible(False);
end;

//==============================================================================
{ Implement MDI Close funcionality }
procedure TfrmMain.tbtnMDICloseClick(Sender: TObject);
begin
  if Assigned(ActiveMDIChild) then
    ActiveMDIChild.Close;
  // allow form to go away before updating buttons
  PostMessage(Handle, WM_CHECK_MDI_BUTTONS, 0, 0);
end;

//==============================================================================
{ Update the MDI buttons - hide them if no longer required }
procedure TfrmMain.WMCheckMDIButtons;
begin
  { Hide MDI buttons if appropriate }
  if frmMain.ActiveMDIChild = nil then
    SetMDIButtonVisible(False)
  else if frmMain.ActiveMDIChild.WindowState <> wsMaximized then
    SetMDIButtonVisible(False);
end;

//==============================================================================
{ Because we no longer have a main menu, we must manually instigate shortcut
     keys by calling the menu ourselves }
procedure TfrmMain.FormShortCut(var Msg: TWMKey; var Handled: Boolean);
begin
  Handled := mnuMain.IsShortCut(Msg);
  { Check merged menu if required }
  if (not Handled) and Assigned(ActiveMDIChild) then
    if ActiveMDIChild is TBaseChild then
      Handled := TBaseChild(ActiveMDIChild).mnuChildMerge.IsShortcut(Msg);
end;

//==============================================================================
{ To ensure that Coolbands don't overlap (which looks messy), set the widths
    to the widths of the toolbar }
procedure TfrmMain.SetBandWidths;
begin
  CoolbarMain.Bands.FindBand(tbMainToolbar).MinWidth := tbMainToolbar.Width;
  if tbContext.Visible then
    CoolbarMain.Bands.FindBand(tbContext).MinWidth := tbContext.Width;
end;

//==============================================================================
{ Set the Coolband minimum widths, preventing the bands from overlapping }
procedure TfrmMain.ToolbarResize(Sender: TObject);
begin
  if Sender is TToolbar then
    CoolbarMain.Bands.FindBand(TToolbar(Sender)).MinWidth := TToolbar(Sender).Width;
end;

//==============================================================================
{ Display the about box }
procedure TfrmMain.mnuHelpAboutClick(Sender: TObject);
begin
  with TdlgAbout.Create(nil) do
    try
      ShowModal;
    finally
      Free;
    end; // try
end;

//==============================================================================
procedure TfrmMain.mnuFileOpenClick(Sender: TObject);
begin
  if ActiveMDIChild is TfrmReportDesigner then
    TfrmReportDesigner(ActiveMDIChild).OpenReport
  else if ActiveMDIChild is TfrmSnapshot then
    TfrmSnapshot(ActiveMDIChild).OpenSnapshot;
end;

//==============================================================================
//This allows the user to change the location of the backup.
procedure TfrmMain.mnuToolsChangeBackupLocationClick(Sender: TObject);
var lcnn : TADOConnection;
  lRec : _Recordset;
  lCursor : TCursor;
  lstOldPath : String;

  function DatabaseIsLocal : boolean;
  var
    lstComputerName : string;
    liPosSlash : Integer;
  begin
    if CompareText(AppSettings.ServerName, '(local)') = 0 then
      Result := True
    else
    begin
      lstComputerName := AppSettings.ComputerID;
      //Check whether the servername contains an instance
      liPosSlash := Pos('\',AppSettings.ServerName);
      if liPosSlash <> 0 then
        if CompareText(lstComputerName, LeftStr(AppSettings.ServerName, liPosSlash -1)) = 0 then
          Result := True
        else
          Result := False
      else
        if CompareText(lstComputerName, AppSettings.ServerName) = 0 then
          Result := True
        else
          Result := False;
    end;
  end;

begin
  lCnn := TAdoConnection.Create(nil);
  lCnn.ConnectionString := dmDatabase.GetNewConnectionString;
  lCnn.LoginPrompt := False;
  lCnn.Open();
  lCursor := HourglassCursor;
  try

    with TdlgServerFolders.Create(Self) do
      try
        lCnn.Execute('Use Master');
        //Check that the sps can be executed by this user
        lRec := lCnn.Execute('Select ' +
            'Permissions(Object_ID(''sp_dropdevice'')) & ' +
            'Permissions(Object_ID(''sp_addumpdevice'')) & 32');
        if Integer(lRec.Fields[0].Value) = 0 then
          CanChange := False
        else
          //and that the user has permission to change the devices
          lRec := lCnn.Execute('select is_srvrolemember(''diskadmin'')');
          if lRec.Fields[0].Value = 0 then
            CanChange := False;
        //go back to the original database.
        lRec := lCnn.Execute('Select phyName from master.dbo.sysdevices ' +
            'where name = ''NBNData_backup''');
        if not (lRec.Bof and lRec.Eof) then
        begin
          lstOldPath := lRec.Fields[0].Value;
          Path := lstOldPath;
        end
        else
          raise TExceptionPath.CreateNonCritical(ResStr_NoBackupDevice);
        if ShowModal = mrOK then
        begin
          with qryMoveBackup do
          begin
            Connection := lCnn;
            Parameters.ParamByName('Name').Value:= 'NBNData_backup';
            Parameters.ParamByName('NewLocation').Value := IncludeTrailingPathDelimiter(Path) + 'NBNData.bak';

            //Need permission on xp_cmdshell to move the backup file
            lRec := lCnn.Execute('Select Permissions(Object_ID(''xp_cmdShell'')) & 32');
            if Integer(lRec.Fields[0].Value) = 0 then
            begin
              //See if the database is local: if so then can move file in Delphi.Create
              if not DatabaseIsLocal then
              begin
                if MessageDlg(ResStr_CannotMoveOldBackup,
                      mtWarning, [mbOK, mbCancel],0) = mrOK then
                begin
                  Parameters.ParamByName('Move').Value := False;
                  ExecSQL;
                end;
              end
              else
              begin
                Parameters.ParamByName('Move').Value := False;
                ExecSQL;
                if FileExists(lstOldPath) then
                begin
                  CopyFile (PChar(lstOldPath), PChar(IncludeTrailingPathDelimiter(Path) + 'NBNData.bak'), False);
                  DeleteFile(lstOldPath);
                end;
              end;
            end
            else
            begin
              Parameters.ParamByName('Move').Value := True;
              ExecSQL;
            end;
          end;
        end;
      finally
        Free;
      end;
  finally
    lCnn.Close;
    lCnn.Free;
    DefaultCursor(lCursor);
  end;
end;


{-------------------------------------------------------------------------------
  Description : When the Quick Report submenu is dropped down, populate the
              list of submenu items dynamically.  Use the list of available
              reports, plus check that the current screen supports
              the provision of SQL to report on.
  Created : 11/03/2003 }
procedure TfrmMain.mnuReportsQuickReportClick(Sender: TObject);
begin
  PopulateQuickReportSubMenu(mnuReportsQuickReport);
end; 

{-------------------------------------------------------------------------------
}
procedure TfrmMain.mnuToolsBatchUpdatesClick(Sender: TObject);
begin
  PopulateBatchUpdateSubMenu(mnuToolsBatchUpdates);
end;

{-------------------------------------------------------------------------------
  Description : Populates the Quick Report sub menu (either the main one or a
       popup) with the list of available reports
  Created : 11/03/2003 }
procedure TfrmMain.PopulateQuickReportSubMenu(const ASubMenu : TMenuItem;
          const ADataType: TKeyType=ktDefault; const AKey: string='');
begin   
  PopulateReportSubMenu(ASubMenu, dmFormActions.actOpenReport.Enabled,
      AppSettings.CRReportIndex, [ADataType], AKey, True);
end;

{-------------------------------------------------------------------------------
  Description : Populates the Batch Update sub menu (either the main one or a
       popup) with the list of available batch updates
}
procedure TfrmMain.PopulateBatchUpdateSubMenu(const ASubMenu : TMenuItem;
          const ADataType: TKeyType=ktDefault; const AKey: string='');
begin
  mnuEmpty.Visible := false;
  PopulateReportSubMenu(ASubMenu, dmFormActions.actRunBatchUpdates.Enabled,
      AppSettings.BatchUpdateIndex, [ADataType], AKey);
  // display an <No batch updates available> menu item if there are none but the mnuEmpty item
  if ASubMenu.Count<=1 then
    mnuEmpty.Visible := true;
end;

{-------------------------------------------------------------------------------
  Description : Populates the Batch Update sub menu (either the main one or a
       popup) with the list of available batch updates for multiple key types
}
procedure TfrmMain.PopulateBatchUpdateSubMenu(const ASubMenu: TMenuItem;
  const ADataTypes: TKeyTypes; const AKey: string);
begin
  mnuEmpty.Visible := false;
  PopulateReportSubMenu(ASubMenu, dmFormActions.actRunBatchUpdates.Enabled,
      AppSettings.BatchUpdateIndex, ADataTypes, AKey);
  // display an <No batch updates available> menu item if there are none but the mnuEmpty item
  if ASubMenu.Count<=1 then
    mnuEmpty.Visible := true;
end;

{-------------------------------------------------------------------------------
  Description : Populates the Quick Report sub menu (either the main one or a
       popup) with the list of available batch updates for multiple key types
}
procedure TfrmMain.PopulateQuickReportSubMenu(const ASubMenu: TMenuItem;
  const ADataTypes: TKeyTypes; const AKey: string);
begin
  PopulateReportSubMenu(ASubMenu, dmFormActions.actOpenReport.Enabled,
      AppSettings.CRReportIndex, ADataTypes, AKey, True);
end;

{-------------------------------------------------------------------------------
  Description : Populates a sub menu (either the main one or a
       popup) with the list of available reports
}
procedure TfrmMain.PopulateReportSubMenu(const ASubMenu : TMenuItem; AMenuEnabled: Boolean;
    AReportIndex: TBaseReportIndex; const ADataTypes: TKeyTypes = [ktDefault];
    const AKey: string=''; AQuickReports: Boolean = False);
var
  i : integer;
  lProviderIntf : IProvidesOccurrencesSQL;
  lDataType, EachDataType: TKeyType;
  lKey: string;
  lNewMenuItem: TMenuItem;
begin
  // clear existing items - always do this so the list is guaranteed
  // up to date in a multi-user environment.
  for i := ASubMenu.Count-1 downto 0 do
    if (not Assigned(ASubMenu.Items[i].Action)) and ASubMenu.Items[i].Visible then
      ASubMenu.Items[i].Free;
  if AMenuEnabled then begin
    // check if we need to add in the batch updates again
    if Assigned(ActiveMDIChild) then begin

      if AQuickReports then begin
        // check for a provider interface that can supply occurrences SQL
        if ActiveMDIChild is TfrmMDIContainer then
          Supports(TfrmMDIContainer(ActiveMDIChild).OleObject, IProvidesOccurrencesSQL, lProviderIntf)
        else
          Supports(ActiveMDIChild, IProvidesOccurrencesSQL, lProviderIntf);
        if Assigned(lProviderIntf) then
          if lProviderIntf.CanProvideSQL then
            AddQuickReportItems(ASubMenu);
      end;

      for EachDataType := Low(TKeyType) to High(TKeyType) do
        if EachDataType in ADataTypes then begin
          lDataType := EachDataType;
          lKey := AKey;
          if (lDataType = ktDefault) then
            if ActiveMDIChild is TBaseChild then begin
              lDataType := TBaseChild(ActiveMDIChild).CustomReportKeyType;
              lKey := TBaseChild(ActiveMDIChild).ItemKey;
            end;
          // Default reports should only appear on Run Report dialog
          if lDataType <> ktDefault then
            AddCustomReportMenuItems(ASubMenu, lDataType, lKey, AReportIndex, AQuickReports);
        end;    // if lDataType in ADataTypes
    end;
  end
  else begin
    // Access to reports not available - presumably one already visible.
    lNewMenuItem := TMenuItem.Create(self);
    lNewMenuItem.Caption := ResStr_ReportWindowOpen;
    lNewMenuItem.Enabled := False;
    ASubMenu.Add(lNewMenuItem);
  end;
  frmMain.XPMenu.ActivateMenuItem(ASubMenu, True);
end;

{-------------------------------------------------------------------------------
  Description : Add the current list of available reports to a quick report
              submenu.
  Created : 11/03/2003 }
procedure TfrmMain.AddQuickReportItems(const ASubMenu : TMenuItem);
var
  lSr : TSearchRec;
begin
  if FindFirst(AppSettings.ReportPath + '*.wzd', 0, lSr)=0 then begin
    repeat
      AddWizardReportMenuItem(lSr.Name, ASubMenu);
    until FindNext(lSr)<>0;
  end;
end;

{-------------------------------------------------------------------------------
  Add the list of custom reports to a quick report popup menu.  Only include
      those which are appropriate to the data type.
}
procedure TfrmMain.AddCustomReportMenuItems(const ASubMenu : TMenuItem;
          const ADataType: TKeyType; const AKey: string; AReportIndex: TBaseReportIndex;
          AQuickReport: Boolean);
var
  i: integer;
  lNewMenuItem : TCustomReportMenuItem;

    // Find a menu item of a given name inside the parent menu, create one if required
    function FindMenuItem(AParentMenu: TMenuItem; const AMenuName: string): TMenuItem;
    var
      i: integer;
      lPathMenuItem: TMenuItem;
    begin
      for i := 0 to AParentMenu.Count-1 do
        if CompareText(AParentMenu.Items[i].Caption, AMenuName)=0 then begin
          Result := AParentMenu.Items[i];
          Exit;
        end;
      // if we get this far, no existing item exists
      lPathMenuItem := TMenuItem.Create(ASubMenu.Owner);
      lPathMenuItem.ImageIndex := 59;
      lPathMenuItem.Caption := AMenuName;
      AParentMenu.Add(lPathMenuItem);
      Result := lPathMenuItem;
    end;

    // recursive proc that builds a menu path to the report
    procedure AddToPath(AParentMenu, AMenuItem: TMenuItem; APath: string);
    var
      lMenuName, lRemainingPath: string;
    begin
      // if no path specified then add menu item
      if APath='' then
        AParentMenu.Add(AMenuItem)
      else begin
        // path specified so need to find or create top menu item and recurse
        lMenuName := LeftStr(APath, Pos('\', APath)-1);
        if lMenuName = '' then
          lMenuName := APath;
        lRemainingPath := Copy(APath, Length(lMenuName)+2, Length(APath));
        AddToPath(FindMenuItem(AParentMenu, lMenuName), AMenuItem, lRemainingPath);
      end;
    end;

begin
  with AReportIndex.FileTypes do
    for i := 0 to Count - 1 do
      if Copy(Strings[i], Pos(':', Strings[i])+1, Length(Strings[i])) = IntToStr(Ord(ADataType)) then
      begin
        lNewMenuItem := TCustomReportMenuItem.Create(ASubMenu.Owner);
        lNewMenuItem.DataType := ADataType;
        lNewMenuItem.Key := AKey;
        lNewMenuItem.FileName := LeftStr(Strings[i], Pos(':', Strings[i])-1);
        lNewMenuItem.Caption := DuplicateCharacters(ExtractWithoutExt(
            lNewMenuItem.FileName), '&');
        if AQuickReport then
          lNewMenuItem.OnClick := CustomReportClick
        else
          lNewMenuItem.OnClick := BatchUpdateClick;
        lNewMenuItem.ImageIndex := 66;
        AddToPath(ASubMenu, lNewMenuItem, AReportIndex.Paths.Values[lNewMenuItem.FileName]);
      end;
end;

{-------------------------------------------------------------------------------
  Description : A menu handler that works for all the Quick Report sub menus
              except the standard ones.  Uses the menu item caption to find the
              report to run.
  Created : 11/03/03 }
procedure TfrmMain.QuickReportClick(Sender: TObject);
var
  lProviderIntf : IProvidesOccurrencesSQL;
  lfrmFilterResult : TfrmFilterResult;
begin
  lfrmFilterResult := nil;
  if Assigned(ActiveMDIChild) then
    lProviderIntf := nil; // default until we can locate the interface
    if ActiveMDIChild is TfrmMDIContainer then
      Supports(TfrmMDIContainer(ActiveMDIChild).OleObject, IProvidesOccurrencesSQL, lProviderIntf)
    else
      Supports(ActiveMDIChild, IProvidesOccurrencesSQL, lProviderIntf);
    if (Sender is TMenuItem) then begin
      lfrmFilterResult := TfrmFilterResult(dmFormActions.GetFormInstance(TfrmFilterResult));
      if Assigned(lfrmFilterResult) and Assigned(lProviderIntf) then
        lfrmFilterResult.RunReportWithFilter(TFileMenuItem(Sender).OriginalFileName, lProviderIntf)
      else if Assigned(lfrmFilterResult) and not Assigned(lProviderIntf) then
        lfrmFilterResult.RunReport(TFileMenuItem(Sender).OriginalFileName)
      else if not Assigned(lfrmFilterResult) and Assigned(lProviderIntf) then
        lfrmFilterResult := TfrmFilterResult.CreateAndRunWithFilter(frmMain,
            TFileMenuItem(Sender).OriginalFileName, lProviderIntf)
      else if not Assigned(lfrmFilterResult) and not Assigned(lProviderIntf) then
        lfrmFilterResult := TfrmFilterResult.CreateAndRun(frmMain,
            TFileMenuItem(Sender).OriginalFileName)
    end;
  if Assigned(dmFormActions.GetFormInstance(TfrmReportPreview)) then
    dmFormActions.GetFormInstance(TfrmReportPreview).BringToFront
  else
    if Assigned(lfrmFilterResult) then lfrmFilterResult.BringToFront;
end;

{-------------------------------------------------------------------------------
}
procedure TfrmMain.CustomReportClick(Sender: TObject);
var
  lfrmFilterResult : TfrmFilterResult;
  lCursor: TCursor;
begin
  lCursor := HourglassCursor;
  SetStatus(ResStr_PreparingReport + '...');
  try
    lfrmFilterResult := nil;
    if Sender is TCustomReportMenuItem then begin
      lfrmFilterResult := TfrmFilterResult(dmFormActions.GetFormInstance(TfrmFilterResult));
      if Assigned(lfrmFilterResult) then
        lfrmFilterResult.RunReport(TCustomReportMenuItem(Sender).FileName,
            TCustomReportMenuItem(Sender).DataType, TCustomReportMenuItem(Sender).Key)
      else
        lfrmFilterResult := TfrmFilterResult.CreateAndRun(frmMain,
            TCustomReportMenuItem(Sender).FileName,
            TCustomReportMenuItem(Sender).DataType,
            TCustomReportMenuItem(Sender).Key);
    end;
    if Assigned(lfrmFilterResult) then lfrmFilterResult.BringToFront;
  finally
    SetStatus('');
    DefaultCursor(lCursor);
  end;
end;

{-------------------------------------------------------------------------------
}
procedure TfrmMain.BatchUpdateClick(Sender: TObject);
begin
  if not dmFormActions.HasUnsavedChanges then begin
    if Sender is TCustomReportMenuItem then
      dmFormActions.InitBatchUpdate(AppSettings.BatchUpdatePath + TCustomReportMenuItem(Sender).FileName,
          TCustomReportMenuItem(Sender).DataType, TCustomReportMenuItem(Sender).Key);
  end;
end;

{-------------------------------------------------------------------------------
}
procedure TfrmMain.UpdateMapWindowSelector;
begin
  AppSettings.UpdateMapMenu(Self, mnuMapWindow);
  AppSettings.UpdateMapMenu(Self, pmMapWindow.Items, True);
end;  // TfrmMain.UpdateMapBrowserMenus

{-------------------------------------------------------------------------------
}
procedure TfrmMain.RefreshXPMenu;
begin
  if Assigned(XPMenu) then begin
    XPMenu.InitComponent(Self);
    XPMenu.InitComponent(dmFormActions.pmRTF);
  end;
end;

{-------------------------------------------------------------------------------
}
procedure TfrmMain.FormShowHint(var HintStr: String; var CanShow: Boolean;
  var HintInfo: THintInfo);
begin
  if Assigned(ActiveMDIChild) then
    TBaseChild(ActiveMDIChild).ShowHint(HintStr, CanShow, HintInfo);
end;

{-------------------------------------------------------------------------------
{ TCustomReportMenuItem }
{-------------------------------------------------------------------------------

{-------------------------------------------------------------------------------
}
procedure TCustomReportMenuItem.SetKey(const Value: string);
begin
  FKey := Value;
end;

{-------------------------------------------------------------------------------
}
procedure TCustomReportMenuItem.SetDataType(const Value: TKeyType);
begin
  FDataType := Value;
end;

procedure TCustomReportMenuItem.SetFileName(const Value: string);
begin
  FFileName := Value;
end;

{-------------------------------------------------------------------------------
  Overload of PopulateQuickReportSubMenu that makes caller's code simpler.
  Populates the sub menu with the quick reports and custom reports available
  for the node type.
}
procedure TfrmMain.PopulateQuickReportSubMenu(ANode: TFlyNode;
  const ASubMenu: TMenuItem);
begin
  if Assigned(ANode) then
    if Assigned(ANode.Data) then
      if TObject(ANode.Data) is TReportableNode then
        PopulateQuickReportSubMenu(ASubMenu,
            TReportableNode(ANode.Data).ReportKeyType,
            TReportableNode(ANode.Data).ItemKey);
end;

{-------------------------------------------------------------------------------
  Overload of PopulateBatchUpdateSubMenu that makes caller's code simpler.
  Populates the sub menu with the batchupdates available
  for the node type.
}
procedure TfrmMain.PopulateBatchUpdateSubMenu(ANode: TFlyNode;
  const ASubMenu: TMenuItem);
begin
  if Assigned(ANode) then
    if Assigned(ANode.Data) then
      if TObject(ANode.Data) is TReportableNode then
        PopulateBatchUpdateSubMenu(ASubMenu,
            TReportableNode(ANode.Data).ReportKeyType,
            TReportableNode(ANode.Data).ItemKey);
end;

{-------------------------------------------------------------------------------
  Insert the favourite report menu options into the main menu
}
procedure TfrmMain.CreateFavouriteReportMenuOptions;
var
  lReports: TStringList;
  i: integer;
  lNewMenuItem: TCustomReportMenuItem;
  lReportFile, lReportTitle: string;
begin
  // clear any existing report entries
  for i := mnuReports.Count - 1 downto 0 do
    if (mnuReports.Items[i] is TCustomReportMenuItem) or
       (mnuReports.Items[i] is TFileMenuItem) then
      mnuReports.Items[i].Free;

  lReports := TStringList.Create;
  try
    with TRegistry.Create do
      try
        RootKey := HKEY_CURRENT_USER;
        if OpenKeyReadOnly(REG_KEY_FAVOURITE_REPORTS) then
          GetValueNames(lReports);
      finally
        CloseKey;
        Free;
      end;

    // load up the XML reports
    with AppSettings.CRReportIndex.Titles do
      for i := 0 to Count - 1 do begin
        lReportFile := LeftStr(Strings[i], Pos(':', Strings[i]) - 1);
        // is it a favourite?
        if (lReports.IndexOf(lReportFile) > -1) and
           (FileExists(AppSettings.ReportPath + lReportFile)) then begin
          lReportTitle := Copy(Strings[i], Pos(':', Strings[i]) + 1, 255);
          lNewMenuItem := TCustomReportMenuItem.Create(self);
          lNewMenuItem.DataType := ktDefault;
          lNewMenuItem.Key := '';
          lNewMenuItem.FileName := lReportFile;
          lNewMenuItem.Caption := DuplicateCharacters(lReportTitle, '&');
          lNewMenuItem.OnClick := CustomReportClick;
          lNewMenuItem.ImageIndex := 66;
          mnuReports.Add(lNewMenuItem);
        end;
      end;

    // now load up the other wizard reports
    for i := 0 to lReports.Count - 1 do begin
      if SameText(RightStr(lReports[i], 4), '.wzd') then
        AddWizardReportMenuItem(lReports[i], mnuReports);
    end;
  finally
    lReports.Free;
  end;
end;

{ TFileMenuItem }

{-------------------------------------------------------------------------------
  Accessor
}
procedure TFileMenuItem.SetOriginalFileName(const Value: string);
begin
  FOriginalFileName := Value;
end;

{-------------------------------------------------------------------------------
  Adds a single wziard report menu item to the parent supplied
}
procedure TfrmMain.AddWizardReportMenuItem(const AReportFile: string; ASubMenu: TMenuItem);
var
  lNewMenuItem: TFileMenuItem;
  lFile: TextFile;
  lFileHeaderLine: string;
begin
  // Check file is new format
  AssignFile(lFile, AppSettings.ReportPath + AReportFile);
  Reset(lFile);
  ReadLn(lFile, lFileHeaderLine);
  CloseFile(lFile);
  if (CompareText(lFileHeaderLine, '<SQL>') <> 0) and
     (CompareText(lFileHeaderLine, '<Template>') <> 0) then
  begin
    lNewMenuItem := TFileMenuItem.Create(ASubMenu.Owner);
    lNewMenuItem.Caption := DuplicateCharacters(dmGeneralData.GetReportCaption(
        AppSettings.ReportPath + AReportFile), '&');
    lNewMenuItem.OnClick := QuickReportClick;
    lNewMenuItem.ImageIndex := 65;
    lNewMenuItem.OriginalFileName := AReportFile;
    ASubMenu.Add(lNewMenuItem);
  end;
end;

{-------------------------------------------------------------------------------
  Reapplies the various options after clicking Ok on the Options dialog
}
procedure TfrmMain.ReapplyOptions;
begin
  // Main Toolbar
  tbMainToolbar.Visible := AppSettings.ShowMainToolbar;
  CoolBarMain.ShowHint  := AppSettings.ShowToolTips;
  ShowHint := AppSettings.ShowToolTips;
  BroadcastMessage(WM_REFRESH_COLOURS);
  UpdateRecordingCards;
end;

{-------------------------------------------------------------------------------
  Rebuilds the index_taxon_designation table.
}
procedure TfrmMain.mnuToolsDatabaseRebuildDesignationIndexClick(
  Sender: TObject);
var lCursor: TCursor;
begin
  if MessageDlg(ResStr_DesignationRebuildingTime, mtWarning, [mbYes, mbNo],0)= mrYes then
  begin
    lCursor := HourglassCursor;
    try
      SetStatus(ResStr_DesignationRebuildingStatus);
      SetProgress(50);
      dmDatabase.RunStoredProc('usp_Index_Taxon_Designation_Rebuild', []);
      SetProgress(100);
      ShowInformation(ResStr_DesignationIndexRebuildComplete);
      SetProgress(0);
    finally
      DefaultCursor(lCursor);
      TaskFinished;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Click handler for the online help menu item. Shells to a web browser.
}
procedure TfrmMain.mnuOnLineHelpClick(Sender: TObject);
begin
  shellfile(GetOnLineHelpUrl);
end;

{-------------------------------------------------------------------------------
  Fetches the URL for online help from the settings table. Throws a non-critical
  exception if not found.
}
function TfrmMain.GetOnLineHelpUrl: string;
var  rs : _Recordset;
begin
  Result := '';
  rs := dmDatabase.ExecuteSQL('SELECT Data FROM [Setting] WHERE Name=''HelpUrl''', true);
  if rs.recordcount=0 then
    raise TExceptionPath.CreateNonCritical(ResStr_HelpUrlNotFound);
  Result := rs.Fields[0].Value
end;

procedure TfrmMain.MnuToolsUpdateDictionary1Click(Sender: TObject);
var rs : _Recordset;
begin
  rs := dmDatabase.ExecuteSQL('SELECT Data FROM [Setting] WHERE Name=''Dict Seq''', true);
  if (not rs.eof) and
     (MessageDlg(Format(ResStr_DictionaryRebuilding,[rs.Fields[0].Value]),
        mtWarning, [mbOk, mbCancel],0)= mrOk) then
  begin
    if AppSettings.UserAccessLevel >= ualFullUser then begin
      with TdlgDictionaryUpgrade.Create(nil) do
      try
        ShowModal;
      finally
        Free;
      end;
    end
    else
      ShowInformation(ResStr_InsufficientPermissons);
    end;
  end;

end.
