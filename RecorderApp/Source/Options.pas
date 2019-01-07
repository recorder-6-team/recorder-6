//==============================================================================
//  Unit:        Options
//
//  Implements:  TdlgOptions
//
//  Description: Dialog containing several settings to customise the application
//
//  Author:      John van Breda
//  Created:     8 Apr 1999
//
//  Last Revision Details:
//    $Revision: 104 $
//    $Date: 31.05.10 16:40 $
//    $Author: Simonlewis $
//
//==============================================================================

unit Options;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ActnList, Buttons, ComCtrls, ExtCtrls, ColorBtn, JPeg, ExtDlgs,
  CheckLst, ExceptionForm, FolderBrowser, FileCtrl, BaseFormUnit, OnlineHelp,
  Constants, XPToolButton, ImageListButton, Recorder2000_TLB, StrUtils, Math,
  ApplicationSettings,ADODB, DatabaseAccessADO;

resourcestring
  SOptions_SelectFolder = 'Select the source folder for %s';
  ResStr_InvalidPath    = 'Invalid path or filename. The file could not be found.';
  ResStr_SelectSRSystem = 'You must select a Spatial Reference system.';
  
  ResStr_InvalidFolder =  'Invalid folder name. '#13#13+
                          'The folder does not exist.  Enter a valid file location or '+
                          'leave blank to use the default folder.';

  ResStr_InvalidCutoffYear = 'The cut-off year has to be a valid value between 0 and 99.' ;
  ResStr_InvalidRapidEntryDelimiter =
      'The rapid entry delimiter must not be a letter, number, space or one of the symbols: <, >, /, - or ~.';

type
  EOptionsError = class(TExceptionPath);
  EConfigureToolbar = class(EOptionsError);

  { Simple class so we can store the 4 char String on a list }
  TSpatialSystemName = class
  public
    Name : String[4];
  end;

  TdlgOptions = class(TForm)
    btnDefault: TButton;
    dlgOpen: TOpenPictureDialog;
    dlgFolder: TFolderBrowser;
    pcOptionsPages: TPageControl;
    tsGeneral: TTabSheet;
    tsMainToolbar: TTabSheet;
    lblToolbarButtons: TLabel;
    lblAvailableActions: TLabel;
    lbOnToolbar: TListBox;
    lbAvailable: TListBox;
    tsAppearance: TTabSheet;
    lblInformationSource: TLabel;
    lblInformationDest: TLabel;
    lblMandatoryFields: TLabel;
    bvlPreview: TBevel;
    ShapeSource: TShape;
    ShapeDest: TShape;
    ShapeSourceDest: TShape;
    lblBackgroundPreview: TLabel;
    colMandatoryField: TColorButton;
    colDragSource: TColorButton;
    colDragDest: TColorButton;
    btnUndoAppearance: TButton;
    gbBackground: TGroupBox;
    rbBackPlain: TRadioButton;
    rbBackBitmap: TRadioButton;
    eBackBitmap: TEdit;
    btnBitmapBrowse: TButton;
    cbNoDragDropFrame: TCheckBox;
    cbShowToolTips: TCheckBox;
    pnlBackBitmap: TPanel;
    imgBackBitmap: TImage;
    eMandatory: TEdit;
    eDragSource: TEdit;
    eDragDest: TEdit;
    eDragSourceDest: TEdit;
    tsSpatialRef: TTabSheet;
    lblSystemSelect: TLabel;
    cblbSpatialRefs: TCheckListBox;
    tsFileLocations: TTabSheet;
    lblRucksacks: TLabel;
    lblTemplates: TLabel;
    lblQueries: TLabel;
    lblCards: TLabel;
    eRucksackFiles: TEdit;
    eTemplates: TEdit;
    btnRucksackBrowse: TButton;
    btnTemplateBrowse: TButton;
    eReports: TEdit;
    btnReportBrowse: TButton;
    btnRecordingCardBrowse: TButton;
    eRecordingCards: TEdit;
    lblImages: TLabel;
    eImageFiles: TEdit;
    btnImageFilesBrowse: TButton;
    lblFilters: TLabel;
    ePolygonFilters: TEdit;
    btnPolygonFiltersBrowse: TButton;
    cbShowMainToolbar: TCheckBox;
    cbShowActiveWindowToolbar: TCheckBox;
    bbAdd: TImageListButton;
    bbRemove: TImageListButton;
    bbClearAll: TImageListButton;
    bbMoveUp: TImageListButton;
    bbMoveDown: TImageListButton;
    bbOK: TImageListButton;
    bbCancel: TImageListButton;
    lblSnapshotFiles: TLabel;
    eSnapshots: TEdit;
    btnSnapshotBrowse: TButton;
    grbMenusWindows: TGroupBox;
    grbTaxonOptions: TGroupBox;
    lblTaxonSearches: TLabel;
    cbShowLastSessionWindows: TCheckBox;
    cbShowWelcomeAtStart: TCheckBox;
    cbShowMenuIcons: TCheckBox;
    cbGraduatedMenus: TCheckBox;
    cmbTaxonRestriction: TComboBox;
    cbFullTranslation: TCheckBox;
    grbConfidentialData: TGroupBox;
    cbExportConfidentialOcc: TCheckBox;
    lblMinimumAccess: TLabel;
    cmbConfAccessLevel: TComboBox;
    chkPartialTaxonSearch: TCheckBox;
    lblBatches: TLabel;
    eBatchUpdates: TEdit;
    btnBatchUpdatesBrowse: TButton;
    bvlSpatialRefs: TBevel;
    bvlToolbars: TBevel;
    bvlAppearance: TBevel;
    bvlFiles: TBevel;
    cbGridRefsAsSquares: TCheckBox;
    chkAutoCompleteSearch: TCheckBox;
    gbLocationNodes: TGroupBox;
    chkIncludeLocationSpatialRef: TCheckBox;
    chkIncludeLocationFileCode: TCheckBox;
    lblExternalFilePath: TLabel;
    eExternalFiles: TEdit;
    btnExternalFilePath: TButton;
    lblExtraLocationSearchColumns: TLabel;
    lblExtraLocationSearchColumnList: TLabel;
    btnExtraLocationSearchColumns: TButton;
    grbTaxonNameOptions: TGroupBox;
    cbShowCommonNames: TCheckBox;
    cbShowAuthors: TCheckBox;
    cbShowNamesAsEntered: TCheckBox;
    lblExportTemplates: TLabel;
    eExportTemplates: TEdit;
    btnExportTemplateBrowse: TButton;
    cbUseOriginalicons: TCheckBox;
    tsSettingTable: TTabSheet;
    edMaster: TEdit;
    lblSetting1: TLabel;
    btnMaster: TButton;
    lblMapMaster: TLabel;
    lblThisWorkStation: TLabel;
    lblSetting2: TLabel;
    lblSetting3: TLabel;
    lblSetting4: TLabel;
    lblSetting5: TLabel;
    lblSetting6: TLabel;
    lblSetting7: TLabel;
    lblSetting8: TLabel;
    lblSetting10: TLabel;
    lblSetting11: TLabel;
    edCompetency: TEdit;
    edPreLocs: TEdit;
    edPrefnames: TEdit;
    edSortMethod: TEdit;
    edTaxDesList: TEdit;
    edTempLicence: TEdit;
    edTempNames: TEdit;
    edGatewayURL: TEdit;
    edHelpUrl: TEdit;
    lblWorksattion: TLabel;
    lblSettingWarning: TLabel;
    tsSundry: TTabSheet;
    grbOther: TGroupBox;
    lblCenturyCutOff: TLabel;
    lblCenturyCutOffInfo: TLabel;
    lblDelimiter: TLabel;
    cbAutoEmail: TCheckBox;
    eDateCutYear: TEdit;
    chkRememberFilters: TCheckBox;
    eRapidEntryDelimiter: TEdit;
    chkOrganiseSurveysByTag: TCheckBox;
    chkUseOldImportWizard: TCheckBox;
    chkIgnoreRememberedMatches: TCheckBox;
    cbPlaceCardDocs: TCheckBox;
    lblSetting12: TLabel;
    edBlockSize: TEdit;
    chkUsePreferredTaxa: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure DrawListItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure bbAddClick(Sender: TObject);
    procedure bbRemoveClick(Sender: TObject);
    procedure bbClearAllClick(Sender: TObject);
    procedure bbMoveUpClick(Sender: TObject);
    procedure lbOnToolbarClick(Sender: TObject);
    procedure bbMoveDownClick(Sender: TObject);
    procedure btnUndoAppearanceClick(Sender: TObject);
    procedure bbSurveyNameClick(Sender: TObject);
    procedure bbTaxonGroupClick(Sender: TObject);
    procedure bbGeoAreaClick(Sender: TObject);
    procedure btnRucksackBrowseClick(Sender: TObject);
    procedure btnTemplateBrowseClick(Sender: TObject);
    procedure rbBackPlainClick(Sender: TObject);
    procedure rbBackBitmapClick(Sender: TObject);
    procedure btnBitmapBrowseClick(Sender: TObject);
    procedure colMandatoryFieldChange(Sender: TObject);
    procedure colDragSourceChange(Sender: TObject);
    procedure colDragDestChange(Sender: TObject);
    procedure btnReportBrowseClick(Sender: TObject);
    procedure lbOnToolbarDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure eBackBitmapKeyPress(Sender: TObject; var Key: Char);
    procedure btnDefaultClick(Sender: TObject);
    procedure cbShowActiveWindowToolbarClick(Sender: TObject);
    procedure bbOkClick(Sender: TObject);
    procedure cblbSpatialRefsClick(Sender: TObject);
    procedure btnRecordingCardBrowseClick(Sender: TObject);
    procedure pcOptionsPagesChange(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure cblbSpatialRefsMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure cblbSpatialRefsClickCheck(Sender: TObject);
    procedure bbCancelClick(Sender: TObject);
    procedure eDateCutYearKeyPress(Sender: TObject; var Key: Char);
    procedure eDateCutYearExit(Sender: TObject);
    procedure eBackBitmapExit(Sender: TObject);
    procedure btnPolygonFiltersBrowseClick(Sender: TObject);
    procedure btnImageFilesBrowseClick(Sender: TObject);
    procedure cmbTaxonRestrictionChange(Sender: TObject);
    procedure cbShowMenuIconsClick(Sender: TObject);
    procedure btnSnapshotBrowseClick(Sender: TObject);
    procedure eRapidEntryDelimiterKeyPress(Sender: TObject; var Key: Char);
    procedure btnBatchUpdatesBrowseClick(Sender: TObject);
    procedure btnExtraLocationSearchColumnsClick(Sender: TObject);
    procedure btnExternalFilePathClick(Sender: TObject);
    procedure btnExportTemplateBrowseClick(Sender: TObject);
    procedure btnMasterClick(Sender: TObject);
    procedure edCompetencyKeyPress(Sender: TObject; var Key: Char);
    procedure FormActivate(Sender: TObject);
    procedure edBlockSizeKeyPress(Sender: TObject; var Key: Char);

  private
    hlpOptions: TOnlineHelp;
    FSpatialRefSystem: String;
    FLeftMouseButton: Boolean;
    FCheckedIndex: Integer;
    FPageAddins: TInterfaceList;
    FExtraLocationSearchColumns: TLocationSearchColumns;
    procedure AddToToolbar;
    function ConfidentialLevelToIndex(level: TUserAccessLevel): Integer;
    function IndexToConfidentialLevel: TUserAccessLevel;
    procedure SetSpatialRefSystem;
    procedure ShowBackBitmap;
    procedure InitCOMSpatialSystems;
    function CheckFileLocation(AEdit: TEdit): boolean;
    function SelectFolder(const AFolder, AMsg: String): String;
    procedure PopulateSettingFields(const AField: String);
    procedure ResetDefaultPaths;
    procedure AddAddinPage(APage: IOptionsPage);
    procedure SetAddinPagesToDefault;
    procedure SaveAddinPages;
    procedure ValidateDelimiter;
    procedure SetExtraLocationSearchColumns(Selection: TLocationSearchColumns);
    property ExtraLocationSearchColumns: TLocationSearchColumns
        read FExtraLocationSearchColumns write SetExtraLocationSearchColumns;
    procedure SaveSettingTable;

  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    property NewSpatialRefSystem:String read FSpatialRefSystem;
  end;

//==============================================================================
implementation

{$R *.DFM}

uses
  FormActions, Maintbar, SpatialRefFuncs, JNCCDatasets, Rucksack, GeneralFunctions,
  GeneralData, ComAddinUnit, ComObj, OleTools, ActiveX, DefaultPaths,
  SelectLocationSearchColumns, Locations;

resourcestring
  ResStr_RuckSacks      = 'Rucksacks:';
  ResStr_Template       = 'Templates:';
  ResStr_ExportTemplate = 'Export Templates:';
  ResStr_Queries        = 'Queries:';
  ResStr_RecordingCards = 'Recording Cards:';
  ResStr_ImageFiles     = 'Image Files:';
  ResStr_PolygonFilters = 'Polygon Filters:';
  ResStr_Snapshots      = 'Snapshots:';
  ResStr_Separator      = 'Separator';
  ResStr_BatchUpdates   = 'Batch Updates:';
  ResStr_None           = '(none)';
  ResStr_ExternalFilePath = 'External Files:';
  Restr_Master_Workstation = 'The map on this workstation must be reset before it can be made the master';
  Restr_Admin_Permission  = 'Changes to the Setting table have not been made as they require Administrator permission';
  ResStr_Setting_Warning = 'WARNING - do NOT change any of these settings unless you fully understand the implications.';
const
  BAD_DELIMITERS = ['a'..'z', '0'..'9', 'A'..'Z', '<', '>', '~', '-', '/', ' '];

  IDX_READONLY = 0;
  IDX_RECORDER = 1;
  IDX_ADDONLY  = 2;
  IDX_FULLOWN  = 3;
  IDX_FULLALL  = 4;
  IDX_ADMIN    = 5;

//==============================================================================
constructor TdlgOptions.Create(AOwner: TComponent);
var
  lAddinIdx: Integer;
  lPageIdx: Integer;
  lOptionsPagesAddin: IOptionsPages;
begin
  inherited Create(AOwner);
  { Just in case the first tab has not been left visible }
  pcOptionsPages.ActivePage := tsGeneral;
  dmGeneralData.GetTaxonSearchOptions(cmbTaxonRestriction);
  cmbTaxonRestriction.ItemIndex :=
      cmbTaxonRestriction.Items.IndexOf(AppSettings.TaxonomicSearchRestriction);

  FPageAddins := TInterfaceList.Create;
  for lAddinIdx := 0 to AppSettings.ComAddins.OptionsPages.Count - 1 do
  begin
    lOptionsPagesAddin := CreateComObject(StringToGUID(
        AppSettings.ComAddins.OptionsPages[lAddinIdx])) as IOptionsPages;
    for lPageIdx := 0 to lOptionsPagesAddin.OptionsPageCount - 1 do
      AddAddinPage(lOptionsPagesAddin.OptionsPageItems[lPageIdx]);
  end; // for
  ExtraLocationSearchColumns := AppSettings.ExtraLocationSearchColumns;
end;  // Create

//==============================================================================
{ Destroy - cleans up spatial reference names }
destructor TdlgOptions.Destroy;
var
  i: Integer;
begin
  with cblbSpatialRefs.Items do
    for i := 0 to Count - 1 do
      if Objects[i] <> nil then TSpatialSystemName(Objects[i]).Free;
  FPageAddins.Free;
  inherited Destroy;
end;  // Destroy

//==============================================================================
procedure TdlgOptions.FormCreate(Sender: TObject);
var
  i:Integer;
begin
  // Main Toolbar
  { Populate available action list }
  lbAvailable.Items.Add('-- ' + ResStr_Separator + ' --');
  with dmFormActions do
    for i:= 0 to alForms.ActionCount - 1 do
    begin
      if alForms.Actions[i].Tag > 0 then
        lbAvailable.Items.AddObject(
            TrimText(TAction(alForms.Actions[i]).Caption),
            alForms.Actions[i]);
    end;

  { Populate current action list from toolbar }
  with frmMain.tbMainToolbar do
    for i := 0 to ButtonCount - 1 do
      if (Buttons[i].Style in [tbsButton,tbsDropDown]) and Assigned(Buttons[i].Action) then
        lbOnToolbar.Items.AddObject(
            dmFormActions.TrimText(TAction(Buttons[i].Action).Caption),
            Buttons[i].Action)
      else
      if Buttons[i].Style = tbsSeparator then
        lbOnToolbar.Items.Add('-- ' + ResStr_Separator + ' --');

  with AppSettings do
  begin
    // General Options
    cbShowLastSessionWindows.Checked   := ShowLastSessionWindows;
    cbShowMenuIcons.Checked            := ShowMenuIcons;
    cbGraduatedMenus.Checked           := GraduatedMenus;
    cbShowMainToolbar.Checked          := ShowMainToolbar;
    cbShowActiveWindowToolbar.Checked  := ShowContextToolbar;
    cbShowWelcomeAtStart.Checked       := ShowWelcomeAtStart;
    cbShowCommonNames.Checked          := DisplayTaxonCommonNames;
    cbShowNamesAsEntered.Checked       := DisplayTaxonEnteredNames;
    cbShowAuthors.Checked              := DisplayTaxonAuthors;
    cbFullTranslation.Checked          := UseRecommendedTaxaNames;
    cbAutoEmail.Checked                := AutoSchemeEmail;
    eDateCutYear.Text                  := RightStr('0' + IntToStr(DateCutYear), 2);
    chkRememberFilters.Checked         := RememberFilters;
    chkPartialTaxonSearch.Checked      := PartialTaxonSearch;
    chkAutoCompleteSearch.Checked      := AutoCompleteSearch;
    chkUsePreferredTaxa.Checked        := UsePreferredTaxa;
    chkIncludeLocationSpatialRef.Checked := IncludeLocationSpatialRef;
    chkIncludeLocationFileCode.Checked := IncludeLocationFileCode;
    eRapidEntryDelimiter.Text          := RapidEntryDelimiter;
    chkOrganiseSurveysByTag.Checked    := OrganiseSurveysByTag;
    chkUseOldImportWizard.Checked      := UseOldImportWizard;
    chkIgnoreRememberedMatches.Checked := IgnoreRememberedMatches;

    cmbConfAccessLevel.ItemIndex := ConfidentialLevelToIndex(AppSettings.ConfidentialAccessLevel);
    cmbConfAccessLevel.Enabled   := UserAccessLevel = ualAdmin;

    // Only for Admin.
    if UserAccessLevel = ualAdmin then
      cbExportConfidentialOcc.Checked := ExportConfidentialOccurrences
    else begin
      cbExportConfidentialOcc.Enabled := False;
      cbExportConfidentialOcc.Checked := False;
    end;

    // Appearance
    colMandatoryField.ActiveColor := MandatoryColour;
    colDragSource.ActiveColor     := DragSourceColour;
    colDragDest.ActiveColor       := DragDestColour;
    rbBackPlain.Checked           := PlainBackground;
    rbBackBitmap.Checked          := not rbBackPlain.Checked;
    if rbBackPlain.Checked then rbBackPlainClick(Self)
                           else rbBackBitmapClick(Self);

    eBackBitmap.Text := BackBitmapName;
    if eBackBitmap.Text <> '' then
      if FileExists(eBackBitmap.Text) then begin
        imgBackBitmap.Picture.LoadFromFile(eBackBitmap.Text);
        ShowBackBitmap;
      end;

    cbNoDragDropFrame.Checked := DisableDragDropFrames;
    cbShowToolTips.Checked    := ShowToolTips;
    cbUseOriginalicons.Checked := UseOriginalIcons;

    // Spatial Refs
    FSpatialRefSystem           := AppSettings.SpatialRefSystem;
    cbGridRefsAsSquares.Checked := AppSettings.GridRefsAsSquares;
    SetSpatialRefSystem;

    // File Locations
    eRucksackFiles.Text  := AppSettings.RucksackPath;
    eTemplates.Text      := AppSettings.ReportTemplatePath;
    eExportTemplates.Text:= AppSettings.ExportTemplatePath;
    eSnapshots.Text      := AppSettings.SnapshotPath;
    eReports.Text        := AppSettings.ReportPath;
    eRecordingCards.Text := AppSettings.RecordingCardPath;
    eImageFiles.Text     := AppSettings.LocalImagesPath;
    ePolygonFilters.Text := AppSettings.PolygonFilterPath;
    eBatchUpdates.Text   := AppSettings.BatchUpdatePath;
    eExternalFiles.Text  := AppSettings.ExternalFilePath;

    //Sundry
     cbPlaceCardDocs.Checked :=  AppSettings.AddDocsToOccurrence;

  end;  // with AppSettings

  //Help Setup
  hlpOptions                  := TOnlineHelp.Create(Self.Handle);
  Self.OnHelp                 := hlpOptions.OnHelpReplacement;
  Self.HelpContext            := IDH_OPTIONS;
  tsGeneral.HelpContext       := IDH_OPTIONSGENERAL;
  tsMainToolbar.HelpContext   := IDH_OPTIONSMAIN;
  tsAppearance.HelpContext    := IDH_OPTIONSAPP;
  tsSpatialRef.HelpContext    := IDH_OPTIONSSPATIAL;
  tsFileLocations.HelpContext := IDH_OPTIONSFILES;
  pcOptionsPages.HelpContext  := tsGeneral.HelpContext;


end;  // FormCreate

//==============================================================================
procedure TdlgOptions.FormDestroy(Sender: TObject);
begin
  hlpOptions.Free;
end;  // FormDestroy

//==============================================================================
{ First add all items to the spatial refs list box.  Then, Loop through each
    item in the spatial ref list.  If the item found has an object which defines
    a system equal to the current system, then check the list box item }
procedure TdlgOptions.SetSpatialRefSystem;
var
  i:Integer;
  lSystemName : TSpatialSystemName;
begin
  with cblbSpatialRefs do
  begin
    for i := 0 to Items.Count - 1 do TSpatialSystemName(Items.Objects[i]).Free;
    Items.Clear;
    { Add all standard systems to the check list box }
    // OS_GB
    lSystemName      := TSpatialSystemName.Create;
    lSystemName.Name := OS_GB;
    Items.AddObject(ResStr_GBTitle, lSystemName);
    // OS_NI
    lSystemName      := TSpatialSystemName.Create;
    lSystemName.Name := OS_NI;
    Items.AddObject(ResStr_NITitle, lSystemName);
    // LAT_LONG
    lSystemName      := TSpatialSystemName.Create;
    lSystemName.Name := LAT_LONG;
    Items.AddObject(ResStr_LatLongTitle, lSystemName);
    // UTM
    lSystemName      := TSpatialSystemName.Create;
    lSystemName.Name := UTM;
    Items.AddObject(ResStr_UTMTitle, lSystemName);

    { Now add any COM spatial systems }
    InitCOMSpatialSystems;

    for i := 0 to Items.Count - 1 do
      Checked[i] := TSpatialSystemName(Items.Objects[i]).Name = FSpatialRefSystem;
  end;  // with cblbSpatialRefs
end;  // SetSpatialRefSystem

//==============================================================================
{ Populate the spatial references list with any extra ones declared by COM
     addins }
procedure TdlgOptions.InitCOMSpatialSystems;
var
  i: Integer;
  lSpatialSystem: TSpatialSystemName;
  lAddin: IRecorderAddin;
  lNamedSystem: INamedSpatialReference;
  lBaseMapFormat: IBaseMapFormat;
  intf: IInterface;
begin
  for i := 0 to AppSettings.ComAddins.SpatialSystemInterfaces.Count - 1 do
  begin
    intf := AppSettings.ComAddins.SpatialSystemInterfaces[i];

    lSpatialSystem      := TSpatialSystemName.Create;
    lSpatialSystem.Name := (intf as ISpatialReference).SpatialRefSystem;

    if Supports(intf, IID_INamedSpatialReference, lNamedSystem) then
    begin
      if not Supports(intf, IID_IBaseMapFormat, lBaseMapFormat) then
        cblbSpatialRefs.Items.AddObject(
            (intf as INamedSpatialReference).SpatialSystemName,
            lSpatialSystem)
    end else
    if Supports(intf, IID_IRecorderAddin, lAddin) then
      cblbSpatialRefs.Items.AddObject((intf as IRecorderAddin).Name, lSpatialSystem)
    else
      cblbSpatialRefs.Items.AddObject(lSpatialSystem.Name, lSpatialSystem);
  end;
end;  // InitCOMSpatialSystems

//==============================================================================
procedure TdlgOptions.DrawListItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
  lAction           : TAction;
  lCaptionTopSpacing: Integer;
begin
  with TListBox(Control) do
  begin
    Canvas.FillRect(Rect);       { clear the rectangle }
    lCaptionTopSpacing := (ItemHeight - Canvas.TextHeight('A')) div 2;

    { Get the action object to draw }
    if Items.Objects[Index] is TAction then
    begin
      lAction := TAction(Items.Objects[Index]);
      { Draw its glyph }
      dmFormActions.ilMenuOn.Draw(Canvas,Rect.Left + 2, Rect.Top + 2, lAction.ImageIndex, true);
      { display the text }
      Canvas.TextOut(
          Rect.Left + dmFormActions.ilMenuOn.Width + 6,
          Rect.Top + lCaptionTopSpacing,
          Items[Index]);
    end else
      Canvas.TextOut(
          Rect.Left + 2,
          Rect.Top + lCaptionTopSpacing,
          '-- ' + ResStr_Separator + ' --');
  end;
end;  // DrawListItem

//==============================================================================
procedure TdlgOptions.bbAddClick(Sender: TObject);
begin
  if lbAvailable.ItemIndex <> -1 then AddToToolbar;
end;  // sbAddClick

//==============================================================================
procedure TdlgOptions.AddToToolbar;
var
  iActionIndex: Integer;
  stText      : String;
begin
  iActionIndex := lbAvailable.ItemIndex;
  stText       := lbAvailable.Items[iActionIndex];

  // Checks the item is not already on toolbar
  if iActionIndex = 0 then
    lbOnToolbar.Items.Add(stText)
  else
  if lbOnToolbar.Items.IndexOf(stText) = -1 then
  begin
    lbOnToolbar.Items.AddObject(stText,lbAvailable.Items.Objects[iActionIndex]);
    bbRemove.Enabled   := true;
    bbClearAll.Enabled := true;
  end;
end;  // AddToToolbar

//==============================================================================
procedure TdlgOptions.bbRemoveClick(Sender: TObject);
begin
  with lbOnToolbar do begin
    if ItemIndex <> -1 then Items.Delete(ItemIndex);
    bbRemove.Enabled   := Items.Count > 0;
    bbClearAll.Enabled := Items.Count > 0;
  end;
end;  // sbRemoveClick

//==============================================================================
procedure TdlgOptions.bbClearAllClick(Sender: TObject);
begin
  lbOnToolbar.Items.Clear;
  bbRemove.Enabled   := false;
  bbClearAll.Enabled := false;
  bbMoveUp.Enabled   := false;
  bbMoveDown.Enabled := false;
end;  // sbClearAllClick

//==============================================================================
procedure TdlgOptions.lbOnToolbarClick(Sender: TObject);
begin
  with lbOnToolbar do begin
    bbMoveUp.Enabled   := (ItemIndex <> -1) and (ItemIndex > 0);
    bbMoveDown.Enabled := (ItemIndex <> -1) and (ItemIndex < Items.Count - 1);
  end;
end;  // lbOnToolbarClick

//==============================================================================
procedure TdlgOptions.bbMoveUpClick(Sender: TObject);
var iIndex:Integer;
begin
  if lbOnToolbar.ItemIndex <> -1 then
    with lbOnToolbar do begin
      iIndex := ItemIndex;
      Items.Move(iIndex, iIndex - 1);
      Dec(iIndex);
      bbMoveUp.Enabled   := (iIndex <> -1) and (iIndex > 0);
      bbMoveDown.Enabled := (iIndex <> -1) and (iIndex < Items.Count - 1);
      ItemIndex := iIndex;
    end;
end;  // sbMoveUpClick

//==============================================================================
procedure TdlgOptions.bbMoveDownClick(Sender: TObject);
var iIndex:Integer;
begin
  if lbOnToolbar.ItemIndex <> -1 then
    with lbOnToolbar do begin
      iIndex := ItemIndex;
      Items.Move(ItemIndex,ItemIndex + 1);
      Inc(iIndex);
      bbMoveUp.Enabled   := (iIndex <> -1) and (iIndex > 0);
      bbMoveDown.Enabled := (iIndex <> -1) and (iIndex < Items.Count - 1);
      ItemIndex := iIndex;
    end;
end;  // sbMoveDownClick

//==============================================================================
procedure TdlgOptions.lbOnToolbarDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
var ItemPos:TPoint;
    iIndex :Integer;
begin
  Accept:=Source=Sender;
  if Accept then begin
    ItemPos := Point(X, Y);
    iIndex  := lbOnToolbar.ItemAtPos(ItemPos, true);
    if iIndex <> -1 then
      with lbOnToolbar do begin
        Items.Move(ItemIndex, iIndex);
        ItemIndex := iIndex;
        bbMoveUp.Enabled   := (ItemIndex <> -1) and (ItemIndex > 0);
        bbMoveDown.Enabled := (ItemIndex <> -1) and (ItemIndex < Items.Count - 1);
      end;
  end;
end;  // lbOnToolbarDragOver

//==============================================================================
procedure TdlgOptions.btnUndoAppearanceClick(Sender: TObject);
begin
  with AppSettings do
  begin
    colMandatoryField.ActiveColor := MandatoryColour;
    colDragSource.ActiveColor     := DragSourceColour;
    colDragDest.ActiveColor       := DragDestColour;

    rbBackPlain.Checked  := PlainBackground;
    rbBackBitmap.Checked := not rbBackPlain.Checked;
    if rbBackPlain.Checked then rbBackPlainClick(Sender)
                           else rbBackBitmapClick(Sender);
    eBackBitmap.Text := BackBitmapName;
    if eBackBitmap.Text <> '' then
    begin
      imgBackBitmap.Picture.LoadFromFile(AppSettings.BackBitmapName);
      ShowBackBitmap;
    end else
      imgBackBitmap.Picture := nil;

    cbNoDragDropFrame.Checked := DisableDragDropFrames;
    cbShowToolTips.Checked    := ShowToolTips;
  end;
end;  // btnUndoAppearanceClick

//==============================================================================
procedure TdlgOptions.bbSurveyNameClick(Sender: TObject);
begin
  dmFormActions.actObservations.Execute;
end;  // sbSurveyNameClick

//==============================================================================
procedure TdlgOptions.bbTaxonGroupClick(Sender: TObject);
begin
  dmFormActions.actTaxonDiction.Execute;
end;  // sbTaxonGroupClick

//==============================================================================
procedure TdlgOptions.bbGeoAreaClick(Sender: TObject);
begin
  dmFormActions.actLocations.Execute;
end;  // sbGeoAreaClick

//==============================================================================
function TdlgOptions.SelectFolder(const AFolder, AMsg: String): String;
var
  stFolder: String;
begin
  Result := AFolder;

  dlgFolder.Title := Format(SOptions_SelectFolder, [AMsg]);
  stFolder        := AFolder;
  if stFolder <> '' then
	  if stFolder[Length(stFolder)] = '\' then stFolder := Copy(stFolder, 1, Length(stFolder) - 1);
  dlgFolder.Folder := stFolder;
  if dlgFolder.Execute then
    Result := dlgFolder.Folder + '\';
end;  // SelectFolder

//==============================================================================
procedure TdlgOptions.btnRucksackBrowseClick(Sender: TObject);
begin
  eRucksackFiles.Text := SelectFolder(eRucksackFiles.Text, ResStr_RuckSacks);
end;  // btnRucksackBrowseClick

//==============================================================================
{ Ellipses button for Templates path }
procedure TdlgOptions.btnTemplateBrowseClick(Sender: TObject);
begin
  eTemplates.Text := SelectFolder(eTemplates.Text, ResStr_Template);
end;  // btnTemplateBrowseClick

//==============================================================================
{ Ellipses button for Export Templates path }
procedure TdlgOptions.btnExportTemplateBrowseClick(Sender: TObject);
begin
  eExportTemplates.Text := SelectFolder(eExportTemplates.Text, ResStr_ExportTemplate);
end;  // btnTemplateBrowseClick

//==============================================================================
{ Ellipses button for Reports path }
procedure TdlgOptions.btnReportBrowseClick(Sender: TObject);
begin
  eReports.Text := SelectFolder(eReports.Text, ResStr_Queries);
end;  // btnReportBrowseClick

//==============================================================================
{ Ellipses button for recording card path }
procedure TdlgOptions.btnRecordingCardBrowseClick(Sender: TObject);
begin
  eRecordingCards.Text := SelectFolder(eRecordingCards.Text, ResStr_RecordingCards);
end;  // btnRecordingCardBrowseClick

//==============================================================================
{ Ellipses button for images path }
procedure TdlgOptions.btnImageFilesBrowseClick(Sender: TObject);
begin
  eImageFiles.Text := SelectFolder(eImageFiles.Text, ResStr_ImageFiles);
end;

//==============================================================================
procedure TdlgOptions.btnPolygonFiltersBrowseClick(Sender: TObject);
begin
  ePolygonFilters.Text := SelectFolder(ePolygonFilters.Text, ResStr_PolygonFilters);
end;  // btnPolygonFiltersBrowseClick

//==============================================================================
procedure TdlgOptions.btnBatchUpdatesBrowseClick(Sender: TObject);
begin
  eBatchUpdates.Text := SelectFolder(eBatchUpdates.Text, ResStr_BatchUpdates);
end;  // btnBatchUpdatesBrowseClick

//==============================================================================
procedure TdlgOptions.rbBackPlainClick(Sender: TObject);
begin
  eBackBitmap.Enabled     := false;
  btnBitmapBrowse.Enabled := false;
  imgBackBitmap.Visible   := false;
end;  // rbBackPlainClick

//==============================================================================
procedure TdlgOptions.rbBackBitmapClick(Sender: TObject);
begin
  eBackBitmap.Enabled     := true;
  btnBitmapBrowse.Enabled := true;
  imgBackBitmap.Visible   := true;
  ShowBackBitmap;
end;  // rbBackBitmapClick

//==============================================================================
procedure TdlgOptions.btnBitmapBrowseClick(Sender: TObject);
var
  bmp: TBitmap;
begin
  if dlgOpen.Execute then
    if FileExists(dlgOpen.FileName) then
    begin
      eBackBitmap.Text := dlgOpen.FileName;
      imgBackBitmap.Picture.LoadFromFile(dlgOpen.FileName);
      try
        imgBackBitmap.Picture.LoadFromFile(dlgOpen.FileName);
      except
        on EInvalidGraphic do
          imgBackBitmap.Picture.Graphic := nil;
      end;
      if imgBackBitmap.Picture.Graphic is TJPEGImage then
      begin
        bmp := TBitmap.Create;
        try
          bmp.Assign(TJPEGImage(imgBackBitmap.Picture.Graphic));
          imgBackBitmap.Picture.Bitmap.Assign(bmp);
        finally
          bmp.Free;
        end;
      end;
      ShowBackBitmap;
    end;
end;  // btnBitmapBrowseClick

//==============================================================================
procedure TdlgOptions.colMandatoryFieldChange(Sender: TObject);
begin
  eMandatory.Color := colMandatoryField.ActiveColor;
end;  // colMandatoryFieldChange

//==============================================================================
procedure TdlgOptions.colDragSourceChange(Sender: TObject);
begin
  ShapeSource.Pen.Color       := colDragSource.ActiveColor;
  ShapeSourceDest.Brush.Color := colDragSource.ActiveColor;
end;  // colDragSourceChange

//==============================================================================
procedure TdlgOptions.colDragDestChange(Sender: TObject);
begin
  ShapeDest.Pen.Color       := colDragDest.ActiveColor;
  ShapeSourceDest.Pen.Color := colDragDest.ActiveColor;
end;  // colDragDestChange

//==============================================================================
procedure TdlgOptions.eBackBitmapKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then
  begin
    if eBackBitmap.Text <> AppSettings.BackBitmapName then
      if FileExists(eBackBitmap.Text) then
      begin
        imgBackBitmap.Picture.LoadFromFile(eBackBitmap.Text);
        ShowBackBitmap;
      end else
      if eBackBitmap.Text = '' then
        imgBackBitmap.Picture.Assign(nil)
      else begin
        MessageDlg(ResStr_InvalidPath, mtWarning,[mbOk],0);
        eBackBitmap.SetFocus;
      end;
    Key:=#0;
  end;
end;  // eBackBitmapKeyPress

//==============================================================================
procedure TdlgOptions.eBackBitmapExit(Sender: TObject);
begin
  if eBackBitmap.Text <> AppSettings.BackBitmapName then
    if FileExists(eBackBitmap.Text) then
    begin
      imgBackBitmap.Picture.LoadFromFile(eBackBitmap.Text);
      ShowBackBitmap;
    end else
    if eBackBitmap.Text = '' then
      imgBackBitmap.Picture.Assign(nil)
    else begin
      MessageDlg(ResStr_InvalidPath, mtWarning, [mbOk], 0);
      eBackBitmap.SetFocus;
    end;
end;  // eBackBitmapExit

//==============================================================================
procedure TdlgOptions.btnDefaultClick(Sender: TObject);
begin
  if pcOptionsPages.ActivePage=tsGeneral then
  begin
    // General options
    cbShowLastSessionWindows.Checked  := DEFAULT_SHOW_LAST_SESSION_WINDOWS;
    cbShowMenuIcons.Checked           := DEFAULT_SHOW_MENU_ICONS;
    cbGraduatedMenus.Checked          := DEFAULT_GRADUATED_MENUS;
    cbShowMainToolbar.Checked         := DEFAULT_SHOW_MAIN_TOOLBAR;
    cbShowActiveWindowToolbar.Checked := DEFAULT_SHOW_CONTEXT_TOOLBAR;
    cbShowWelcomeAtStart.Checked      := DEFAULT_SHOW_WELCOME_AT_START;
    cbShowCommonNames.Checked         := DEFAULT_SHOW_COMMON_NAMES;
    cbShowNamesAsEntered.Checked      := DEFAULT_SHOW_ENTERED_NAMES;
    cbShowAuthors.Checked             := DEFAULT_SHOW_AUTHORS;
    cbFullTranslation.Checked         := DEFAULT_FULL_TRANSLATION;
    cbAutoEmail.Checked               := DEFAULT_AUTO_SCHEME_EMAIL;
    cbPlaceCardDocs.Checked           := DEFAULT_ADD_DOCS_TO_OCCURRENCE;
    eDateCutYear.Text                 := RightStr('0' + IntToStr(DEFAULT_CUT_YEAR), 2);
    chkRememberFilters.Checked        := DEFAULT_REMEMBER_FILTERS;
    chkPartialTaxonSearch.Checked     := DEFAULT_PARTIAL_TAXON_SEARCH;
    chkPartialTaxonSearch.Checked     := DEFAULT_AUTO_COMPLETE_SEARCH;
    chkUsePreferredTaxa.Checked       := DEFAULT_USE_PREFERRED_TAXA;
    if AppSettings.UserAccessLevel = ualAdmin then
    begin
      cmbConfAccessLevel.ItemIndex    := ConfidentialLevelToIndex(DEFAULT_CONFIDENTIAL_ACCESS_LEVEL);
      cbExportConfidentialOcc.Checked := DEFAULT_EXPORT_CONFIDENTIAL_OCC;
    end;
    chkOrganiseSurveysByTag.Checked    := DEFAULT_ORGANISE_SURVEYS_BY_TAG;
    chkUseOldImportWizard.Checked      := DEFAULT_USE_OLD_IMPORT_WIZARD;
    chkIgnoreRememberedMatches.Checked := DEFAULT_IGNORE_REMEMBERED_MATCHES;
    cbUseOriginalicons.Checked                := DEFAULT_USE_ORIGINAL_ICONS;
  end else
  if pcOptionsPages.ActivePage=tsMainToolbar then
  begin
    // Main Toolbar
    with lbOnToolbar.Items, dmFormActions do
    begin
      Clear;
      AddObject(TrimText(actObservations.Caption),actObservations);
      AddObject(TrimText(actLocations.Caption),actLocations);
      AddObject(TrimText(actNames.Caption),actNames);
      AddObject(TrimText(actDocuments.Caption),actDocuments);
      Add('-- ' + ResStr_Separator + ' --');
      AddObject(TrimText(actTaxonDiction.Caption),actTaxonDiction);
      AddObject(TrimText(actBiotopeDiction.Caption),actBiotopeDiction);
      AddObject(TrimText(actAdminAreaDiction.Caption),actAdminAreaDiction);
      Add('-- ' + ResStr_Separator + ' --');
      AddObject(TrimText(actMapWindow.Caption),actMapWindow);
      Add('-- ' + ResStr_Separator + ' --');
      AddObject(TrimText(actRucksack.Caption),actRucksack);
    end;
  end else
  if pcOptionsPages.ActivePage=tsAppearance then
  begin
    // Appearance
    rbBackPlain.Checked           := true;
    eBackBitmap.Enabled           := false;
    btnBitmapBrowse.Enabled       := false;
    colMandatoryField.ActiveColor := DEFAULT_MANDATORY_COLOUR;
    colDragSource.ActiveColor     := DEFAULT_DRAG_SOURCE_COLOUR;
    colDragDest.ActiveColor       := DEFAULT_DRAG_DEST_COLOUR;
    cbNoDragDropFrame.Checked     := DEFAULT_DISABLE_DRAG_DROP_FRAME;
    cbShowToolTips.Checked        := DEFAULT_SHOW_TOOL_TIPS;
  end else
  if pcOptionsPages.ActivePage=tsSpatialRef then
  begin
    // Grid References
    FSpatialRefSystem           := DEFAULT_SPATIAL_REF_SYSTEM;
    cbGridRefsAsSquares.Checked := DEFAULT_GRID_REFS_AS_SQUARES;
    SetSpatialRefSystem;
  end else
  if pcOptionsPages.ActivePage=tsFileLocations then
    ResetDefaultPaths;
  SetAddinPagesToDefault;
  PopulateSettingFields('Data_default');
end;  // btnDefaultClick


//==============================================================================
{ Reset file paths to their post installation status.  To do this, we have to
    make some assumptions as there is no record of the paths.
}
procedure TdlgOptions.ResetDefaultPaths;

  procedure SetPathTo(AEdit: TEdit; const AFolder: String);
  var
    path: String;
  begin
    path := GetProgramDataFolder(AFolder);
    if DirectoryExists(path) then AEdit.Text := path
  end;

begin
  SetPathTo(eRucksackFiles,  PATH_RUCKSACKS);
  SetPathTo(eTemplates,      PATH_REPORT_TEMPLATES);
  SetPathTo(eExportTemplates,PATH_EXPORT_TEMPLATES);
  SetPathTo(eSnapshots,      PATH_SNAPSHOTS);
  SetPathTo(eReports,        PATH_REPORTS);
  SetPathTo(eRecordingCards, PATH_RECORDING_CARDS);
  SetPathTo(ePolygonFilters, PATH_POLYGON_FILTERS);
  SetPathTo(eImageFiles,     PATH_LOCAL_IMAGES);
  SetPathTo(eBatchUpdates,   PATH_BATCH_UPDATES);
  SetPathTo(eExternalFiles,  PATH_EXTERNAL_FILES);
end;

//==============================================================================
procedure TdlgOptions.cbShowActiveWindowToolbarClick(Sender: TObject);
begin
  if frmMain.MDIChildCount > 0 then
    frmMain.tbContext.Visible := cbShowActiveWindowToolbar.Checked;
end;  // cbShowActiveWindowToolbarClick

//==============================================================================
procedure TdlgOptions.ShowBackBitmap;
var
  picHeight, picWidth, pnlHeight, pnlWidth, newHeight, newWidth: Integer;
begin
  if Assigned(imgBackBitmap.Picture) then
  begin
    picHeight := imgBackBitmap.Picture.Height;
    picWidth  := imgBackBitmap.Picture.Width;
    pnlHeight := pnlBackBitmap.Height;
    pnlWidth  := pnlBackBitmap.Width;

    if (picWidth > 0) and (picHeight > 0) then
      with imgBackBitmap do
        if (picHeight > pnlHeight) or (picWidth > pnlWidth) then
        begin
          newHeight := (picHeight*(pnlWidth - 2)) div picWidth;
          newWidth  := (picWidth*(pnlHeight - 2)) div picHeight;
          Stretch := true;
          if (picHeight >= picWidth) or (newHeight > pnlHeight) then
          begin
            Height := pnlHeight - 2;
            Width  := newWidth;
            Top    := 1;
            Left   := (pnlWidth - Width) div 2;
          end else begin
            Width  := pnlWidth - 2;
            Height := newHeight;
            Top    := (pnlHeight - Height) div 2;
            Left   := 1;
          end
        end else begin
          Stretch := false;
          Top     := 1;
          Left    := 1;
          Height  := pnlHeight - 2;
          Width   := pnlWidth - 2;
        end;
  end;
end;  // ShowBackBitmap

//==============================================================================
procedure TdlgOptions.bbOkClick(Sender: TObject);
var
  lToolButton: TXPToolButton;
  i: Integer;
  lToolbar: TToolbar;
  lCheck: boolean;
  locForm: TForm;

    {---------------------------------------------------------------------------
      Gets the path stored in the TEdit control- or if the text is blank returns
      the default path.
    }
    function Path(ePath: TEdit; default: String): String;
    var
      testPath: String;
    begin
      if ePath.Text = '' then begin
        testPath := GetProgramDataFolder(default);
        if DirectoryExists(testPath) then
          Result := testPath
        else
          Result := GetProgramDataFolder(''); //If all else fails, set to the root
                                              // directory of the program.
      end else
        Result := ePath.Text;
    end;

begin
  SaveSettingTable;

  ValidateDelimiter;

  if cmbTaxonRestriction.ItemIndex = -1 then cmbTaxonRestriction.ItemIndex := 0;

  // Check a Spatial Ref is selected
  lCheck := false;
  for i := 0 to cblbSpatialRefs.Items.Count - 1 do
    lCheck := lCheck or cblbSpatialRefs.Checked[i];

  if not lCheck then
  begin
    pcOptionsPages.ActivePage := tsSpatialRef;
    cblbSpatialRefs.SetFocus;
    ModalResult := mrNone;
    raise TExceptionPath.CreateValidation(ResStr_SelectSRSystem, cblbSpatialRefs);
  end;
  lCheck :=
      CheckFileLocation(eRucksackFiles) and
      CheckFileLocation(eRecordingCards) and
      CheckFileLocation(eTemplates) and
      CheckFileLocation(eExportTemplates) and
      CheckFileLocation(eReports) and
      CheckFileLocation(eSnapshots) and
      CheckFileLocation(eImageFiles) and
      CheckFileLocation(ePolygonFilters) and
      CheckFileLocation(eBatchUpdates);

  // Hide main toolbar if no buttons selected, but leave as set otherwise.
  if lbOnToolbar.Items.Count = 0 then cbShowMainToolbar.Checked := false;

  if lCheck then begin
    if ((AppSettings.IncludeLocationSpatialRef<>chkIncludeLocationSpatialRef.Checked) or
        (AppSettings.IncludeLocationFileCode<>chkIncludeLocationFileCode.Checked)) then begin
      locForm := dmFormActions.GetFormInstance(TfrmLocations);
      if Assigned(locForm) then
        TfrmLocations(locForm).tvLocations.Invalidate;
    end;
    with AppSettings do begin
      // General Options
      ShowLastSessionWindows  := cbShowLastSessionWindows.Checked;
      AddDocsToOccurrence   := cbPlaceCardDocs.checked;
      ShowMenuIcons           := cbShowMenuIcons.Checked;
      GraduatedMenus          := cbGraduatedMenus.Checked;
      ShowMainToolbar         := cbShowMainToolbar.Checked;
      ShowContextToolbar      := cbShowActiveWindowToolbar.Checked;
      ShowWelcomeAtStart      := cbShowWelcomeAtStart.Checked;
      DisplayTaxonCommonNames := cbShowCommonNames.Checked;
      DisplayTaxonEnteredNames:= cbShowNamesAsEntered.Checked;
      DisplayTaxonAuthors     := cbShowAuthors.Checked;
      UseRecommendedTaxaNames := cbFullTranslation.Checked;
      AutoSchemeEmail         := cbAutoEmail.Checked;
      DateCutYear             := StrToInt(eDateCutYear.Text);
      RememberFilters         := chkRememberFilters.Checked;
      PartialTaxonSearch      := chkPartialTaxonSearch.Checked;
      AutoCompleteSearch      := chkAutoCompleteSearch.Checked;
      UsePreferredTaxa        := chkUsePreferredTaxa.Checked;
      IncludeLocationSpatialRef := chkIncludeLocationSpatialRef.Checked;
      IncludeLocationFileCode := chkIncludeLocationFileCode.Checked;
      RapidEntryDelimiter     := eRapidEntryDelimiter.Text;
      OrganiseSurveysByTag    := chkOrganiseSurveysByTag.Checked;
      UseOldImportWizard      := chkUseOldImportWizard.Checked;
      IgnoreRememberedMatches    := chkIgnoreRememberedMatches.Checked;
      TaxonomicSearchRestriction := cmbTaxonRestriction.Items[cmbTaxonRestriction.ItemIndex];
      ExtraLocationSearchColumns := Self.ExtraLocationSearchColumns;

      // Only admin can change these values, and therefore save them.
      if UserAccessLevel = ualAdmin then begin
        ExportConfidentialOccurrences := cbExportConfidentialOcc.Checked;
        SetConfidentialAccess(
            IndexToConfidentialLevel,
            cmbConfAccessLevel.ItemIndex = IDX_FULLALL);
      end;
      // General Options
      lToolbar := frmMain.tbMainToolbar;
      { Update the toolbar - first get the right number of buttons }
      for i:=lToolbar.ButtonCount - 1 downto 0 do lToolbar.Buttons[i].Free;

      for i := lbOnToolbar.Items.Count - 1 downto 0 do
      begin
        lToolButton := TXPToolButton.Create(lToolbar);
        if lbOnToolbar.Items.Objects[i] is TAction then
        begin
          lToolButton.Action := TAction(lbOnToolbar.Items.Objects[i]);
          if lToolButton.Action = dmFormActions.actSpeciesForPlace then begin
            lToolButton.Style        := tbsDropDown;
            lToolButton.Width        := 39;
            lToolButton.DropDownMenu := frmMain.pmRecordCards;
          end else
          if lToolButton.Action = dmFormActions.actMapWindow then begin
            lToolButton.Style        := tbsDropDown;
            lToolButton.Width        := 39;
            lToolButton.DropDownMenu := frmMain.pmMapWindow;
          end else
          if lToolButton.Action is TCOMAction then
            lToolButton.Tag := TCOMAction(lToolButton.Action).Index; // helps with COM refresh
        end else begin
          lToolButton.Style := tbsSeparator;
          lToolButton.Width := 8;
        end;
        lToolButton.Parent := lToolbar;
      end;

      // Appearance
      MandatoryColour       := colMandatoryField.ActiveColor;
      DragSourceColour      := colDragSource.ActiveColor;
      DragDestColour        := colDragDest.ActiveColor;
      PlainBackground       := rbBackPlain.Checked;
      BackBitmapName        := IfThen(PlainBackground, '', eBackBitmap.Text);
      DisableDragDropFrames := cbNoDragDropFrame.Checked;
      ShowToolTips          := cbShowToolTips.Checked;
      UseOriginalIcons      := cbUseOriginalicons.Checked;

      // Spatial Ref System
      SpatialRefSystem  := NewSpatialRefSystem;
      GridRefsAsSquares := cbGridRefsAsSquares.Checked;

      // File Locations
      RucksackPath       := Path(eRucksackFiles,  PATH_RUCKSACKS);
      ReportTemplatePath := Path(eTemplates,      PATH_REPORT_TEMPLATES);
      ExportTemplatePath := Path(eExportTemplates,PATH_EXPORT_TEMPLATES);
      SnapshotPath       := Path(eSnapshots,      PATH_SNAPSHOTS);
      ReportPath         := Path(eReports,        PATH_REPORTS);
      RecordingCardPath  := Path(eRecordingCards, PATH_RECORDING_CARDS);
      PolygonFilterPath  := Path(ePolygonFilters, PATH_POLYGON_FILTERS);
      LocalImagesPath    := Path(eImageFiles,     PATH_LOCAL_IMAGES);
      BatchUpdatePath    := Path(eBatchUpdates,   PATH_BATCH_UPDATES);
      ExternalFilePath   := Path(eExternalFiles,  PATH_EXTERNAL_FILES);
    end; // with AppSettings



    SaveAddinPages;
    ModalResult := mrOk;
    AppSettings.WriteRegistrySettings;
  end else
    ModalResult := mrNone;
end;  // bbOkClick

//==============================================================================
function TdlgOptions.CheckFileLocation(AEdit:TEdit):boolean;
var
  folder: String;
begin
  Result := (AEdit.Text = '') or DirectoryExists(AEdit.Text);
  if not Result then
  begin
    pcOptionsPages.ActivePage := tsFileLocations;
    MessageDlg(ResStr_InvalidFolder, mtWarning,[mbOk],0);
    AEdit.SetFocus;
  end else begin
    folder := AEdit.Text;
    if folder <> '' then folder := IncludeTrailingPathDelimiter(folder);
    AEdit.Text := folder;
  end;
end;  // CheckFileLocation

//==============================================================================
procedure TdlgOptions.cblbSpatialRefsClick(Sender: TObject);
var
  i, idx: Integer;
begin
   {First remove the old selections apart from the current one }
  if FLeftMouseButton then
    with cblbSpatialRefs do begin
      idx := ItemIndex;
      for i := 0 to Items.Count - 1 do Checked[i] := False;
      FSpatialRefSystem := TSpatialSystemName(Items.Objects[idx]).Name;
      Checked[idx]      := true;
    end;
end;  // cblbSpatialRefsClick

//==============================================================================
{ Setting the Help Context of the Option Pages object }
procedure TdlgOptions.pcOptionsPagesChange(Sender: TObject);
begin
   pcOptionsPages.HelpContext := pcOptionsPages.ActivePage.HelpContext;
end;

//==============================================================================
procedure TdlgOptions.cblbSpatialRefsMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  liIndex: Integer;
begin
  FLeftMouseButton := False;
  if (Button = mbLeft) then
   FLeftMouseButton := True
  else
  begin
   {Store index of currently checked box}
   liIndex := 0;
   while not cblbSpatialRefs.Checked[liIndex] and (liIndex < cblbSpatialRefs.Items.Count-1) do
     Inc(liIndex);
   FCheckedIndex := liIndex;
  end;
end;

//==============================================================================
procedure TdlgOptions.cblbSpatialRefsClickCheck(Sender: TObject);
var
  i: Integer;
begin
  {Clicked-on Check box is already checked. Test for which button used to click
  and if right mouse button reset display before ONClick event}
  if not FLeftMouseButton then
  begin
    for i:= 0 to cblbSpatialRefs.Items.Count - 1 do
      cblbSpatialRefs.Checked[i] := False;
    cblbSpatialRefs.Checked[FCheckedIndex] := true;
  end //if
end;

//==============================================================================
procedure TdlgOptions.bbCancelClick(Sender: TObject);
begin
  if frmMain.MDIChildCount > 0 then
    frmMain.tbContext.Visible := AppSettings.ShowContextToolbar;
  ModalResult := mrCancel;
end;  // bbCancelClick

//==============================================================================
procedure TdlgOptions.eDateCutYearKeyPress(Sender: TObject; var Key: Char);
begin
  if not (Key in [#8, '0'..'9']) then Key := #0;
end;  // eDateCutYearKeyPress

//==============================================================================
procedure TdlgOptions.eDateCutYearExit(Sender: TObject);
var
  lVal,lErr: Integer;
begin
  if (eDateCutYear.Text = '') then eDateCutYear.Text := IntToStr(AppSettings.DateCutYear);
  Val(eDateCutYear.Text, lVal, lErr);
  if (lErr <> 0) or not (lVal in [0..99]) then
    raise TExceptionPath.CreateValidation(ResStr_InvalidCutoffYear, eDateCutYear)
  else
    eDateCutYear.Text := RightStr('0' + eDateCutYear.Text, 2);
end;  // eDateCutYearExit

//==============================================================================
{ Don't allow header for list of rucksacks to be selected }
procedure TdlgOptions.cmbTaxonRestrictionChange(Sender: TObject);
begin
  if cmbTaxonRestriction.Text = ResStr_ContentsOfRucksack then
    cmbTaxonRestriction.ItemIndex := 0;
end;

//==============================================================================
{ Ensure that the Graduated Menus option is only enabled when the Show Menu
     Icons option is checked }
procedure TdlgOptions.cbShowMenuIconsClick(Sender: TObject);
begin
  cbGraduatedMenus.Enabled := cbShowMenuIcons.Checked;
end;

//==============================================================================
procedure TdlgOptions.btnSnapshotBrowseClick(Sender: TObject);
begin
  eSnapshots.Text := SelectFolder(eSnapshots.Text, ResStr_Snapshots);
end;

{-------------------------------------------------------------------------------
  Adds a single page declared by an Addin to the page control
}
procedure TdlgOptions.AddAddinPage(APage: IOptionsPage);
var
  lNewPage: TTabSheet;
  lOleProxy: TOleProxy;
begin
  lNewPage := TTabSheet.Create(pcOptionsPages);
  lOleProxy := TOleProxy.Create(lNewPage, APage as IOleObject);
  lOleProxy.Parent     := lNewPage;
  lOleProxy.Align      := alClient;
  lNewPage.Caption     := APage.Title;
  lNewPage.PageControl := pcOptionsPages;
  FPageAddins.Add(APage);
  APage.Load;
end;

{-------------------------------------------------------------------------------
  Invokes the Default method on each addin page
}
procedure TdlgOptions.SetAddinPagesToDefault;
var
  i: Integer;
begin
  for i := 0 to FPageAddins.Count - 1 do
    (FPageAddins[i] as IOptionsPage).Default;
end;

{-------------------------------------------------------------------------------
  Invokes the Save method on each addin page
}
procedure TdlgOptions.SaveAddinPages;
var
  i: Integer;
begin
  for i := 0 to FPageAddins.Count - 1 do
    (FPageAddins[i] as IOptionsPage).Save;
end;

{-------------------------------------------------------------------------------
  Prevents entry of <, >, ~ and alphanumeric characters
}
procedure TdlgOptions.eRapidEntryDelimiterKeyPress(Sender: TObject; var Key: Char);
begin
  if (Key in BAD_DELIMITERS) then Key := #0;
end;

{-------------------------------------------------------------------------------
  Ensure that <, >, ~ and alphanumeric characters haven't been pasted in
}
procedure TdlgOptions.ValidateDelimiter;
begin
  if (Length(eRapidEntryDelimiter.Text) > 0)
     and (eRapidEntryDelimiter.Text[1] in BAD_DELIMITERS) then
  begin
    pcOptionsPages.ActivePage := tsGeneral;
    ModalResult               := mrNone; // don't close the dialog
    raise TExceptionPath.CreateValidation(ResStr_InvalidRapidEntryDelimiter, eRapidEntryDelimiter);
  end;
end;

{-------------------------------------------------------------------------------
  Display the dialog allowing the user to select the extra columns that are
  to be displayed when searching for locations.
}
procedure TdlgOptions.btnExtraLocationSearchColumnsClick(Sender: TObject);
var
  lDialog: TdlgSelectLocationSearchColumns;
begin
  lDialog := TdlgSelectLocationSearchColumns.Create(Self);
  try
    lDialog.SelectedColumns := ExtraLocationSearchColumns;
    if lDialog.ShowModal = mrOk then
      ExtraLocationSearchColumns := lDialog.SelectedColumns;
  finally
    lDialog.Release;
  end;
end;

{-------------------------------------------------------------------------------
  Set the selection of additional columns to display when searching for
  locations.
}
procedure TdlgOptions.SetExtraLocationSearchColumns(Selection: TLocationSearchColumns);
var
  lText: String;

  procedure AddColumn(const Name: String);
  begin
    if lText = ResStr_None then
      lText := Name
    else
      lText := lText + ', ' + Name;
  end;

begin
  lText := ResStr_None;
  if lscLocationType in Selection then AddColumn(ResStr_LocationType);
  if lscSpatialReference in Selection then AddColumn(ResStr_SpatialReference);
  if lscFileCode in Selection then AddColumn(ResStr_FileCode);
  lblExtraLocationSearchColumnList.Caption := lText;
  FExtraLocationSearchColumns := Selection;
end;

{-------------------------------------------------------------------------------
  Converts a user access level and restriction to the correct item to show
  in the combobox.
}
function TdlgOptions.ConfidentialLevelToIndex(level: TUserAccessLevel): Integer;
begin
  case level of
    ualAdmin   : Result := IDX_ADMIN;
    ualFullUser: Result := IfThen(AppSettings.ConfidentialFullEdit, IDX_FULLALL, IDX_FULLOWN);
    ualAddOnly : Result := IDX_ADDONLY;
    ualRecorder: Result := IDX_RECORDER;
  else
    Result := IDX_READONLY;
  end;
end;

{-------------------------------------------------------------------------------
  Converts the index in the combobox to a more useful user access level.
}
function TdlgOptions.IndexToConfidentialLevel: TUserAccessLevel;
begin
  case cmbConfAccessLevel.ItemIndex of
    IDX_ADMIN   : Result := ualAdmin;
    IDX_FULLOWN,
    IDX_FULLALL : Result := ualFullUser;
    IDX_ADDONLY : Result := ualAddOnly;
    IDX_RECORDER: Result := ualRecorder;
  else
    Result := ualReadOnly;
  end;
end;

{-------------------------------------------------------------------------------
  Display the browse dialog to pick the external file path.
}
procedure TdlgOptions.btnExternalFilePathClick(Sender: TObject);
begin
  eExternalFiles.Text := SelectFolder(eExternalFiles.Text, ResStr_ExternalFilePath);
end;


procedure TdlgOptions.btnMasterClick(Sender: TObject);
var rs : _Recordset;
begin
  rs := dmDatabase.ExecuteSQL('Select * From Computer_Map WHERE Computer_id = ''' +
        lblThisWorkStation.Caption + '''', true);
  if not rs.Eof then
    edMaster.Text := lblThisWorkStation.Caption
  else
    MessageDlg(Restr_Master_Workstation,mtConfirmation, [mbOk], 0);

end;

procedure TdlgOptions.PopulateSettingFields(const AField: String);
var rs : _recordset;
begin
  lblSettingWarning.Caption := ResStr_Setting_Warning;

  rs := dmDatabase.ExecuteSQL('Select host_name() as hostname', true);
  lblThisWorkstation.caption := rs.Fields['hostname'].Value;
  rs.close;

  rs := dmDatabase.ExecuteSQL('Select * From Setting', true);
  while not rs.Eof do begin
    if rs.Fields['Name'].Value = 'GatewayURL' then  edGatewayURL.text := rs.Fields[AField].Value
      else if rs.Fields['Name'].Value = 'HelpURL' then  edHelpURL.text := rs.Fields[AField].Value
      else if rs.Fields['Name'].Value = 'PrefLocs' then  edPreLocs.text := rs.Fields[AField].Value
      else if rs.Fields['Name'].Value = 'PrefNames' then  edPrefNames.text := rs.Fields[AField].Value
      else if rs.Fields['Name'].Value = 'SortMethod' then  edSortMethod.text := rs.Fields[AField].Value
      else if rs.Fields['Name'].Value = 'TaxDesList' then  edTaxDesList.text := rs.Fields[AField].Value
      else if rs.Fields['Name'].Value = 'TempLic' then  edTempLicence.text := rs.Fields[AField].Value
      else if rs.Fields['Name'].Value = 'TempName' then  edTempNames.text := rs.Fields[AField].Value
      else if rs.Fields['Name'].Value = 'Competency' then  edCompetency.text := rs.Fields[AField].Value
      else if rs.Fields['Name'].Value = 'BlockSize' then  edBlockSize.text := rs.Fields[AField].Value;
   rs.MoveNext
  end;
  rs.close;
  rs := dmDatabase.ExecuteSQL('Select Computer_Id  From Computer_Map WHERE Master = 1', true);
    if not rs.eof then edMaster.text := rs.Fields['Computer_Id'].Value
      else  edMaster.text := '';
  rs.close;
end;

procedure TdlgOptions.edCompetencyKeyPress(Sender: TObject; var Key: Char);
begin
   if not (Key in [#8, '0'..'9']) then Key := #0;
end;

procedure TdlgOptions.FormActivate(Sender: TObject);
begin
  // Setting table
 PopulateSettingFields('Data');
end;

procedure TdlgOptions.SaveSettingTable;
var sql : string;
begin
  if AppSettings.UserAccessLevel = ualAdmin then begin
    if edMaster.Text <> lblThisWorkStation.caption then begin
      dmDatabase.ExecuteSQL ('Update Computer_Map set Object_Sheet_Folder = ''' +
         appsettings.ObjectSheetFilePath + ''' WHERE Computer_Id = ''' +
         lblThisWorkStation.caption + '''',false);
      dmDatabase.ExecuteSQL ('Update Computer_Map set master = 0 WHERE ' +
         ' EXISTS (SELECT * FROM Computer_Map ' +
         ' WHERE Computer_Id = ''' + lblThisWorkStation.caption  + ''')',false);
      dmDatabase.ExecuteSQL ('Update Computer_Map set master = 1 WHERE Computer_Id = ''' +
          lblThisWorkStation.caption  + '''',false);
    end;

    if edCompetency.text = '' then edcompetency.text := '0';
    sql := 'Update Setting SET [Data] = ''%s'' where [Name] = ''%s''';

    dmDatabase.ExecuteSQL(Format(sql,[edGatewayURL.text,'GatewayURL']),false);
    dmDatabase.ExecuteSQL(Format(sql,[edHelpURL.text,'HelpURL']),false);
    dmDatabase.ExecuteSQL(Format(sql,[edPreLocs.text,'PrefLocs']),false);
    dmDatabase.ExecuteSQL(Format(sql,[edPrefNames.text,'PrefNames']),false);
    dmDatabase.ExecuteSQL(Format(sql,[edSortMethod.text,'SortMethod']),false);
    dmDatabase.ExecuteSQL(Format(sql,[edTaxDesList.text,'TaxDesList']),false);
    dmDatabase.ExecuteSQL(Format(sql,[edTempLicence.text,'TempLic']),false);
    dmDatabase.ExecuteSQL(Format(sql,[edTempNames.text,'TempName']),false);
    dmDatabase.ExecuteSQL(Format(sql,[edCompetency.text,'Competency']),false);
    dmDatabase.ExecuteSQL(Format(sql,[edBlockSize.text,'BlockSize']),false);
    dmDatabase.ExecuteSQL('usp_Setting_Temp_Survey');

    PopulateSettingFields('Data');
  end else
    MessageDlg(Restr_Admin_Permission,mtInformation, [mbOk], 0);

end;

procedure TdlgOptions.edBlockSizeKeyPress(Sender: TObject; var Key: Char);
begin
 if not (Key in [#8, '0'..'9']) then Key := #0;
end;

end.



