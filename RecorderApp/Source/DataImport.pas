//==============================================================================
//  Unit:        DataImport
//
//  Implements:  TdlgDataImport
//
//  Description:
//
//  Author:      John van Breda
//  Created:     8 April 1999
//
//  Changes:     Eric Salmon - 21/03/2002
//               Bug fix in DoDuplicates, the TotalCount is incremented to
//               include all records from imported tables.
//
//  Last Revision Details:
//    $Revision: 81 $
//    $Date: 16/02/09 11:14 $
//    $Author: Andrewkemp $
//
//  $History: DataImport.pas $
//  
//  *****************  Version 81  *****************
//  User: Andrewkemp   Date: 16/02/09   Time: 11:14
//  Updated in $/JNCC/Development/Build/Source
//  VI 18006 (CCN 262) - done
//  Updated to use `GetProgramDataFolder' to determine default paths for
//  folders beneath "User Files".
//  
//  *****************  Version 80  *****************
//  User: Johnvanbreda Date: 6/02/08    Time: 16:30
//  Updated in $/JNCC/Development/Build/Source
//  A bit of tidying and bug fixing
//  
//  *****************  Version 79  *****************
//  User: Rickyshrestha Date: 28/12/07   Time: 10:45
//  Updated in $/JNCC/Development/Build/Source
//  Changed some hardcoded strings to resourcestring
//  
//  *****************  Version 78  *****************
//  User: Rickyshrestha Date: 27/12/07   Time: 17:26
//  Updated in $/JNCC/Development/Build/Source
//  Changed some hardcoded strings to resourcestring
//  
//  *****************  Version 77  *****************
//  User: Rickyshrestha Date: 12/12/07   Time: 15:23
//  Updated in $/JNCC/Development/Build/Source
//  Changes some constant to resourcestring
//  ResStr_FileExists
//    ResStr_NoFilter
//    ResStr_NothingToExport 
//    ResStr_SinceLastExport
//    ResStr_SinceDate
//    ResStr_All
//  
//  *****************  Version 76  *****************
//  User: Ericsalmon   Date: 12/03/04   Time: 13:51
//  Updated in $/JNCC/Development/Build/Source
//  Bug fix.
//  
//  *****************  Version 75  *****************
//  User: Ericsalmon   Date: 1/03/04    Time: 11:18
//  Updated in $/JNCC/Development/Build/Source
//  Development.
//  
//  *****************  Version 74  *****************
//  User: Ericsalmon   Date: 12/02/04   Time: 14:58
//  Updated in $/JNCC/Development/Build/Source
//  Replaced unnecessary TBitBtn with TButton.
//  
//  *****************  Version 73  *****************
//  User: Ericsalmon   Date: 9/02/04    Time: 12:13
//  Updated in $/JNCC/Development/Build/Source
//  Replaced BitBtn with ImageListButton.
//  
//  *****************  Version 72  *****************
//  User: Ericsalmon   Date: 23/05/03   Time: 14:50
//  Updated in $/JNCC/Development/Build/Source
//  IR636 / JNCC611
//  Done. Minor adjustments to code for speed. Still slow on merging
//  individuals, as indexes on Entered_By/Changed_By fields have been
//  discarded.
//  
//  *****************  Version 71  *****************
//  User: Johnvanbreda Date: 20/03/03   Time: 14:12
//  Updated in $/JNCC/Source
//  ID550
//  Changed deletion of temp import file
//  
//  *****************  Version 70  *****************
//  User: Ericsalmon   Date: 20/02/03   Time: 11:16
//  Updated in $/JNCC/Source
//  Updated call to TTempData.Create to include reference to
//  frmMain.SetStatus method, which will be passed on to DBMerger
//  eventually.
//  
//  *****************  Version 69  *****************
//  User: Ericsalmon   Date: 10/02/03   Time: 12:09
//  Updated in $/JNCC/Source
//  Fixes and improvements.
//  
//  *****************  Version 68  *****************
//  User: Ericsalmon   Date: 3/02/03    Time: 14:24
//  Updated in $/JNCC/Source
//  Cleanup.
//  
//  *****************  Version 67  *****************
//  User: Johnvanbreda Date: 31/01/03   Time: 14:16
//  Updated in $/JNCC/Source
//  Duplicate check performance
//  
//  *****************  Version 66  *****************
//  User: Ericsalmon   Date: 31/01/03   Time: 11:20
//  Updated in $/JNCC/Source
//  Additional optimisation.
//  
//  *****************  Version 65  *****************
//  User: Ericsalmon   Date: 29/01/03   Time: 14:36
//  Updated in $/JNCC/Source
//  Improved import, speed and error recovery.
//  
//  *****************  Version 64  *****************
//  User: Johnvanbreda Date: 21/01/03   Time: 16:34
//  Updated in $/JNCC/Source
//  Import security fixes.
//  
//  *****************  Version 63  *****************
//  User: Andrewkemp   Date: 17/01/03   Time: 18:07
//  Updated in $/JNCC/Source
//  IR248: Ensures FTempData is freed after being created in
//  bbAnalyseClick.  This ensures that the temporary database is cleaned
//  up.
//  
//  *****************  Version 62  *****************
//  User: Andrewkemp   Date: 10/01/03   Time: 16:00
//  Updated in $/JNCC/Source
//  IR 68: Import of zip files behaves gracefully if the specified file is
//  not actually a zip file, or if it is a valid zip file which is empty.
//  
//  *****************  Version 61  *****************
//  User: Ericsalmon   Date: 27/12/02   Time: 15:00
//  Updated in $/JNCC/Source
//  
//  *****************  Version 60  *****************
//  User: Ericsalmon   Date: 18/12/02   Time: 16:19
//  Updated in $/JNCC/Source
//  Bug fix.
//  
//  *****************  Version 59  *****************
//  User: Ericsalmon   Date: 18/12/02   Time: 12:47
//  Updated in $/JNCC/Source
//  Bug fixes and code rework for SQL Server.
//  
//  *****************  Version 58  *****************
//  User: Pollyshaw    Date: 6/12/02    Time: 14:16
//  Updated in $/JNCC/Source
//  Changed reference to 'XMLDoc' to 'JNCCXMLDoc'
//  
//  Copyright © Dorset Software Services Ltd, 2001
//
//==============================================================================

unit DataImport;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, Db,
  StdCtrls, Buttons, ExtCtrls, ComCtrls, Grids, JNCCXMLDoc, MergeDataData, DBMerger,
  DBBrowser, Recorder2000_TLB, ExceptionForm, DataClasses, MarkupDocs, VCLUnzip,
  OnlineHelp, GeneralFunctions, GeneralData, ApiUtils, ImageListButton, ADODB,
  DatabaseUtilities, TempData_ADO, Relationships_ADO, DatabaseAccessADO;

type
  EDataImportDialog = class(TExceptionPath);

  TdlgDataImport = class(TForm)
    gbImportType: TGroupBox;
    Label1: TLabel;
    cmbImportType: TComboBox;
    Label2: TLabel;
    eSource: TEdit;
    dlgOpen: TOpenDialog;
    gbImportDetails: TGroupBox;
    lblImportDetails: TLabel;
    mmDetails: TMemo;
    pcValidation: TPageControl;
    tsDuplicateItems: TTabSheet;
    tsInvalidItems: TTabSheet;
    tvDuplicateData: TTreeView;
    tvImportedData: TTreeView;
    tvOriginalData: TTreeView;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    lblInvalidData: TLabel;
    tvInvalidData: TTreeView;
    mmInvalidDetails: TMemo;
    lblInvalidDetails: TLabel;
    reXML: TRichEdit;
    bbCancel: TImageListButton;
    bbAbort: TImageListButton;
    bbAccept: TButton;
    bbReject: TButton;
    bbAllOriginal: TButton;
    bbAllImported: TButton;
    bbAllLatest: TButton;
    bbAnalyse: TButton;
    bbImport: TImageListButton;
    bbImportFrom: TButton;
    procedure bbImportFromClick(Sender: TObject);
    procedure bbAnalyseClick(Sender: TObject);
    procedure bbImportClick(Sender: TObject);
    procedure bbCancelClick(Sender: TObject);
    procedure TreeViewDataCustomDrawItem(Sender: TCustomTreeView;
      Node: TTreeNode; State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure cmbImportTypeChange(Sender: TObject);
    procedure tvDuplicateDataChange(Sender: TObject; Node: TTreeNode);
    procedure tvImportedDataClick(Sender: TObject);
    procedure tvOriginalDataClick(Sender: TObject);
    procedure tvInvalidDataChange(Sender: TObject; Node: TTreeNode);
    procedure bbAcceptClick(Sender: TObject);
    procedure bbRejectClick(Sender: TObject);
    procedure bbAllImportedClick(Sender: TObject);
    procedure bbAllOriginalClick(Sender: TObject);
    procedure bbAllLatestClick(Sender: TObject);
    procedure eSourceChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
    hlpImport: TOnlineHelp;
    FCancelled:boolean;
    FOperationStarted: boolean;
    FImportType: String;
    { Instances of XML objects }
    FTempData: TTempData;
    FXMLDoc: TXMLDoc;
    FMergeData: TdmMergeData;
    FDBImportUtil: TDBBrowserUtility;
    FDBOrigUtil: TDBBrowserUtility;
    FImportIntf: IUnknown;
    FDuplicateCount: integer; // number of duplicate records
    FTotalCount: integer; // total number of records - can't use TempData version as this isn't available from COM
    FUniqueInvalidItemList: TStringList; // allows us to ignore multiple failures on one record
    FMetadataTag: TTag;
    FRejections: TStringList;
    FDuplicates: TStringList;
    FTablesWithFailures: TStringList;
    FMainDbChanged: boolean;
    FUnzippedFiles : TStringList;
    procedure BrowseIntoForeignKey(Sender: TObject;
      iDBUtil: TDBBrowserUtility);
    function BuildDuplicateCheckQuery(const iTableName: string;
      const iPrimaryKey: TPrimaryKey): String;
    function CompareSpecificDateAndReject(const iField: string; iOrigRecord,
      iImportRecord: TStringList; iNode: TTreeNode): boolean;
    procedure DeleteNode(iNode: TTreeNode);
    procedure DisplayInvalidRecords;
    procedure DisplayInvalidTags;
    procedure DisplayValidationPages(iDoDetail: boolean);
    procedure DoCOMImport;
    procedure DoComValidation;
    procedure DoDuplicates;
    procedure DoNBNAnalysis(const AImportType: String);
    procedure GeneralCreation;
    function GetCancelled: Boolean;
    function GetRecordFromNode(iNode: TTreenode): TSingleRecord;
    function GetTagText(const iParentTag, iName: string): string;
    function ImportDatabaseUnzipped(const AZipFile: String): String;
    function ImportDatabaseFromXMLFileName(const AXMLFile: String): String;
    procedure PopulateImportTypeCombo;
    procedure PrepareDatabaseComparison;
    function RejectUsingDateComparison(iOrigRecord,
      iImportRecord: TStringList; iNode: TTreeNode): boolean;
    procedure SetupDetailMemo(iDisplayDescription: boolean);
    procedure StoreItemInList(AList: TStringList; ANode: TTreeNode);
    procedure SetImportButtonState;
    function GetCompletionMessage(const iiRecordsCopied: integer): string;
    procedure RebuildIndexes;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    constructor CreateCheckOnly(AOwner: TComponent; const iPath, iDetails: string);
    destructor Destroy; override;

    // deals with difference between cancelled and aborted
    property Cancelled: boolean read GetCancelled;
    property MainDbChanged: Boolean read FMainDbChanged;
  end;

//==============================================================================
implementation

uses
  Maintbar, ApplicationSettings, ComObj, FormActions, XMLTypes, GenFuncs,
  Constants, DefaultPaths,Registry;

const
  NBN_DATA = 'NBN Data';
  EXTENSION = 'xml';
  NBN_DATABASE = 'NBN Access Database (zipped)';
  NBN_DB_EXT = 'zip';

  NOT_SPECIFIED = 'Not specified';

  CHANGED_DATE = 'CHANGED_DATE';
  ENTRY_DATE = 'ENTRY_DATE';

  INVALID_TAGS = 'Invalid XML tags';

  DETAILS_HEIGHT_FULL = 221;
  DETAILS_HEIGHT_SMALL = 61;

resourcestring
  ResStr_InvalidZipFile = 'The specified ZIP file is not a valid NBN Database Import file.';
  ResStr_InstanceMissing = 'A problem has occurred - the addin is not available.';
  ResStr_TableNotRecord = 'Cannot obtain record information for a table level node.';
  ResStr_RecordNotAvailable = 'The record you are trying to view is not available in the import file.';
  ResStr_SpecifyFile = 'Please specify a file name to export to.';
  ResStr_CompareNotAll = 'Not all records could be compared on the basis of date stamps.';
  ResStr_NoFile = 'The file specified does not exist.  Please specify a valid file.';
  ResStr_AbortQuery = 'Are you sure you want to abort this operation?';

  //Status
  ResStr_ReadingDataFile =  'Reading data file...';
  ResStr_AnalysingDataESC = 'Analysing Data (Press ESC to cancel)...';
  ResStr_CheckingImportedDB = 'Checking imported database...';
  ResStr_ImportingData = 'Importing Data...';
  ResStr_IndentifyDupRecords =  'Identifying duplicate records...';
  ResStr_PerformingExtValidCheck =  'Performing external validation checks...';

  //Captions
  ResStr_BBAnalyse =  '&Analyse';
  ResStr_BBImport = '&Import';


  {$R *.DFM}
//==============================================================================
constructor TdlgDataImport.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  GeneralCreation;
  FOperationStarted := False; // for correct operation of cancel button
  pcValidation.ActivePage := tsDuplicateItems;
  PopulateImportTypeCombo;
  ClientHeight := bbCancel.Top+bbCancel.Height+5;
  bbImport.Visible        :=false;
  bbAbort.Visible         :=false;
  gbImportType.Visible    :=true;
  gbImportDetails.Visible :=false;
  FCancelled              :=false;
  FMainDbChanged          :=false; // flag to determine if work started on main db
  FMergeData := TdmMergeData.Create(Self);
end;  // Create

//==============================================================================
{ Version of constructor which creates the dialog for an existing temporary
    import database }
constructor TdlgDataImport.CreateCheckOnly(AOwner: TComponent; const iPath,
  iDetails: string);
begin
  inherited Create(AOwner);
  GeneralCreation;
  FTempData := TTempData.Create(nil, frmMain.ProgressBar, frmMain.SetStatus, GetCancelled);
  FTempData.ConnectTo( iPath);
  FMergeData := TdmMergeData.Create(Self);
  PrepareDatabaseComparison;
  // put the supplied details into the detail memo
  mmDetails.Lines.Text := iDetails;
  DisplayValidationPages(False);
  frmMain.TaskFinished;
end;  // CreateCheckOnly

//==============================================================================
{ Create code that is called for all constructors }
procedure TdlgDataImport.GeneralCreation;
begin
  { Set the position manually - using the maximum dialog size not the
        initial size to centre it }
  Top := (Screen.Height - Height) div 2;
  Left := (Screen.Width - Width) div 2;

  FRejections := TStringList.Create;
  FRejections.Sorted := true;
  FDuplicates := TStringList.Create;
  FDuplicates.Sorted := true;

  FUniqueInvalidItemList := TStringList.Create;
  FUniqueInvalidItemList.Sorted := True;
  FUniqueInvalidItemList.Duplicates := dupError; // warn us if an item is failing validation multiple times.
  //Help Setup
  hlpImport :=  TonlineHelp.Create(Self.Handle);
  OnHelp := hlpImport.OnHelpReplacement;
  HelpContext := IDH_IMPORTDATA;
  FUnzippedFiles := TStringList.Create;  // keep track of files we unzip so we can cleanup
end;  // GeneralCreation

//==============================================================================
{ Destructor just cleans up the data module }
destructor TdlgDataImport.Destroy;
var i: integer;
    lCursor: TCursor;
begin
  { Free up the validation messages first }
  lCursor := HourglassCursor;
  try
    with tvInvalidData do
      for i := 0 to Items.Count-1 do
        if Items[i].Data <> nil then
          if TObject(Items[i].Data) is TMessage then
            TMessage(Items[i].Data).Free;
  finally
    DefaultCursor(lCursor);
  end;
  FTempData.Free;
  FMergeData.Free;
  FRejections.Free;
  FDuplicates.Free;
  FUniqueInvalidItemList.Free;
  FXMLDoc.Free;
  FDBImportUtil.Free;
  FDBOrigUtil.Free;
  { Ensure working directory won't cause dll load problems }
  if GetDriveType(PChar(GetCurrentDir))=DRIVE_REMOVABLE then
    SetCurrentDir(ExtractFilePath(Application.Exename));
  hlpImport.Free;
  // cleanup any files we unzipped
  for i := 0 to FUnzippedFiles.Count-1 do 
    if FileExists(FUnzippedFiles[i]) then
      DeleteFile(FUnzippedFiles[i]);
  FUnzippedFiles.Free;
  inherited Destroy;
end;  // Destroy

//==============================================================================
function TdlgDataImport.GetCancelled: Boolean;
begin
  Result := FCancelled;
end;  // GetCancelled

//==============================================================================
procedure TdlgDataImport.bbImportFromClick(Sender: TObject);
begin
  if dlgOpen.Execute then eSource.Text:=dlgOpen.FileName;
end;  // sbImportFromClick

//==============================================================================
procedure TdlgDataImport.bbAnalyseClick(Sender: TObject);
var lCursor:TCursor;
begin
  if not FileExists(eSource.Text) then
    raise EDataImportDialog.CreateValidation(ResStr_NoFile, eSource);
  FOperationStarted := True;
  bbAnalyse.Enabled := False;
  bbImport.Enabled := False;
  lCursor := HourglassCursor;
  try
    if eSource.Text = '' then
      MessageDlg(ResStr_SpecifyFile, mtInformation, [mbOk], 0)
    else
      try
        frmMain.SetStatus(ResStr_ReadingDataFile);
        if (cmbImportType.Text = NBN_DATA) or (cmbImportType.Text = NBN_DATABASE) then
          DoNBNAnalysis(cmbImportType.Text)
        else
          DoComImport;
      finally
        frmMain.TaskFinished;
        FOperationStarted := False;
      end; //try..finally
  finally
    DefaultCursor(lCursor);
  end; // try..finally
end;  // bbAnalyseClick

//==============================================================================
{ Actually performs the analysis of an import file for NBN data }
procedure TdlgDataImport.DoNBNAnalysis(const AImportType: String);
var
  lOverallTaskIndex, lTaskIndex, lSubTaskIndex: integer;
  lCursor   : TCursor;
  lTagToProcess: TTag;
  ltfFinished: boolean;
  lstDatabaseName: String;
begin
  frmMain.SetStatus(ResStr_AnalysingDataESC);
  frmMain.ProgressBar.TaskPosition := 0;
  lCursor := HourglassCursor;
  FImportType := AImportType;
  try
    try
      // Importing from zipped Access database
      if AImportType = NBN_DATABASE then begin
        lstDatabaseName := ImportDatabaseUnzipped(eSource.Text);
        if lstDatabaseName <> '' then begin
          FTempData := TTempData.Create(nil, frmMain.ProgressBar, frmMain.SetStatus, GetCancelled);
          FTempData.ConnectTo(lstDatabaseName);
          PrepareDatabaseComparison;
        end else begin
          FCancelled := true;
        end;
      end else
      if AImportType = NBN_DATA then begin
        FXMLDoc := TXMLDoc.Create(eSource.Text, nil, frmMain.ProgressBar);
        try
          if FCancelled then Exit;
          lstDatabaseName := ImportDatabaseFromXMLFileName(eSource.Text);
          FTempData := TTempData.Create(FXMLDoc, frmMain.ProgressBar, frmMain.SetStatus, GetCancelled);
          FTempData.ConnectTo(lstDatabaseName);
          if FCancelled then Exit;
          ltfFinished := False;
          lOverallTaskIndex := frmMain.ProgressBar.EmbedTask(0,90);
          { Loop until all survey events and entire document are read into db }
          repeat
            lTaskIndex := frmMain.ProgressBar.EmbedTask(FXMLDoc.PercentStart, FXMLDoc.PercentEnd);
            lSubTaskIndex := frmMain.ProgressBar.EmbedTask(0,25);
            lTagToProcess := FXMLDoc.ProcessDocument(True);
            if FCancelled then Exit;
            { Note, if whole document returned we can ignore tags unless inside content }
            if lTagToProcess.Name = 'nbndata' then
            begin
              lTagToProcess := FXMLDoc.GetTagByName(CONTENT_TAG);
              ltfFinished := true;
            end;
            lSubTaskIndex := frmMain.ProgressBar.FinishAndEmbedNext(lSubTaskIndex, 25,100);
            FTempData.ProcessDocumentChunk(lTagToProcess);
            FXMLDoc.FreeTagTree(lTagToProcess);
            frmMain.ProgressBar.FinishTask(lSubTaskIndex);
            frmMain.ProgressBar.FinishTask(lTaskIndex);
          until  ltfFinished;
          lOverallTaskIndex := frmMain.ProgressBar.FinishAndEmbedNext(lOverallTaskIndex, 90, 92);
          FXMLDoc.CheckStack;
          FMetaDataTag := FXMLDoc.GetTagByName('metadata');
          FTempData.FinaliseProcessing;
          frmMain.SetStatus(ResStr_CheckingImportedDB);
          lOverallTaskIndex := frmMain.ProgressBar.FinishAndEmbedNext(lOverallTaskIndex, 92, 100);
          PrepareDatabaseComparison;
          frmMain.ProgressBar.FinishTask(lOverallTaskIndex);
        except
          on Exception do // any problems, free the XML Doc
          begin
            FXMLDoc.Free;
            FXMLDoc := nil;
            bbAnalyse.Enabled := True;
            raise;
          end;
        end;
      end;  // if NBN_DATA
    finally
      frmMain.TaskFinished;
      DefaultCursor(lCursor);
    end; // try..finally
  except
    on TMarkupException do
      FCancelled := True; // nothing - this sort already handled
    on ECancellation do
      FCancelled := True;
  end; // try..except
  if not FCancelled then
    DisplayValidationPages(True)
  else // import was cancelled
  begin
    FCancelled:=false;
    FXMLDoc.Free;
    FXMLDoc := nil;
    bbAnalyse.Enabled := True;
  end;
end;  // bbAnalyseClick

//==============================================================================
{ Enable/disable relative buttons, and show the bottom half of the form for
    user controlled validation once the import file is analysed }
procedure TdlgDataImport.DisplayValidationPages(iDoDetail: boolean);
begin
  bbAnalyse.Visible:=False;
  bbCancel.Visible :=False;

  // Expand form height
  ClientHeight := bbAbort.Top+bbAbort.Height+5;

  bbImport.Visible:=true;
  bbAbort.Visible :=true;
  SetupDetailMemo(iDoDetail);
  // Show the validation tabs
  pcValidation.Visible:=true;

  gbImportType.Visible   :=false;
  gbImportDetails.Visible:=true;
end;  // DisplayValidationPages

//==============================================================================
{ Populate the memo at the top of the dialog with metadata regarding the
     import file. }
procedure TdlgDataImport.SetupDetailMemo(iDisplayDescription: boolean);
var lstRestrictionText: String;
begin
  with mmDetails.Lines do begin
    if iDisplayDescription then
      if cmbImportType.Text = NBN_DATA then begin
        Add('From:'#9#9 + GetTagText('dataset_source','dataset_owner') + #9#9 +
            'Date Exported: ' + GetTagText('dataset_source','date_exported'));
        Add('Dataset:'#9#9 + GetTagText('dataset_source','dataset_name'));
        Add('Title: '#9#9 + GetTagText('metadata','dataset_title'));
        lstRestrictionText := GetTagText('dataset_source','dataset_restrictions');
        if lstRestrictionText <> NOT_SPECIFIED then
          Add('Restrictions: '#9 + lstRestrictionText);
      end // if iDisplayDescription
      else if cmbImportType.Text = NBN_DATABASE then begin
        Add('Date Exported: ' + DateToStr(FileDateToDateTime(FileAge(FTempData.DatabasePath))));
        Add('Title: ' + ExtractFileName(Copy(FTempData.DatabasePath, 1, Length(FTempData.DatabasePath) - 4)));
      end;
    Add('');
    Add(GetPlural(FTotalCount - FUniqueInvalidItemList.Count, 'Item') + ' successfully read, of which there are:');
    Add('    ' + GetPlural(FTotalCount - FDuplicateCount - FUniqueInvalidItemList.Count, 'New item') + '.');
    Add('    ' + GetPlural(FDuplicateCount, 'Duplicated item') + '.');
    Add(GetPlural(FUniqueInvalidItemList.Count + FTempData.InvalidTagList.Count,'Invalid item') + '.');
  end;
end;  // SetupDetailMemo

//==============================================================================
{ Function to locate text in the parsed xml document, from a particular tag
    within the parent tag specified.  Parent tag should be a uniquely
    identifiable tag such as metadata.  }
function TdlgDataImport.GetTagText(const iParentTag, iName: string): string;
var
  i: integer;
  lTag: TTag;
begin
  Result := NOT_SPECIFIED;
  try
    lTag := FXMLDoc.GetTagByName(iParentTag);
    for i := 0 to lTag.NestedTags.Count-1 do
    begin
      if TTag(lTag.NestedTags[i]).Name = iName then
      begin
        Result := TTag(lTag.NestedTags[i]).Text;
        break; // from loop - found a result
      end;
    end;
  except on EDTDNotComplete do
    ; // nothing - return not specified
  end; // try
end;  // GetTagText

//==============================================================================
procedure TdlgDataImport.DoCOMImport;
begin
  if FImportIntf = nil then
    raise EDataImportDialog.Create(ResStr_InstanceMissing);
  if (FImportIntf as IImportFilter).ImportFile(eSource.Text) then
    ModalResult := mrOk;
end;  // DoCOMImport

//==============================================================================
procedure TdlgDataImport.bbImportClick(Sender: TObject);
var
  lRecordsCopied: integer;
  lCursor       : TCursor;
begin
  lCursor := HourglassCursor;
  FMainDbChanged   := True; // have started to change main db
  bbImport.Enabled := False; // prevent accidental double-click
  FCancelled       := False;
  try
    frmMain.SetStatus(ResStr_ImportingData);
    frmMain.ProgressBar.TaskPosition := 0;
    lRecordsCopied := FTempData.CopyRecordsIntoMainDB(FDuplicates, FRejections);
    // Check tables with preferred records (eg location names) have 1 and only 1 preferred
    dmGeneralData.CheckPreferred;
    { Ensure sample type list correct }
    dmFormActions.UpdateSampleTypeList;
  finally
    DefaultCursor(lCursor);
    frmMain.TaskFinished;
  end;
  if not FCancelled then
  begin
    // hide the validation tabs for next time in.
    MessageDlg(GetCompletionMessage(lRecordsCopied), mtInformation, [mbOk], 0);
    Close;
  end;
  RebuildIndexes;
  ModalResult := mrOK;
end;  // bbImportClick

//==============================================================================
{ Description: Return a message indicating records imported/rejected on
     completion of the import
  Created: 26/09/2002 }
function TdlgDataImport.GetCompletionMessage(const iiRecordsCopied: integer): string;
var
  liRejectCount: integer;
begin
  liRejectCount := FTotalCount + FTempData.InvalidTagList.Count - iiRecordsCopied;
  Result := GetPlural(iiRecordsCopied, 'item') + ' successfully imported' + #10 +
            GetPlural(liRejectCount, 'item') + ' rejected';
  // if any rejects from recorder 2000, inform user where the file is
  if liRejectCount - FRejections.Count > 0 then
    Result := Result + #10#10 + 'Items rejected by ' + Application.Title +
           ' are detailed in the file ' +
           GetProgramDataFolder(PATH_USER_FILES) +
           IMPORT_REJECTS_FILE;
end;

//==============================================================================
{ Cancel or escape key pressed - if operation started then must query for
     confirmation }
procedure TdlgDataImport.bbCancelClick(Sender: TObject);
begin
  if FOperationStarted then
    if MessageDlg(ResStr_AbortQuery, mtConfirmation, [mbYes, mbNo], 0) = mrYes then
      FCancelled:=true
    else
      { Stop the dialog from closing }
      ModalResult := mrNone;
end;  // bbCancelClick

//==============================================================================
{ Draw table level nodes in bold in the duplicate items treeview }
procedure TdlgDataImport.TreeViewDataCustomDrawItem(Sender: TCustomTreeView;
  Node: TTreeNode; State: TCustomDrawState; var DefaultDraw: Boolean);
begin
  with Sender.Canvas do begin
    if (Node.Level=0) then Font.Style:=[fsBold];
    if cdsSelected in State then begin
      Font.Color :=clHighlightText;
      Brush.Color:=clHighlight;
    end;
    DefaultDraw := true;
  end;
end;  // TreeViewCustomDrawItem

//==============================================================================
procedure TdlgDataImport.PrepareDatabaseComparison;
begin
  tvDuplicateData.Items.BeginUpdate; // for performance
  FDuplicateCount := 0; // Initialise
  FTotalCount     := 0;
  FDBImportUtil := TDBBrowserUtility.Create(tvImportedData, FTempData.Database);
  FDBOrigUtil := TDBBrowserUtility.Create(tvOriginalData, dmDatabase.dbLocal);
  
  FTablesWithFailures := TStringList.Create;
  try
    { Initiate validation through any COM addins }
    DoComValidation;
    { Display all internal invalid records and tags }
    DisplayInvalidRecords;
    DisplayInvalidTags;
    { Check for duplicates in valid records }
    DoDuplicates;
  finally
    FTablesWithFailures.Free;
    tvDuplicateData.Items.EndUpdate;
  end; // try..finally
  SetImportButtonState;
end;  // PrepareDatabaseComparison

//==============================================================================
{ Check each table - go through fields in import database and check if
         exists in the main DB }
procedure TdlgDataImport.DoDuplicates;
var i               : integer;
    lTableNode      : TTreenode;
    lTableAdded     : boolean;
    lPrimaryKey     : TPrimaryKey;
    lKeyDisplayString: string;
    lqryCountAndDup : TADOQuery;
    lNotRejected    : Boolean;
begin
  frmMain.SetStatus(ResStr_IndentifyDupRecords);
  lTableNode := nil; // initialise
  lqryCountAndDup := TADOQuery.Create(nil);
  try
    lqryCountAndDup.Connection := FTempData.Database;
    for i := 0 to FTempData.TablesCreated.Count-1 do
    begin
      frmMain.ProgressBar.TaskPosition := i * 100 div FTempData.TablesCreated.Count;
      // Get proper TotalCount by added all record from currently processed table
      with lqryCountAndDup do begin
        SQL.Text := 'SELECT Count(*) FROM ' + FTempData.TablesCreated[i];
        Open;
        try
          Inc(FTotalCount, Fields[0].AsInteger);
        finally
          Close;
        end;
      end;
      // Now, find out the duplicates
      lPrimaryKey := dmDatabase.SplitPrimaryKey(FTempData.TablesCreated[i]);
      dmDatabase.CreateLinkedTable(FTempData.Database, FTempData.TablesCreated[i], 'SQLSERVER_' + FTempData.TablesCreated[i]);
      lqryCountAndDup.SQL.Text := BuildDuplicateCheckQuery(FTempData.TablesCreated[i], lPrimaryKey);
      with lqryCountAndDup do begin
        Open;
        try
          lTableAdded := False;
          while not Eof do begin
            { If 2 field key then need to do extra check }
            if lPrimaryKey.Key2 = '' then
              lNotRejected := FRejections.IndexOf(Uppercase(FTempData.TablesCreated[i]) + ';' +
                                                  FieldByName(lPrimaryKey.Key1).AsString + ';') = -1
            else
              lNotRejected := FRejections.IndexOf(Uppercase(FTempData.TablesCreated[i]) + ';' +
                                                  FieldByName(lPrimaryKey.Key1).AsString + ';' +
                                                  FieldByName(lPrimaryKey.Key2).AsString) = -1;
            if lNotRejected then
            begin
              { Store the record which is a duplicate }
              if not lTableAdded then
              begin
                lTablenode := tvDuplicateData.Items.Add(nil, Uppercase(FTempData.TablesCreated[i]));
                lTableAdded := True;
              end;
              if lPrimaryKey.Key2 = '' then
                lKeyDisplayString := FieldByName(lPrimaryKey.Key1).AsString
              else
                lKeyDisplayString := FieldByName(lPrimaryKey.Key1).AsString + ', ' +
                                     FieldByName(lPrimaryKey.Key2).AsString;
              tvDuplicateData.Items.AddChild(lTableNode, lKeyDisplayString);
              Inc(FDuplicateCount);
            end; // not already rejected
            Next;
          end; // while
        finally
          Close;
        end; // try
      end; // with
      dmDatabase.RemoveLinkedTable(FTempData.Database, 'SQLSERVER_' + FTempData.TablesCreated[i]);
    end; // for all FTempData.TablesCreated
  finally
    lqryCountAndDup.Free;
  end;
end;  // DoDuplicates

//==============================================================================
{ BuildDuplicateCheckQuery prepares qryAllPurpose to scan for duplicates on
     a given table.  NOTE, ONLY THE FIRST PART OF THE PRIMARY KEY IS USED
     IN THE JOIN IF THE KEY HAS 2 FIELDS }
function TdlgDataImport.BuildDuplicateCheckQuery(const iTableName: string;
  const iPrimaryKey: TPrimaryKey): String;
begin
  { Add where clause to find only duplicate records - only use first part of
     key in join if key has 2 fields otherwise the query takes ages}
  Result := 'SELECT DISTINCT Import.' + iPrimaryKey.Key1;
  if iPrimaryKey.Key2 <> '' then
    Result := Result + ', Import.' + iPrimaryKey.Key2;
  Result := Result +
            Format(' FROM %s AS Import INNER JOIN SQLSERVER_%s AS Main ON Import.%s = Main.%s',
                   [iTableName, iTableName, iPrimaryKey.Key1, iPrimaryKey.Key1]);
  if iPrimaryKey.Key2 <> '' then
    Result := Result + Format(' WHERE Import.%s = Main.%s',
                       [iPrimaryKey.Key2, iPrimaryKey.Key2]);
end;  // BuildDuplicateCheckQuery

//==============================================================================
{ Perform any validation on the import database as required for addins }
procedure TdlgDataImport.DoComValidation;
var
  lAddinIndex, lFailureIndex: integer;
  lValidationIntf: IValidation;
  lGuid: TGUID;
  lKeyList: TEditableKeyList;
  lItemTable: string;
  lTableIndex: integer;
  lParentNode: TTreenode;
  lMessage: TMessage;
  lInvalidCount: integer;
begin
  frmMain.SetStatus(ResStr_PerformingExtValidCheck);
  { Loop through each addin }
  for lAddinIndex := 0 to AppSettings.ComAddins.Validators.Count-1 do
  begin
    lGuid := StringToGuid(AppSettings.ComAddins.Validators[lAddinIndex]);
    lValidationIntf := CreateComObject(lGuid) as IValidation;
    lInvalidCount := lValidationIntf.Validate(FTempData.DatabasePath);
    if lInvalidCount > 0 then
    begin
      lKeyList := TEditableKeyList.Create(lValidationIntf.KeyList);
      for lFailureIndex := 0 to lKeyList.Header.ItemCount-1 do
      begin
        if lKeyList.Header.TableName = MIXED_DATA then
          lItemTable := lKeyList.Items[lFailureIndex].KeyField2
        else
          lItemTable := lKeyList.Header.TableName;
        try
          FUniqueInvalidItemList.Add(lItemTable + '=' + lKeyList.Items[lFailureIndex].KeyField1);
          lTableIndex := FTablesWithFailures.IndexOf(lItemTable);
          if lTableIndex=-1 then // need new top-level node
          begin
            lParentNode := tvInvalidData.Items.Add(nil, lItemTable);
            FTablesWithFailures.AddObject(lItemTable, lParentNode);
          end
          else // add to existing node
            lParentNode := TTreeNode(FTablesWithFailures.Objects[lTableIndex]);
          lMessage := TMessage.Create;
          lMessage.Msg := lValidationIntf.ErrorString[lFailureIndex];
          tvInvalidData.Items.AddChildObject(lParentNode,
                                             lKeyList.Items[lFailureIndex].KeyField1,
                                             lMessage);

          { Now remember not to import the record }
          FRejections.Add(Uppercase(lItemTable) + ';' +
                          lKeyList.Items[lFailureIndex].KeyField1 + ';' +
                          lKeyList.Items[lFailureIndex].KeyField2);
        except
          on EStringListError do ; // nothing - record already listed as invalid
        end; // try
      end;
    end; // for lFailureIndex
  end; // for lAddinIndex
end;  // DoComValidation

//==============================================================================
{ Set the file open dialog up correctly according to the current import
     type }
procedure TdlgDataImport.cmbImportTypeChange(Sender: TObject);
var
  lstGuid: string;
  lstExtension: string;
  liItemIndex: Integer;
begin
  liItemIndex:= cmbImportType.ItemIndex;
  if liItemIndex <> -1 then // just for safety
  begin
    if (cmbImportType.Text = NBN_DATA) then // NBN_DATA
    begin
      dlgOpen.DefaultExt := EXTENSION;
      { Add an export filter and an All files filter }
      dlgOpen.Filter := NBN_DATA + '|' + '*.' + EXTENSION + '|All files|*.*';
      bbAnalyse.Caption := ResStr_BBAnalyse;
      FImportIntf := nil;
    end else
    if  (cmbImportType.Text = NBN_DATABASE) then // NBN_DATABASE
    begin
      dlgOpen.DefaultExt := NBN_DB_EXT;
      { Add an export filter and an All files filter }
      dlgOpen.Filter := NBN_DATABASE + '|*.' + NBN_DB_EXT + '|All files|*.*';
      bbAnalyse.Caption := ResStr_BBAnalyse;
      FImportIntf := nil;
    end else
    begin // COM
      { Read GUID by index so no probs if duplicate names }
      with AppSettings.ComAddins.ImportFilters do
        lstGuid := Values[cmbImportType.Text];
      FImportIntf := CreateComObject(StringToGUID(lstGuid));
      lstExtension := (FImportIntf as IFilter).DefaultFileExtension;
      dlgOpen.DefaultExt := lstExtension;
      dlgOpen.Filter := (FImportIntf as IRecorderAddin).Name + '|' +
                         '*.' + lstExtension + '|All files|*.*';
      bbAnalyse.Caption := ResStr_BBImport;
    end;
  end;
end;  // cmbImportTypeChange

//==============================================================================
{ Add NBN Data and any COM import types to the combo box }
procedure TdlgDataImport.PopulateImportTypeCombo;
var
  i: integer;
begin
  with cmbImportType do begin
    { First add the NBNData import type }
    Items.Add(NBN_DATA);
    Items.Add(NBN_DATABASE);
    ItemIndex :=0;
    { Now do any COM filters }
    with AppSettings.ComAddins.ImportFilters do
      for i := 0 to Count-1 do
        Items.Add(Names[i]);
  end;
  { Now ensure the file dialog is set up }
  cmbImportTypeChange(Self);
end;  // PopulateImportTypeCombo

//==============================================================================
{ Populate the two comparison treeviews according to the item selected in the
    duplicate items treeview }
procedure TdlgDataImport.tvDuplicateDataChange(Sender: TObject;
  Node: TTreeNode);
var
  lKeyList: TEditableKeyList;
  lCommaPos: integer;
  lCursor  : TCursor;
begin
  lCursor := HourglassCursor;
  tvImportedData.Items.BeginUpdate;
  tvOriginalData.Items.BeginUpdate;
  try
    tvImportedData.Items.Clear;
    tvOriginalData.Items.Clear;
    { Only change if a record node is selected, not a table node }
    if tvDuplicateData.Selected.Parent <> nil then
    begin
      { Create a key list for the item we wan't to compare - the same list applies
           to both databases }
      lKeyList := TEditableKeyList.Create;
      try
        lKeyList.SetTable(tvDuplicateData.Selected.Parent.Text);
        { Get the key values, allowing for join table cases }
        lCommaPos := Pos(', ', tvDuplicateData.Selected.Text);
        if lCommaPos = 0 then
          lKeyList.AddItem(tvDuplicateData.Selected.Text, '')
        else
          lKeyList.AddItem(Copy(tvDuplicateData.Selected.Text, 1, lCommaPos-1),
                           Copy(tvDuplicateData.Selected.Text, lCommaPos+2, 255));
        FDBImportUtil.SetTreeViewItem(lKeyList);
        FDBOrigUtil.SetTreeViewItem(lKeyList);
      finally
        lKeyList.Free;
      end;
    end; // if
    SetImportButtonState;
  finally // reset cursor and allow update of treeviews
    tvOriginalData.Items.EndUpdate;
    tvImportedData.Items.EndUpdate;
    DefaultCursor(lCursor);
  end; // try..finally
end;  // tvDuplicateDataChange

//==============================================================================
{ When clicking the comparison treeview items, populate any foreign keys with
     the detail record data }
procedure TdlgDataImport.tvImportedDataClick(Sender: TObject);
begin
  try
    BrowseIntoForeignKey(Sender, FDBImportUtil);
  except
    on Exception do
      raise EDataImportDialog.CreateNonCritical(ResStr_RecordNotAvailable);
  end;
end;  // tvImportedDataClick

//==============================================================================
{ When clicking the comparison treeview items, populate any foreign keys with
     the detail record data }
procedure TdlgDataImport.tvOriginalDataClick(Sender: TObject);
begin
  { Don't handle exceptions here - they really are a problem with the nbndata.mdb! }
  BrowseIntoForeignKey(Sender, FDBOrigUtil);
end;  // tvOriginalDataClick

//==============================================================================
{ Actually do the work of populating a foreign key node with data from the
    detail record }
procedure TdlgDataImport.BrowseIntoForeignKey(Sender: TObject;
                                               iDBUtil: TDBBrowserUtility);
var
  lRelIndex: integer;
begin
  with TTreeView(Sender) do
    if Selected <> nil then
      if (Selected.Data <> nil) and (not Selected.HasChildren) then
      begin
        { Find any relationships from the node }
        lRelIndex:= dmDatabase.Relationships.FindRelationship(
                           TRecordDataNode(Selected.Data).FTableName,
                           TRecordDataNode(Selected.Data).FFieldName);
        if lRelIndex <> NO_RELATIONSHIP then
          { Populate the node with the record identified by the relationhip }
          iDBUtil.PopulateRelationship(lRelIndex, Selected);
      end; // if..Data<>nil
end;  // BrowseIntoForeignKey

//==============================================================================
{ Set the validation message as appropriate }
procedure TdlgDataImport.tvInvalidDataChange(Sender: TObject;
  Node: TTreeNode);
begin
  mmInvalidDetails.Clear;
  mmInvalidDetails.Height := DETAILS_HEIGHT_FULL;
  reXML.Visible := False;
  if Node.Data<>nil then
    if TObject(Node.Data) is TMessage then
    begin
      mmInvalidDetails.Lines.Add(TMessage(Node.Data).Msg);
    end
    else if TObject(Node.Data) is TTag then
    begin
      mmInvalidDetails.Height := DETAILS_HEIGHT_SMALL;
      reXML.Clear;
      reXML.Visible := True;
      mmInvalidDetails.Lines.Add(TTag(Node.Data).InvalidationText);
      FXMLDoc.PopulateErrorReport(reXML, TTag(Node.Data).DocRow, TTag(Node.Data).DocBlockPos);
    end;
end;  // tvInvalidDataChange

//==============================================================================
{ Stores the record identified by a node in tvDuplicateData in the list
    supplied, either rejection or duplicate list }
procedure TdlgDataImport.StoreItemInList(AList: TStringList; ANode: TTreeNode);
var lNode  : TTreeNode;
    lCursor: TCursor;
  //----------------------------------------------------------------------------
  procedure AddItemToList(ASingleRecord: TSingleRecord);
  begin
    { Trim down the memory usage as we go as it can get very large if lots of rejected items }
    if AList.Count mod 300 = 0 then
      if Win32Platform = VER_PLATFORM_WIN32_NT then
        SetProcessWorkingSetSize(GetCurrentProcess, $FFFFFFFF, $FFFFFFFF);
    AList.Add(Uppercase(ASingleRecord.TableName) + ';' +
              ASingleRecord.Key1 + ';' + ASingleRecord.Key2);
  end;
  //----------------------------------------------------------------------------
begin
  if ANode <> nil then begin
    lCursor := HourglassCursor;
    try
      if ANode.Count = 0 then
        // No children, one record node to process
        AddItemToList(GetRecordFromNode(ANode))
      else begin
        // Table node, several record nodes to process
        AList.Capacity := AList.Capacity + ANode.Count;
        lNode := ANode.GetFirstChild;
        while lNode <> nil do begin
          AddItemToList(GetRecordFromNode(lNode));
          lNode := lNode.GetNextSibling;
        end;
      end;
    finally
      DefaultCursor(lCursor);
    end;
  end;
end;  // StoreItemInList

//==============================================================================
{ Handle the acceptance of a single invalid item.  All we have to do is remove
    it from the hierarchy }
procedure TdlgDataImport.bbAcceptClick(Sender: TObject);
begin
  StoreItemInList(FDuplicates, tvDuplicateData.Selected);
  DeleteNode(tvDuplicateData.Selected);
  SetImportButtonState;
end;  // bbAcceptClick

//==============================================================================
{ Like acceptance, but we must record the rejection so we know to skip it
     during the import }
procedure TdlgDataImport.bbRejectClick(Sender: TObject);
//var lCursor: TCursor;
//    lNode  : TTreeNode;
begin
  StoreItemInList(FRejections, tvDuplicateData.Selected);
  DeleteNode(tvDuplicateData.Selected);
  SetImportButtonState;
end;  // bbRejectClick

//==============================================================================
{ Reads the information required to identify a record in the DB from a single
   node in tcDuplicateData }
function TdlgDataImport.GetRecordFromNode(iNode: TTreenode): TSingleRecord;
var
  lKeyText: string;
begin
  if iNode.Parent = nil then
    raise EDataImportDialog.Create(ResStr_TableNotRecord);
  lKeyText := iNode.Text;
  if Pos(', ', lKeyText) = 0 then
  begin
    Result.Key1 := lKeyText;
    Result.Key2 := '';
  end else
  begin // split join key, so extract each keystring
    Result.Key1 := Copy(lKeyText, 1, Pos(', ', lKeyText)-1); // up to ', '
    Result.Key2 := Copy(lKeyText, Pos(', ', lKeyText)+2, 255); // skip ', ', then rest of string
  end;
  Result.TableName := iNode.Parent.Text;
end;  // GetRecordFromNode

//==============================================================================
{ Removes the node from the duplicate items tree view.  Removes the
    parent as well if it has no children }
procedure TdlgDataImport.DeleteNode(iNode: TTreeNode);
var
  lParent: TTreenode;
  lParentDeleted: boolean;
begin
  tvDuplicateData.Items.BeginUpdate;
  try
    lParentDeleted := False;
    if iNode <> nil then
    begin
      lParent := iNode.Parent;
      { Delete the parent if this is the last child }
      if lParent <> nil then
        if lParent.Count = 1 then
        begin
          tvDuplicateData.Items.Delete(lParent);
          lParentDeleted := True;
        end;
      { If we haven't deleted the whole lot, then delete the item }
      if not lParentDeleted then
        tvDuplicateData.Items.Delete(iNode);
    end;
    tvImportedData.Items.Clear;
    tvOriginalData.Items.Clear;
  finally
    tvDuplicateData.Items.EndUpdate;
  end;
end;  // DeleteNode

//==============================================================================
{ To accept all duplicate items, just clear the treeview without adding any
    items to the rejection list.  This does not affect any records already
    rejected. }
procedure TdlgDataImport.bbAllImportedClick(Sender: TObject);
var lCursor: TCursor;
begin
  lCursor := HourglassCursor;
  tvDuplicateData.Items.BeginUpdate;
  try
    with tvDuplicateData do begin
      Selected := Items.GetFirstNode;   // Get first node in tree
      while Selected <> nil do begin
        bbAcceptClick(nil);             // Accept node, and delete it from tree
        Selected := Items.GetFirstNode; // Get next first node, previous has been removed
      end;
    end;
  finally
    tvDuplicateData.Items.EndUpdate;
    DefaultCursor(lCursor);
  end;
end;  // bbAllImportedClick

//==============================================================================
{ To reject remaining duplicate items, clear the treeview and add any
    items to the rejection list.  This does not affect any records already
    rejected. }
procedure TdlgDataImport.bbAllOriginalClick(Sender: TObject);
var lCursor: TCursor;
begin
  lCursor := HourglassCursor;
  tvDuplicateData.Items.BeginUpdate;
  try
    { Loop through tree view, rejecting the record level nodes }
    with tvDuplicateData do begin
      Selected := Items.GetFirstNode;   // Get first node in tree
      while Selected <> nil do begin
        bbRejectClick(nil);             // Reject node and delete it from tree
        Selected := Items.GetFirstNode; // Get next first node, previous has been removed
      end;
    end;
  finally
    tvDuplicateData.Items.EndUpdate;
    DefaultCursor(lCursor);
  end;
end;  // bbAllOriginalClick

//==============================================================================
{ This is the tricky one.  We must check each item against the original record,
    and only reject it if it is older.  Use changed_date to test if it exists,
    otherwise use entry_date.  If neither exists, don't delete the item from the
    tree view }
procedure TdlgDataImport.bbAllLatestClick(Sender: TObject);
var
  lOrigRecord  : TStringList;
  lImportRecord: TStringList;
  lSingleRecord: TSingleRecord;
  lCursor      : TCursor;
  lParentNode, lRecordNode, lDelNode: TTreeNode;
begin
  lCursor := HourglassCursor;
  lOrigRecord := TStringList.Create;
  lImportRecord := TStringList.Create;
  tvDuplicateData.Items.BeginUpdate;
  try
    lParentNode := tvDuplicateData.Items.GetFirstNode;
    if lParentNode <> nil then
      // Process all tables, "parent" nodes
      while lParentNode <> nil do begin
        lRecordNode := lParentNode.GetFirstChild;
        // Need to select next parent now, as "RejectUsingDateComparison" might
        // cause it to be deleted.
        lParentNode := lParentNode.GetNextSibling;
        // Process all records for the table
        while lRecordNode <> nil do begin
          { Read the table name and key values }
          lSingleRecord := GetRecordFromNode(lRecordNode);
          // Get next node now, in case current is deleted in next function
          lDelNode := lRecordNode;
          lRecordNode := lRecordNode.GetNextSibling;

          lOrigRecord.Clear;
          { Locate the main db record }
          dmGeneralData.GetRecordStrings(lOrigRecord, lSingleRecord.TableName,
                                         lSingleRecord.Key1, lSingleRecord.Key2);
          dmGeneralData.qryRecordStrings.Connection := FTempData.Database;
          try
            lImportRecord.Clear;
            { Locate the import db record }
            dmGeneralData.GetRecordStrings(lImportRecord, lSingleRecord.TableName,
                                           lSingleRecord.Key1, lSingleRecord.Key2);
          finally // for safety, ensure we switch back to the main db
            dmDatabase.SetDatabaseLocal([dmGeneralData.qryRecordStrings]);
          end;
          { If a node is deleted, we don't need to advance to the next item,
             otherwise we must reindex the hierarchy }
          RejectUsingDateComparison(lOrigRecord, lImportRecord, lDelNode);
        end;
      end;
  finally
    lOrigRecord.Free;
    lImportRecord.Free;
    DefaultCursor(lCursor);
    tvDuplicateData.Items.EndUpdate;
  end;
  if tvDuplicateData.Items.Count > 0 then
    MessageDlg(ResStr_CompareNotAll, mtInformation, [mbOk], 0);
  SetImportButtonState;
end;  // bbAllLatestClick

//==============================================================================
{ Rejects an import based on date time stamps and comparison with an existing
    record. If changed date or entry date present, then we can make our
    comparison, else we must ignore the node.  Returns true if a succesful
    comparison was achieved }
function TdlgDataImport.RejectUsingDateComparison(iOrigRecord,
  iImportRecord: TStringList; iNode: TTreeNode): boolean;
begin
  Result := False; // comparison couldn't be done on date - default
  if iOrigRecord.IndexOfName(CHANGED_DATE)<>-1 then
  begin
    if (iOrigRecord.Values[CHANGED_DATE]='') or (iImportRecord.Values[CHANGED_DATE]='') then
    begin
      if iImportRecord.Values[CHANGED_DATE]<>'' then begin
        { must have a changed date, so accept this record - just remove from treeview }
        StoreItemInList(FDuplicates, iNode);
        DeleteNode(iNode);
        Result := True; // successful comparison
      end else
        { One or other changed date missing, so try an entry date comparison }
        Result := CompareSpecificDateAndReject(ENTRY_DATE, iOrigRecord,
                                               iImportRecord, iNode);
    end else // do changed date comparison
      Result := CompareSpecificDateAndReject(CHANGED_DATE, iOrigRecord,
                                             iImportRecord, iNode);
  end else
  if iOrigRecord.IndexOfName(ENTRY_DATE)<>-1 then
    { Try an entry_date comparison }
    Result := CompareSpecificDateAndReject(ENTRY_DATE, iOrigRecord,
                                           iImportRecord, iNode);
end;  // RejectUsingDateComparison

//==============================================================================
{ Once we have decided whether to compare records on entry date or changed date,
    actually do the comparison and reject the import if relevant. }
function TdlgDataImport.CompareSpecificDateAndReject(const iField: string;
  iOrigRecord, iImportRecord: TStringList; iNode: TTreeNode): boolean;
var
  lOrigDate, lImportDate: TDateTime;
begin
  Result := True;// default is successful comparison
  { If either value is missing, ignore the node }
  if (iOrigRecord.Values[iField]='') or (iImportRecord.Values[iField]='') then
    Result := False
  else
  begin
    lOrigDate:=StrToDateTime(iOrigRecord.Values[iField]);
    lImportDate:=StrToDateTime(iImportRecord.Values[iField]);
    if lOrigDate >= lImportDate then
      StoreItemInList(FRejections, iNode);
    DeleteNode(iNode);
  end;
end;  // CompareSpecificDateAndReject

//==============================================================================
{ Enable or disable the Import button when all the items in tcDuplicateData
    have been accounted for.  Also ensures comparison treeviews area cleared
    as appropriate }
procedure TdlgDataImport.SetImportButtonState;
begin
  bbImport.Enabled := (tvDuplicateData.Items.Count=0);
  bbAllImported.Enabled := not bbImport.Enabled;
  bbAllOriginal.Enabled := not bbImport.Enabled;
  bbAllLatest.Enabled := not bbImport.Enabled;
  bbAccept.Enabled := tvDuplicateData.Selected<>nil;
  bbReject.Enabled := tvDuplicateData.Selected<>nil;
  if tvDuplicateData.Selected = nil then
  begin
    tvImportedData.Items.Clear;
    tvOriginalData.Items.Clear;
  end else if tvDuplicateData.Selected.Parent = nil then
  begin
    tvImportedData.Items.Clear;
    tvOriginalData.Items.Clear;
  end;
end;  // SetImportButtonState

//==============================================================================
{ Loop through any invalid records in the TempData instance, and set up the
    relevant treeview so the user can see them }
procedure TdlgDataImport.DisplayInvalidRecords;
var
  i, lTableIndex: integer;
  lParentNode: TTreeNode;
  lItemTable: string;
  lItemKey: string;
begin
  for i := 0 to FTempData.InvalidDataList.Count-1 do
  begin
    lItemTable := FTempData.InvalidDataList.Names[i];
    lItemKey := Copy(FTempData.InvalidDataList[i], Length(lItemTable)+2, 255);
    try
      FUniqueInvalidItemList.Add(lItemTable + '=' + lItemKey);
      lTableIndex := FTablesWithFailures.IndexOf(lItemTable);
      if lTableIndex=-1 then // need new top-level node
      begin
        lParentNode := tvInvalidData.Items.Add(nil, Uppercase(lItemTable));
        FTablesWithFailures.AddObject(lItemTable, lParentNode);
      end
      else // add to existing node
        lParentNode := TTreeNode(FTablesWithFailures.Objects[lTableIndex]);
      tvInvalidData.Items.AddChildObject(lParentNode,
                                         lItemKey,
                                         TMessage(FTempData.InvalidDataList.Objects[i]));
    except
      on EStringListError do ; // nothing - record already marked as invalid
    end;
  end;
end;  // DisplayInvalidRecords

//==============================================================================
{ Loop through all the invalid tags (ie those with parsing errors) and display
    them in the treeview }
procedure TdlgDataImport.DisplayInvalidTags;
var
  i: integer;
  lParentNode: TTreenode;
begin
  if FTempData.InvalidTagList.Count > 0 then
  begin
    lParentNode := tvInvalidData.Items.AddChildFirst(nil, INVALID_TAGS);
    for i := 0 to FTempData.InvalidTagList.Count-1 do
      tvInvalidData.Items.AddChildObject(lParentNode,
                                         TTag(FTempData.InvalidTagList[i]).Name,
                                         TTag(FTempData.InvalidTagList[i]));
  end; // if anything to do
end;  // DisplayInvalidTags

//==============================================================================
{ Enable the analyse button only when a valid file selected }
procedure TdlgDataImport.eSourceChange(Sender: TObject);
begin
  bbAnalyse.Enabled := FileExists(eSource.Text);
end;  // eSourceChange

//==============================================================================
procedure TdlgDataImport.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  frmMain.TaskFinished;
end;  // FormClose

//==============================================================================
function TdlgDataImport.ImportDatabaseUnzipped(const AZipFile: String): String;
var lUnzipTool: TVCLUnzip;
    lstFileName: String;
    lCursor: TCursor;
begin
  lCursor := HourglassCursor;
  Createdir(gfnGetTempPath + TEMP_DB_FOLDER);
  lUnzipTool := TVCLUnzip.Create(nil);
  with lUnzipTool do
    try
      ZipName := AZipFile;
      ReadZip;
      if (Count <> 1) or (CompareText(ExtractFileExt(FileName[0]), '.mdb') <> 0) then begin
        Result := '';
        raise EInvalidZipFile.CreateNonCritical(ResStr_InvalidZipFile);
      end else begin
        DestDir := gfnGetTempPath + TEMP_DB_FOLDER;
        lstFileName := FileName[0];
        // Overwrite any previous mdb in temp folder, the Zip still has a copy
        OverwriteMode := Always;
        DoAll := true;
        // Do the unzip
        UnZip;
      end;
    finally
      Free;
      Result := gfnGetTempPath + TEMP_DB_FOLDER + lstFileName;
      FUnzippedFiles.Add(gfnGetTempPath + TEMP_DB_FOLDER + lstFileName);
      DefaultCursor(lCursor);
    end;
end;  // ImportDatabaseUnzipped

//==============================================================================
// Work out an Access database name from the XML file name.
function TdlgDataImport.ImportDatabaseFromXMLFileName(const AXMLFile: String): String;
begin
  // Make sure the temp folder exists
  Createdir(gfnGetTempPath + TEMP_DB_FOLDER);
  // Use XML File name for temp DB file
  Result := gfnGetTempPath + TEMP_DB_FOLDER + ChangeFileExt(ExtractFileName(AXMLFile), '.mdb');
end;  // ImportDatabaseFromXMLFileName

//==============================================================================
procedure TdlgDataImport.RebuildIndexes;
begin
  if FTempData.RebuildTaxonIndex then begin
    RebuildIndexTaxonName(dmGeneralData.qryAllPurpose, frmMain.SetStatus, frmMain.SetProgress);
    Application.ProcessMessages;
    RebuildIndexTaxonSynonym(dmGeneralData.qryAllPurpose, frmMain.SetStatus, frmMain.SetProgress);
    frmMain.TaskFinished;
  end;
end;  // RebuildIndexes

//==============================================================================
end.
