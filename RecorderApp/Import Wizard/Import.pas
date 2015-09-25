{===============================================================================
  Unit:        Import

  Defines:     TfraImport

  Description: Process the data and import it into Recorder.

  Model:       ImportWizard

  Last revision information:
    $Revision: 23 $
    $Date: 24/03/10 16:01 $
    $Author: Andrewkemp $

===============================================================================}

unit Import;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, IWBasePage, ExtCtrls, IWSettings, StdCtrls, ImageListButton, HTMLView,
  Buttons, DatabaseAccessADO, MarkupDocs, JNCCXMLDoc, XMLTypes, Recorder2000_TLB,
  DataClasses, ComCtrls, ApplicationSettings, ComObj, RapTree;

resourcestring
  ResStr_MoreThan10KDuplicates = 'More than 10,000 duplicates present';

  //Status
  ResStr_CheckingImportedDB = 'Checking imported database...';
  ResStr_DuplicatesFound =  'Duplicates found: %s';

type
  {-----------------------------------------------------------------------------
    Wizard page displayed whilst the imported data is parsed and transferred into a temporary 
    import database, prior to import analysis, validation and duplicate checking then actual
    import.
    Once the process of creating and loading data into the temporary import database is
    complete, the next wizard page is shown immediately.
  }
  TfraImport = class (TBasePage)
    btnStop: TBitBtn;
    lblProcessing: TLabel;
    lblInfo: TLabel;
    lblStepProcessing: TLabel;
    lblCreating: TLabel;
    lblStepCreating: TLabel;
    pnlPostProcessing: TPanel;
    lblStepValidating: TLabel;
    lblStepDuplicates: TLabel;
    lblValidating: TLabel;
    lblDuplicates: TLabel;
    procedure btnStopClick(Sender: TObject);
  private
    FDescLabel: TLabel;
    FStep: Integer;
    FStepLabel: TLabel;
    FCancelled: boolean;
    FMetaDataTag: TTag;
    FImportIntf: IUnknown;
    FTablesWithFailures: TStringList;
    FUniqueInvalidItemList: TStringList;
    FUnzippedFiles: TStringList;
    procedure SetStepOn(AStep, ADesc: TLabel);
    procedure SetStepOff;
    procedure DoNBNAnalysis;
    procedure DoCOMImport;
    procedure DoComValidation;
    procedure PrepareDatabaseComparison;
    function GetCancelled: boolean;
    function BuildDuplicateCheckQuery(const iTableName: string;
      const iPrimaryKey: TPrimaryKey): String;
    function ImportDatabaseUnzipped(const AZipFile: String): String;
    procedure DoDuplicates;
    procedure DisplayInvalidRecords;
    procedure DisplayInvalidTags;
    function ImportDatabaseFromXMLFileName(const AXMLFile: String): String;
    function TemporaryDirectory: String;
  protected
    procedure GenerateImportDatabase; virtual;
    function GetHasNext: Boolean; override;
    function GetHasPrevious: Boolean; override;
    function GetNext: TBasePageClass; override;
    function GetPrevious: TBasePageClass; override;
    function GetHtmlImageName: String; override;
  public
    constructor Create(AOwner: TComponent; ASettings: TdmIWSettings); override;
    destructor Destroy; override;
    procedure LoadContent; override;
    procedure SaveContent; override;
    procedure Cancel; override;
    function Execute: Boolean; override;
    property Cancelled: boolean read GetCancelled;
  end;

//==============================================================================
implementation

{$R *.dfm}

uses
  IWResourceStrings, IWConstants, ImportAnalysis, MatchGeneric, Maintbar,
  TempData_ADO, GeneralFunctions, VCLUnzip, ADODb;

const
  INVALID_TAGS = 'Invalid XML tags';

{-==============================================================================
    TfraImport
===============================================================================}
{-------------------------------------------------------------------------------
}
procedure TfraImport.btnStopClick(Sender: TObject);
begin
  inherited;
  lblInfo.Caption := ResStr_ImportAborted;
  Cancel;
end;  // TfraImport.btnStopClick

{-------------------------------------------------------------------------------
}
procedure TfraImport.Cancel;
begin
  inherited;
  if Assigned(FStepLabel) then FStepLabel.Caption := 'X';
  if Assigned(FDescLabel) then FDescLabel.Font.Style := [];
  FStepLabel := nil;
  FDescLabel := nil;
  FCancelled := True;
  btnStop.Enabled := False;
  ChangedContent;
end;  // TfraImport.Cancel

{-------------------------------------------------------------------------------
}
function TfraImport.Execute: Boolean;
begin
  try
    try
      FStepLabel := nil;
      FDescLabel := nil;
      FStep := 1;
      SetStepOn(lblStepProcessing, lblProcessing);
      FCancelled :=false;
      if Settings.ImportType <> itAddin then
        CreateDir(TemporaryDirectory);
      case Settings.ImportType of
        itNBNData, itZippedAccess: DoNBNAnalysis;
        itAddin: DoComImport;
      else
        GenerateImportDatabase;
        if not Cancelled then
          Settings.TempData := TTempData.Create(nil, frmMain.ProgressBar,
              frmMain.SetStatus, GetCancelled);
        if not Cancelled then
        begin
          Settings.TempData.ConnectTo(Settings.ImportFile.ImportDatabase);
          PrepareDatabaseComparison;
        end;
      end; // case
      Result := not Cancelled;
    finally
      frmMain.SetStatus('');
      frmMain.SetProgress(0);
    end;
  except
    // If the user cancels the import, all sorts of things could go wrong and cause
    // an error. If this is the case just suppress the error, otherwise raise the
    // error and cancel the import.
    if not Cancelled then begin
      Cancel;
      raise;
    end else
      Result := False;
  end;
end;  // TfraImport.Execute

{-------------------------------------------------------------------------------
}
procedure TfraImport.GenerateImportDatabase;
begin
  with Settings.ImportFile do
  begin
    OnStatusChanged := frmMain.SetStatus;
    OnProgress := frmMain.SetProgress;
    AbortPredicate := GetCancelled;

    ApplyTableRules;

    if not Cancelled then
    begin
      SetStepOn(lblStepCreating, lblCreating);
      GenerateImportDatabase;
    end;
  end;
end;  // TfraImport.GenerateImportDatabase

{-------------------------------------------------------------------------------
}
function TfraImport.GetHasNext: Boolean;
begin
  Result := False;
end;  // TfraImport.GetHasNext

{-------------------------------------------------------------------------------
}
function TfraImport.GetHasPrevious: Boolean;
begin
  Result := Cancelled;
end;  // TfraImport.GetHasPrevious

{-------------------------------------------------------------------------------
}
function TfraImport.GetNext: TBasePageClass;
begin
  Result := TfraImportAnalysis;
end;  // TfraImport.GetNext

{-------------------------------------------------------------------------------
}
function TfraImport.GetPrevious: TBasePageClass;
begin
  Result := TfraMatchGeneric;
end;  // TfraImport.GetPrevious

{-------------------------------------------------------------------------------
}
procedure TfraImport.LoadContent;
begin
  if Settings.ImportType in [itNBNData, itZippedAccess, itAddin] then
  begin
    pnlPostProcessing.Top := lblCreating.Top;
    lblCreating.Visible   := False;
  end;
end;  // TfraImport.LoadContent

{-------------------------------------------------------------------------------
}
procedure TfraImport.SetStepOn(AStep, ADesc: TLabel);
begin
  SetStepOff;
  if Cancelled then Exit;
  FStepLabel := AStep;
  FDescLabel := ADesc;
  
  AStep.Caption := '>>';
  ADesc.Font.Style := [fsBold];
  Refresh;
end;  // TfraImport.SetStepOn 

{-------------------------------------------------------------------------------
}
procedure TfraImport.DoNBNAnalysis;
var
  lOverallTaskIndex, lTaskIndex, lSubTaskIndex: integer;
  lCursor   : TCursor;
  lTagToProcess: TTag;
  ltfFinished: boolean;
  lstDatabaseName: String;
begin
  frmMain.ProgressBar.TaskPosition := 0;
  lCursor := HourglassCursor;
  try
    // Importing from zipped Access database
    if Settings.ImportType=itZippedAccess then begin
      lstDatabaseName := ImportDatabaseUnzipped(Settings.SourceDataFile);
      if lstDatabaseName <> '' then begin
        Settings.TempData := TTempData.Create(nil, frmMain.ProgressBar, frmMain.SetStatus, GetCancelled);
        Settings.TempData.ConnectTo(lstDatabaseName);
        PrepareDatabaseComparison;
      end
      else
        FCancelled := true;
    end
    else if Settings.ImportType = itNBNData then begin
      try
        if FCancelled then Exit;
        lstDatabaseName := ImportDatabaseFromXMLFileName(Settings.SourceDataFile);
        Settings.TempData := TTempData.Create(Settings.XMLDoc, frmMain.ProgressBar, frmMain.SetStatus, GetCancelled);
        Settings.TempData.ConnectTo(lstDatabaseName);
        if FCancelled then Exit;
        ltfFinished := False;
        lOverallTaskIndex := frmMain.ProgressBar.EmbedTask(0,90);
        { Loop until all survey events and entire document are read into db }
        repeat
          lTaskIndex := frmMain.ProgressBar.EmbedTask(Settings.XMLDoc.PercentStart, Settings.XMLDoc.PercentEnd);
          lSubTaskIndex := frmMain.ProgressBar.EmbedTask(0,25);
          lTagToProcess := Settings.XMLDoc.ProcessDocument(True);
          if FCancelled then Exit;
          { Note, if whole document returned we can ignore tags unless inside content }
          if lTagToProcess.Name = 'nbndata' then
          begin
            lTagToProcess := Settings.XMLDoc.GetTagByName(CONTENT_TAG);
            ltfFinished := true;
          end;
          lSubTaskIndex := frmMain.ProgressBar.FinishAndEmbedNext(lSubTaskIndex, 25,100);
          Settings.TempData.ProcessDocumentChunk(lTagToProcess);
          Settings.XMLDoc.FreeTagTree(lTagToProcess);
          frmMain.ProgressBar.FinishTask(lSubTaskIndex);
          frmMain.ProgressBar.FinishTask(lTaskIndex);
        until  ltfFinished;
        lOverallTaskIndex := frmMain.ProgressBar.FinishAndEmbedNext(lOverallTaskIndex, 90, 92);
        Settings.XMLDoc.CheckStack;
        FMetaDataTag := Settings.XMLDoc.GetTagByName('metadata');
        Settings.TempData.FinaliseProcessing;
        frmMain.SetStatus(ResStr_CheckingImportedDB);
        lOverallTaskIndex := frmMain.ProgressBar.FinishAndEmbedNext(lOverallTaskIndex, 92, 100);
        PrepareDatabaseComparison;
        frmMain.ProgressBar.FinishTask(lOverallTaskIndex);
      except
        on Exception do // any problems, free the XML Doc
        begin
          Settings.FreeXMLDoc;
          raise;
        end;
      end;
    end;  // if NBN_DATA
  finally
    frmMain.TaskFinished;
    DefaultCursor(lCursor);
  end; // try..finally
  if FCancelled then
  begin
    FCancelled:=false;
    Settings.FreeXMLDoc;
  end;
end;  // btnAnalyseClick

{-------------------------------------------------------------------------------
}
procedure TfraImport.PrepareDatabaseComparison;
begin
  Settings.DuplicateNodes.BeginUpdate; // for performance
  FTablesWithFailures := TStringList.Create;
  try
    { Initiate validation through any COM addins }
    try
      DoComValidation;
    finally
      frmMain.SetStatus('');
      frmMain.SetProgress(0);
    end;
    { Display all internal invalid records and tags }
    DisplayInvalidRecords;
    DisplayInvalidTags;
    { Check for duplicates in valid records }
    DoDuplicates;
  finally
    FreeAndNil(FTablesWithFailures);
    Settings.DuplicateNodes.EndUpdate;
  end; // try..finally
end;  // PrepareDatabaseComparison

{-------------------------------------------------------------------------------
}
procedure TfraImport.DoCOMImport;
begin
  if not Assigned(FImportIntf) then
    raise EImportAnalysis.Create(ResStr_InstanceMissing);
  (FImportIntf as IImportFilter).ImportFile(Settings.SourceDataFile);
end;  // DoCOMImport

{-------------------------------------------------------------------------------
}
procedure TfraImport.DoComValidation;
var
  lAddinIndex, lFailureIndex: integer;
  lValidationIntf: IValidation;
  lGuid: TGUID;
  lKeyList: TEditableKeyList;
  lItemTable: string;
  lTableIndex: integer;
  lParentNode: TFlyNode;
  lMessage: TMessage;
  lInvalidCount: integer;
begin
  SetStepOn(lblStepValidating, lblValidating);
  { Loop through each addin }
  for lAddinIndex := 0 to AppSettings.ComAddins.Validators.Count-1 do
  begin
    lGuid := StringToGuid(AppSettings.ComAddins.Validators[lAddinIndex]);
    lValidationIntf := CreateComObject(lGuid) as IValidation;
    lInvalidCount := lValidationIntf.Validate(Settings.TempData.DatabasePath);
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
            lParentNode := Settings.InvalidNodes.Add(nil, lItemTable);
            FTablesWithFailures.AddObject(lItemTable, lParentNode);
          end
          else // add to existing node
            lParentNode := TFlyNode(FTablesWithFailures.Objects[lTableIndex]);
          lMessage := TMessage.Create;
          lMessage.Msg := lValidationIntf.ErrorString[lFailureIndex];
          Settings.InvalidNodes.AddChildObject(lParentNode,
                                             lKeyList.Items[lFailureIndex].KeyField1,
                                             lMessage);

          { Now remember not to import the record }
          if lKeyList.Header.TableName = MIXED_DATA then
            Settings.Rejections.AddObject(
                Uppercase(lItemTable)
                    + ';' + lKeyList.Items[lFailureIndex].KeyField1
                    + ';',
                lMessage)
          else
            Settings.Rejections.AddObject(
                Uppercase(lItemTable)
                    + ';' + lKeyList.Items[lFailureIndex].KeyField1
                    + ';' + lKeyList.Items[lFailureIndex].KeyField2,
                lMessage);
        except
          on EStringListError do ; // nothing - record already listed as invalid
        end; // try
      end;
    end; // for lFailureIndex
  end; // for lAddinIndex
end;  // DoComValidation

{-------------------------------------------------------------------------------
  BuildDuplicateCheckQuery prepares qryAllPurpose to scan for duplicates on
     a given table.  NOTE, ONLY THE FIRST PART OF THE PRIMARY KEY IS USED
     IN THE JOIN IF THE KEY HAS 2 FIELDS }
function TfraImport.BuildDuplicateCheckQuery(const iTableName: string;
  const iPrimaryKey: TPrimaryKey): String;
begin
  { Add where clause to find only duplicate records - only use first part of
     key in join if key has 2 fields otherwise the query takes ages}
  Result := 'SELECT DISTINCT Import.' + iPrimaryKey.Key1;
  if iPrimaryKey.Key2 <> '' then
    Result := Result + ', Import.' + iPrimaryKey.Key2;
  Result := Result +
            Format(' FROM [%s] AS Import INNER JOIN SQLSERVER_%s AS Main ON Import.%s = Main.%s',
                   [iTableName, iTableName, iPrimaryKey.Key1, iPrimaryKey.Key1]);
  if iPrimaryKey.Key2 <> '' then
    Result := Result + Format(' WHERE Import.%s = Main.%s',
                       [iPrimaryKey.Key2, iPrimaryKey.Key2]);
end;  // BuildDuplicateCheckQuery

{-------------------------------------------------------------------------------
}
function TfraImport.GetCancelled: boolean;
begin
  Result := FCancelled;
end;

{-------------------------------------------------------------------------------
}
function TfraImport.ImportDatabaseUnzipped(const AZipFile: String): String;
var lUnzipTool: TVCLUnzip;
    lstFileName: String;
    lCursor: TCursor;
begin
  lCursor := HourglassCursor;
  lUnzipTool := TVCLUnzip.Create(nil);
  with lUnzipTool do
    try
      ZipName := AZipFile;
      ReadZip;
      if (Count <> 1) or (CompareText(ExtractFileExt(FileName[0]), '.mdb') <> 0) then begin
        Result := '';
        raise EInvalidZipFile.CreateNonCritical(ResStr_InvalidZipFile);
      end else begin
        DestDir := TemporaryDirectory;
        lstFileName := FileName[0];
        // Overwrite any previous mdb in temp folder, the Zip still has a copy
        OverwriteMode := Always;
        DoAll := true;
        // Do the unzip
        UnZip;
      end;
    finally
      Free;
      Result := TemporaryDirectory + lstFileName;
      FUnzippedFiles.Add(TemporaryDirectory + lstFileName);
      DefaultCursor(lCursor);
    end;
end;  // ImportDatabaseUnzipped

{-------------------------------------------------------------------------------
  Check each table - go through fields in import database and check if
         exists in the main DB }
procedure TfraImport.DoDuplicates;
var i               : integer;
    lTableNode      : TFlyNode;
    lTableAdded     : boolean;
    lPrimaryKey     : TPrimaryKey;
    lKeyDisplayString: string;
    lqryCountAndDup : TADOQuery;
    lNotRejected    : Boolean;
begin
  SetStepOn(lblStepDuplicates, lblDuplicates);
  frmMain.SetProgress(0);
  lTableNode := nil; // initialise
  lqryCountAndDup := TADOQuery.Create(nil);
  try
    lqryCountAndDup.Connection := Settings.TempData.Database;
    for i := 0 to Settings.TempData.TablesCreated.Count-1 do
    begin
      // Get proper TotalCount by added all record from currently processed table
      with lqryCountAndDup do begin
        SQL.Text := 'SELECT Count(*) FROM [' + Settings.TempData.TablesCreated[i] + ']';
        Open;
        try
          Settings.TotalCount := Settings.TotalCount + Fields[0].AsInteger;
        finally
          Close;
        end;
      end;
      // Now, find out the duplicates
      lPrimaryKey := dmDatabase.SplitPrimaryKey(Settings.TempData.TablesCreated[i]);
      dmDatabase.CreateLinkedTable(Settings.TempData.Database,
          Settings.TempData.TablesCreated[i], 'SQLSERVER_' + Settings.TempData.TablesCreated[i]);
      lqryCountAndDup.SQL.Text := BuildDuplicateCheckQuery(Settings.TempData.TablesCreated[i], lPrimaryKey);
      with lqryCountAndDup do begin
        Open;
        try
          lTableAdded := False;
          while not Eof do begin
            { If 2 field key then need to do extra check }
            if lPrimaryKey.Key2 = '' then
              lNotRejected := Settings.Rejections.IndexOf(Uppercase(Settings.TempData.TablesCreated[i]) + ';' +
                                                  FieldByName(lPrimaryKey.Key1).AsString + ';') = -1
            else
              lNotRejected := Settings.Rejections.IndexOf(Uppercase(Settings.TempData.TablesCreated[i]) + ';' +
                                                  FieldByName(lPrimaryKey.Key1).AsString + ';' +
                                                  FieldByName(lPrimaryKey.Key2).AsString) = -1;
            if lNotRejected then
            begin
              { Store the record which is a duplicate }
              if not lTableAdded then
              begin
                lTablenode := Settings.DuplicateNodes.Add(nil, Uppercase(Settings.TempData.TablesCreated[i]));
                lTableAdded := True;
              end;
              if lPrimaryKey.Key2 = '' then
                lKeyDisplayString := FieldByName(lPrimaryKey.Key1).AsString
              else
                lKeyDisplayString := FieldByName(lPrimaryKey.Key1).AsString + ', ' +
                                     FieldByName(lPrimaryKey.Key2).AsString;
              Settings.DuplicateNodes.AddChild(lTableNode, lKeyDisplayString);
              Settings.DuplicateCount := Settings.DuplicateCount + 1;
            end; // not already rejected
            Next;
            if Settings.DuplicateCount mod 100 = 0 then
              frmMain.SetStatus(Format(ResStr_DuplicatesFound,[IntToStr(Settings.DuplicateCount)]));
          end; // while
        finally
          Close;
        end; // try
      end; // with
      dmDatabase.RemoveLinkedTable(Settings.TempData.Database, 'SQLSERVER_' + Settings.TempData.TablesCreated[i]);
      frmMain.SetProgress(
          100 * (i + 1) div Settings.TempData.TablesCreated.Count);
    end; // for all Settings.TempData.TablesCreated
  finally
    lqryCountAndDup.Free;
    frmMain.SetProgress(0);
    frmMain.SetStatus('');
  end;
end;  // DoDuplicates


{-------------------------------------------------------------------------------
  Loop through any invalid records in the TempData instance, and set up the
    relevant treeview so the user can see them }
procedure TfraImport.DisplayInvalidRecords;
var
  i, lTableIndex: integer;
  lParentNode: TFlyNode;
  lItemTable: string;
  lItemKey: string;
begin
  for i := 0 to Settings.TempData.InvalidDataList.Count-1 do
  begin
    lItemTable := Settings.TempData.InvalidDataList.Names[i];
    lItemKey := Copy(Settings.TempData.InvalidDataList[i], Length(lItemTable)+2, 255);
    try
      FUniqueInvalidItemList.Add(lItemTable + '=' + lItemKey);
      lTableIndex := FTablesWithFailures.IndexOf(lItemTable);
      if lTableIndex=-1 then // need new top-level node
      begin
        lParentNode := Settings.InvalidNodes.Add(nil, Uppercase(lItemTable));
        FTablesWithFailures.AddObject(lItemTable, lParentNode);
      end
      else // add to existing node
        lParentNode := TFlyNode(FTablesWithFailures.Objects[lTableIndex]);
      Settings.InvalidNodes.AddChildObject(lParentNode,
                                         lItemKey,
                                         TMessage(Settings.TempData.InvalidDataList.Objects[i]));
    except
      on EStringListError do ; // nothing - record already marked as invalid
    end;
  end;
end;  // DisplayInvalidRecords

{-------------------------------------------------------------------------------
  Loop through all the invalid tags (ie those with parsing errors) and display
     them in the treeview
}
procedure TfraImport.DisplayInvalidTags;
var
  i: integer;
  lParentNode: TFlyNode;
begin
  if Settings.TempData.InvalidTagList.Count > 0 then
  begin
    lParentNode := Settings.InvalidNodes.AddChildFirst(nil, INVALID_TAGS);
    for i := 0 to Settings.TempData.InvalidTagList.Count-1 do
      Settings.InvalidNodes.AddChildObject(lParentNode,
                                         TTag(Settings.TempData.InvalidTagList[i]).Name,
                                         TTag(Settings.TempData.InvalidTagList[i]));
  end; // if anything to do
end;  // DisplayInvalidTags

{-------------------------------------------------------------------------------
  Work out an Access database name from the XML file name.
}
function TfraImport.ImportDatabaseFromXMLFileName(const AXMLFile: String): String;
begin
  // Use XML File name for temp DB file
  Result := TemporaryDirectory + ChangeFileExt(ExtractFileName(AXMLFile), '.mdb');
end;  // ImportDatabaseFromXMLFileName


{-------------------------------------------------------------------------------
}
constructor TfraImport.Create(AOwner: TComponent; ASettings: TdmIWSettings);
begin
  inherited;
  FUniqueInvalidItemList := TStringList.Create;
  FUniqueInvalidItemList.Sorted := True;
  FUniqueInvalidItemList.Duplicates := dupError; // warn us if an item is failing validation multiple times.
  FUnzippedFiles := TStringList.Create;
end;

{-------------------------------------------------------------------------------
}
destructor TfraImport.Destroy;
var
  i: integer;
begin
  FUniqueInvalidItemList.Free;
  for i := 0 to FUnzippedFiles.Count-1 do
    if FileExists(FUnzippedFiles[i]) then
      DeleteFile(FUnzippedFiles[i]);
  inherited;
end;

{-------------------------------------------------------------------------------
}
procedure TfraImport.SaveContent;
begin
  inherited;
  Settings.InvalidCount := FUniqueInvalidItemList.Count;
end;

{-------------------------------------------------------------------------------
}
procedure TfraImport.SetStepOff;
begin
  if Assigned(FStepLabel) then FStepLabel.Caption := '';
  if Assigned(FDescLabel) then FDescLabel.Font.Style := [];
  FStepLabel := nil;
  FDescLabel := nil;
  Refresh;
  Application.ProcessMessages;
end;

{-------------------------------------------------------------------------------
}
function TfraImport.GetHtmlImageName: String;
begin
  Result := 'Import.jpg';
end;

{-------------------------------------------------------------------------------
}
function TfraImport.TemporaryDirectory: String;
begin
  Result := GetWindowsTempDir + TEMP_DB_FOLDER;
end;

end.


