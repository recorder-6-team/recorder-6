unit ExportWizard;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ExtCtrls, Recorder2000_TLB, ADOdb, ADOInt, ComObj,
  ActiveX, ShlObj, FileCtrl, Registry, StrUtils, TypInfo, gnugettext, SHFolder;

type
  TOccKeyMode = (kmWizard, kmXmlReport);

  TdlgExportWizard = class(TForm)
    pcWizard: TPageControl;
    tsGrid: TTabSheet;
    btnCancel: TButton;
    btnPrev: TButton;
    btnNext: TButton;
    btnFinish: TButton;
    tsRegion: TTabSheet;
    Label4: TLabel;
    mmMTBsToInclude: TMemo;
    tsOutput: TTabSheet;
    lblOutputFinestInstruct: TLabel;
    rgGridSystem: TRadioGroup;
    lblOutputAggregatedInstruct: TLabel;
    rgOutputMode: TRadioGroup;
    tsNonCompatibleGrids: TTabSheet;
    gbNonCompatibleGrids: TGroupBox;
    eOutputDir: TEdit;
    btnSelectDir: TButton;
    Label1: TLabel;
    ProgressBar: TProgressBar;
    lblExportProgress: TLabel;
    rgSynonymHandling: TRadioGroup;
    rgNonGermanGridConversion: TRadioGroup;
    rgGermanGridConversions: TRadioGroup;
    gbGermanGridConversions: TGroupBox;
    chkDiscardQYX: TCheckBox;
    chkDiscardQQQ: TCheckBox;
    rgResolveConflicts: TRadioGroup;
    procedure btnNextClick(Sender: TObject);
    procedure btnPrevClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure btnFinishClick(Sender: TObject);
    procedure pcWizardChange(Sender: TObject);
    procedure rgOutputModeClick(Sender: TObject);
    procedure pcWizardChanging(Sender: TObject; var AllowChange: Boolean);
    procedure rgGridSystemClick(Sender: TObject);
    procedure btnSelectDirClick(Sender: TObject);
  private
    FoldConn: _Connection;
    FOccKeyMode: TOccKeyMode;
    FRecorder: IRecorder2000;
    FConnection: TADOConnection;
    FClosing: boolean;
    FTranslations: TStringList;
    FOutputGridSystem: string;
    FOutputGridCode: string;
    FOutputResolution: integer;
    FMessages: TStringList;
    addinPath: string;
    function IsInteger(S: String): Boolean;
    procedure SetButtonState;
    procedure ValidateCurrentPage;
    procedure ValidateRegion;
    procedure ValidateOutput;
    procedure ValidateGrid;
    procedure BuildAtlasOutputData;
    procedure ProcessSynonyms;
    procedure ConvertOrDiscardGridData;
    procedure ConvertGermanDataToOutputSystem;
    procedure ConvertNonGermanDataToOutputSystem;
    procedure DiscardNonGermanGridData;
    procedure PrepareConversionTable;
    procedure OutputDataTableFinest(outputConn: TADOConnection);
    procedure OutputDataTableAggregated(outputConn: TADOConnection);
    procedure OutputTaxaTable(outputConn: TADOConnection);
    procedure ProcessAggregationValues;
    procedure DiscardOutOfRangeGridData;
    procedure ConvertOrDiscardGermanData;
    procedure SetProgressLabel(const text: string);
    procedure ValidateNonCompatibleGrids;
    procedure Msg(const text: string);
    function GetSpecialFolderPath(folder: integer): string;
    procedure LoadSettings;
    procedure SaveSettings(const overrideFilePath: string='');
    function CleanupRegions(text: string): string;
    procedure ValidateAllPages;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

uses
  XmlSystemsImpl;

const
  SQL_CLEANUP_TEMP = 'IF OBJECT_ID(''TempDB..#%s'') IS NOT NULL DROP TABLE #%s';

  SQL_FIELDS =
      'TD.Taxon_List_Item_Key, '+
      'S.Spatial_Ref AS MTBQ, '+
      'S.Spatial_Ref_System AS OUTPUTSYS, '+
      'CAST(''*'' as VARCHAR(1)) AS STATUS, '+
      'CAST('''' as VARCHAR(1)) AS RAST_UN, '+
      'CAST('''' as VARCHAR(1)) AS FE_U, '+
      'CAST('''' as VARCHAR(1)) AS FE_H, '+
      'CAST('''' as VARCHAR(4)) AS USR, '+
      'S.Lat, S.Long, '+
      'S.Spatial_Ref AS SPATREF, '+
      'S.Spatial_Ref_System AS SPATREFSYS, '+
      'CAST(NULL AS BIGINT) AS selvalue, '+
      'CAST(NULL AS CHAR(2)) AS aggvalue, '+
      'TD.Taxon_Occurrence_Key, '+
      'S.Sample_Key, S.Vague_Date_Start as date_from, S.Vague_Date_End as date_to ';

  SQL_POPULATE_DATA_WIZARD_REPORT = 'SELECT '+
  SQL_FIELDS +
  'INTO #interim '+
  'FROM #Report_Output RO '+
  'INNER JOIN Taxon_Determination TD ON TD.Taxon_Occurrence_Key=RO.Occurrence_Key AND TD.Preferred=1 '+
  'INNER JOIN Taxon_Occurrence XO ON XO.Taxon_Occurrence_Key=RO.Occurrence_Key '+
  'INNER JOIN Sample S ON S.Sample_Key=XO.Sample_Key '+
  'WHERE RO.Type=''T'' AND S.Lat IS NOT NULL';

  SQL_POPULATE_DATA_XML_REPORT = 'SELECT '+
  SQL_FIELDS +
  'INTO #interim '+
  'FROM #Report_Output RO '+
  'INNER JOIN Taxon_Determination TD ON TD.Taxon_Occurrence_Key=RO.Taxon_Occurrence_Key AND TD.Preferred=1 '+
  'INNER JOIN Taxon_Occurrence XO ON XO.Taxon_Occurrence_Key=RO.Taxon_Occurrence_Key '+
  'INNER JOIN Sample S ON S.Sample_Key=XO.Sample_Key '+
  'WHERE S.Lat IS NOT NULL';

  SQL_CONVERT_TO_PREFERRED_NAMES_NS = 'UPDATE #interim '+
  'SET #interim.Taxon_List_Item_Key=ITN.Recommended_Taxon_List_Item_Key '+
  'FROM #interim '+
  'INNER JOIN Index_Taxon_Name ITN ON ITN.Taxon_List_Item_Key=#interim.Taxon_List_Item_Key';

  SQL_CONVERT_TO_PREFERRED_NAMES_ITS = 'UPDATE #interim '+
  'SET #interim.Taxon_List_Item_Key=TLI.Preferred_Name '+
  'FROM #interim '+
  'INNER JOIN Taxon_List_Item TLI ON TLI.Taxon_List_Item_Key=#interim.Taxon_List_Item_Key';

  SQL_CONVERT_COMMON_TO_PREFERRED_NAMES_NS = 'UPDATE #interim '+
  'SET #interim.Taxon_List_Item_Key=ITN.Recommended_Taxon_List_Item_Key '+
  'FROM #interim '+
  'INNER JOIN Index_Taxon_Name ITN ON ITN.Taxon_List_Item_Key=#interim.Taxon_List_Item_Key '+
  'INNER JOIN Taxon_List_Item TLI ON TLI.Taxon_List_Item_Key=#interim.Taxon_List_Item_Key '+
  'INNER JOIN Taxon_Version TV ON TV.Taxon_Version_Key=TLI.Taxon_Version_Key '+
  'INNER JOIN Taxon T ON T.Taxon_Key=TV.Taxon_Key AND T.Language<>''la''';

  SQL_CONVERT_COMMON_TO_PREFERRED_NAMES_ITS = 'UPDATE #interim '+
  'SET #interim.Taxon_List_Item_Key=TLI.Preferred_Name '+
  'FROM #interim '+
  'INNER JOIN Taxon_List_Item TLI ON TLI.Taxon_List_Item_Key=#interim.Taxon_List_Item_Key '+
  'INNER JOIN Taxon_Version TV ON TV.Taxon_Version_Key=TLI.Taxon_Version_Key '+
  'INNER JOIN Taxon T ON T.Taxon_Key=TV.Taxon_Key AND T.Language<>''la''';

  SQL_UPDATE_DATA = 'UPDATE #interim '+
  'SET %s=TOD.Data '+
  'FROM #interim '+
  'INNER JOIN Taxon_Occurrence_Data TOD ON TOD.Taxon_Occurrence_Key=#interim.Taxon_Occurrence_Key '+
  '    AND TOD.Measurement_Unit_Key=''%s'' '+
  '    AND TOD.Measurement_Qualifier_Key=''%s''';

  SQL_UPDATE_SAMPLE_DATA = 'UPDATE #interim '+
  'SET %s=SD.Data '+
  'FROM #interim '+
  'INNER JOIN Sample_Data SD ON SD.Sample_Key=#interim.Sample_Key '+
  '    AND SD.Measurement_Unit_Key=''%s'' '+
  '    AND SD.Measurement_Qualifier_Key=''%s''';

  SQL_UPDATE_RAST_UN_1 = 'UPDATE #interim SET RAST_UN=''O'' WHERE RAST_UN=''E''';
  SQL_UPDATE_RAST_UN_2 = 'UPDATE #interim SET RAST_UN='''' '+
      'WHERE RAST_UN NOT IN (''N'',''O'',''S'',''W'',''U'','''') '+
      'OR SPATREFSYS NOT IN (''QYX'',''QQQ'')';

  SQL_CREATE_DATA_OUTPUT_TABLE_FINEST = 'CREATE TABLE data ('+
      'TLI_KEY CHAR(16), '+
      'MTBQ VARCHAR(7), '+
      'OUTPUTSYS VARCHAR(4), '+      
      'STATUS VARCHAR(1), '+
      'RAST_UN VARCHAR(1), '+
      'FE_U VARCHAR(1), '+
      'FE_H VARCHAR(1), '+
      'USR VARCHAR(4), '+
      'LAT VARCHAR(16), '+
      '[LONG] VARCHAR(16), '+
      'SPATREF VARCHAR(40), '+
      'SPATREFSYS VARCHAR(4))';

  SQL_CREATE_DATA_OUTPUT_TABLE_AGGREGATED = 'CREATE TABLE data ('+
      'TLI_KEY CHAR(16), '+
      'MTBQ VARCHAR(7), '+
      'OUTPUTSYS VARCHAR(4), '+      
      'STATUS VARCHAR(1), '+
      'RAST_UN VARCHAR(1))';

  SQL_CREATE_TAXA_OUTPUT_TABLE = 'CREATE TABLE taxa ('+
      'TLI_KEY CHAR(16), '+
      'TAXNAME VARCHAR(200), '+
      'TAXNAMED VARCHAR(200), '+
      'AUTOR VARCHAR(40))';

  SQL_DISCARD_INCOMPATIBLE_GERMAN_DATA = 'DELETE FROM #interim '+
      'WHERE SPATREFSYS=''%s'' AND LEN(SPATREF)>5';

  SQL_DISCARD_NONGERMAN_GRID_DATA = 'DELETE FROM #interim '+
      'WHERE SPATREFSYS NOT IN (%s)';

  SQL_STRIP_MTB_SLASHES = 'UPDATE #interim '+
      'SET MTBQ = REPLACE(MTBQ, ''/'', ''''), SPATREF = REPLACE(SPATREF, ''/'', '''') '+
      'WHERE SPATREFSYS IN (''QYX'',''QQQ'')';

  SQL_SWITCH_QYX_FORMAT = 'UPDATE #interim '+
      'SET MTBQ = LEFT(MTBQ, 5) +  CHAR(64+ SUBSTRING(MTBQ, 6, 1)) + SUBSTRING(MTBQ, 7, 1)'+
      'WHERE OUTPUTSYS = ''QYX'' AND LEN(MTBQ)>5';

  SQL_REDUCE_PRECISION = 'UPDATE #interim '+
      'SET MTBQ = LEFT(MTBQ, %d) WHERE SPATREFSYS NOT IN (''QYX'',''QQQ'')';

type
  EValidation = class(Exception);

{$R *.dfm}

constructor TdlgExportWizard.Create(AOwner: TComponent);
var
  reg: TRegistry;
begin
  inherited;
  FOldConn := nil;
  TranslateComponent(self, 'atlasexporter');
  TextDomain('atlasexporter');  
  LoadSettings;
  FClosing := false;
  FMessages := TStringList.Create;
  FMessages.Sorted := true;
  FMessages.Duplicates := dupIgnore;
  FRecorder := CreateOleObject('Recorder2000.AutoApplicationSettings') as IRecorder2000;
  reg := TRegistry.Create;
  reg.RootKey := HKEY_LOCAL_MACHINE;
  reg.OpenKeyReadOnly('SOFTWARE\Dorset Software\Recorder 6');
  addinPath := reg.ReadString('Addin Path');
  reg.free;
  FConnection := TAdoConnection.Create(nil);
  FoldConn := FConnection.ConnectionObject;
  FConnection.ConnectionObject := FRecorder.ReportResults.ReportConnection as ADOInt.Connection;
  // Test if the report has a taxon_occurrence_key. If not we cannot use the exporter.
  try
    // report wizard output should give us an occurrence key and type='T'
    FConnection.Execute('SELECT TOP 1 Occurrence_Key, Type FROM #Report_Output');
    FOccKeyMode := kmWizard;
  except on e:EOleException do
    try
      // also accept an XML report with a taxon_occurrence_key field
      FConnection.Execute('SELECT TOP 1 Taxon_Occurrence_Key FROM #Report_Output');
      FOccKeyMode := kmXmlReport;
    except on e:EOleException do
      begin
        Msg(_('The Atlas Exporter is only available for reports that include a taxon occurrence key in the data'));
        Abort;
      end;
    end;
  end;
  pcWizard.ActivePageIndex := 0;
  SetButtonState;
end;

procedure TdlgExportWizard.LoadSettings;
var
  path: string;
  settings: TStringList;
begin
  path := GetSpecialFolderPath(CSIDL_LOCAL_APPDATA) + '\Atlas Exporter';
  ForceDirectories(path);
  eOutputDir.Text := '';
  if FileExists(path + '\settings.ini') then begin
    settings := TStringList.Create;
    settings.LoadFromFile(path + '\settings.ini');
    if settings.IndexOfName('rgOutputMode')<>-1 then
      rgOutputMode.ItemIndex := StrToInt(settings.Values['rgOutputMode']);
    if settings.IndexOfName('rgSynonymHandling')<>-1 then
      rgSynonymHandling.ItemIndex := StrToInt(settings.Values['rgSynonymHandling']);
    if settings.IndexOfName('eOutputDir')<>-1 then
      eOutputDir.Text := settings.Values['eOutputDir'];
    if settings.IndexOfName('rgOutputMode')<>-1 then
      rgOutputMode.ItemIndex := StrToInt(settings.Values['rgOutputMode']);
    if settings.IndexOfName('rgGridSystem')<>-1 then
      rgGridSystem.ItemIndex := StrToInt(settings.Values['rgGridSystem']);
    if settings.IndexOfName('rgNonGermanGridConversion')<>-1 then
      rgNonGermanGridConversion.ItemIndex := StrToInt(settings.Values['rgNonGermanGridConversion']);
    if settings.IndexOfName('rgGermanGridConversions')<>-1 then
      rgGermanGridConversions.ItemIndex := StrToInt(settings.Values['rgGermanGridConversions']);
    if settings.IndexOfName('rgResolveConflicts')<>-1 then
      rgResolveConflicts.ItemIndex := StrToInt(settings.Values['rgResolveConflicts']);
    if settings.IndexOfName('chkDiscardQYX')<>-1 then
      chkDiscardQYX.Checked := StrToBool(settings.Values['chkDiscardQYX']);
    if settings.IndexOfName('chkDiscardQQQ')<>-1 then
      chkDiscardQQQ.Checked := StrToBool(settings.Values['chkDiscardQQQ']);
    if settings.IndexOfName('mmMTBsToInclude')<>-1 then
      mmMTBsToInclude.Text := settings.Values['mmMTBsToInclude'];
  end else
    // default output dir for first usage is My Documents
    eOutputDir.Text := GetSpecialFolderPath(CSIDL_PERSONAL);
end;

(**
 * Saves the current settings to the user's appdata folder, or to a different
 * location if specified in the optional parameter (a full path to the file to save).
 *)
procedure TdlgExportWizard.SaveSettings(const overrideFilePath: string='');
var
  path: string;
  settings: TStringList;
begin
  settings := TStringList.Create;
  settings.Add('rgOutputMode=' + IntToStr(rgOutputMode.ItemIndex));
  settings.Add('rgSynonymHandling=' + IntToStr(rgSynonymHandling.ItemIndex));
  settings.Add('eOutputDir=' + eOutputDir.Text);
  settings.Add('rgGridSystem=' + IntToStr(rgGridSystem.ItemIndex));
  settings.Add('rgNonGermanGridConversion=' + IntToStr(rgNonGermanGridConversion.ItemIndex));
  settings.Add('rgGermanGridConversions=' + IntToStr(rgGermanGridConversions.ItemIndex));
  settings.Add('rgResolveConflicts=' + IntToStr(rgResolveConflicts.ItemIndex));
  settings.Add('chkDiscardQYX=' + BoolToStr(chkDiscardQYX.Checked));
  settings.Add('chkDiscardQQQ=' + BoolToStr(chkDiscardQQQ.Checked));
  settings.Add('mmMTBsToInclude=' + mmMTBsToInclude.Lines.Text);
  if overrideFilePath<>'' then
    path := overrideFilePath
  else
    path := GetSpecialFolderPath(CSIDL_LOCAL_APPDATA) + '\Atlas Exporter\settings.ini';
  settings.SaveToFile(path);
end;

(**
 * Function to strip whitespace from a text string.
 *)
function TdlgExportWizard.CleanupRegions(text: string): string;
begin
  text := StringReplace(text, #9, '', [rfReplaceAll]);  // tab
  text := StringReplace(text, #10, '', [rfReplaceAll]); // lf
  text := StringReplace(text, #13, '', [rfReplaceAll]); // cr
  text := StringReplace(text, #32, '', [rfReplaceAll]); // space
  if Copy(text, Length(text), 1)=';' then
    text := Copy(text, 1, Length(text)-1);
  result := text;
end;

function TdlgExportWizard.GetSpecialFolderPath(folder : integer) : string;
const
  SHGFP_TYPE_CURRENT = 0;
var
  path: array [0..MAX_PATH] of char;
begin
  if SUCCEEDED(SHGetFolderPath(0,folder,0,SHGFP_TYPE_CURRENT,@path[0])) then
    Result := path
  else
    Result := '';
end;

procedure TdlgExportWizard.btnNextClick(Sender: TObject);
begin
  try
    ValidateCurrentPage;
    if pcWizard.ActivePageIndex < pcWizard.PageCount - 1 then
    repeat
      pcWizard.ActivePageIndex := pcWizard.ActivePageIndex + 1;
    until pcWizard.ActivePage.TabVisible=true;
    SetButtonState;
  except on e:EValidation do
    Msg(e.message);
  end;
end;

procedure TdlgExportWizard.btnPrevClick(Sender: TObject);
begin
  try
    ValidateCurrentPage;
    if pcWizard.ActivePageIndex > 0 then
      repeat
        pcWizard.ActivePageIndex := pcWizard.ActivePageIndex - 1;
      until pcWizard.ActivePage.TabVisible=true;
    SetButtonState;
  except on e:EValidation do
    Msg(e.message);
  end;
end;

(*
 * Enable or disable the next and previuos buttons depending on which page we are on.
 *)
procedure TdlgExportWizard.SetButtonState();
begin
  btnNext.Enabled := pcWizard.ActivePageIndex < pcWizard.PageCount - 1;
  btnPrev.Enabled := pcWizard.ActivePageIndex > 0;
  btnFinish.Visible := pcWizard.ActivePageIndex = pcWizard.PageCount - 1;
end;

(*
 * Cancel button needs to manually close the form as this calls from an addin.
 *)
procedure TdlgExportWizard.btnCancelClick(Sender: TObject);
begin
  FClosing := true;
end;

procedure TdlgExportWizard.ValidateCurrentPage;
begin
  if pcWizard.ActivePage = tsOutput then
    ValidateOutput
  else if pcWizard.ActivePage = tsGrid then
    ValidateGrid
  else if pcWizard.ActivePage = tsGrid then
    ValidateNonCompatibleGrids
  else if pcWizard.ActivePage = tsRegion then
    ValidateRegion;
end;

procedure TdlgExportWizard.ValidateAllPages;
begin
  ValidateOutput;
  ValidateGrid;
  if rgOutputMode.ItemIndex=1 then
    ValidateNonCompatibleGrids;
  ValidateRegion;
end;

procedure TdlgExportWizard.ValidateOutput;
begin
  if rgOutputMode.ItemIndex = -1 then
    raise EValidation.Create(_('Please select an output mode'));
  if rgSynonymHandling.ItemIndex = -1 then
    raise EValidation.Create(_('Please select how synonyms are handled'));
  if (not DirectoryExists(eOutputDir.Text)) then
    raise EValidation.Create(_('Please specify a valid directory to output data into'));
end;

procedure TdlgExportWizard.ValidateGrid;
begin
  if rgGridSystem.ItemIndex = -1 then
    raise EValidation.Create(_('Please select a grid system'));
end;

procedure TdlgExportWizard.ValidateNonCompatibleGrids;
begin
  if rgNonGermanGridConversion.ItemIndex=-1 then
    raise EValidation.Create(_('Please select how non-German grids are handled'));
end;

(*
 * Check the MTB squares given for the region are valid.
 *)
procedure TdlgExportWizard.ValidateRegion;
var
  list: TStringList;
  idx: integer;
begin
  list := TStringList.Create;
  try
    list.Delimiter := ';';
    list.DelimitedText := CleanupRegions(mmMTBsToInclude.Text);
    for idx := 0 to list.Count-1 do begin
      if (Length(list[idx])<>4) or (not IsInteger(list[idx])) then
        raise EValidation.Create(_('Region MTB Squares are not specified in a valid format'));
    end;
  finally
    list.Free;
  end;
end;

function TdlgExportWizard.IsInteger(S: String): Boolean;
begin
  try
    Result := True;
    StrToInt(S);
  except on E: EConvertError do
    Result := False;
  end;
end;


procedure TdlgExportWizard.btnFinishClick(Sender: TObject);
var
  affected: integer;
  outputFolder: string;
  outputConn: TADOConnection;
begin
  ValidateAllPages;
  outputFolder := eOutputDir.Text;
  // ensure trailing slash on folder
  if Copy(outputFolder, Length(outputFolder), 1) <> '\' then
    outputFolder := outputFolder + '\';
  if FileExists(outputFolder + 'DATA.DBF') or FileExists(outputFolder + 'TAXA.DBF') then begin
    if MessageDlg(_('The destination folder already contains exported atlas data. Are you sure you want to overwrite it?'),
        mtConfirmation, mbOKCancel, 0)=mrCancel then begin
      Abort;
    end;
    if (FileExists(outputFolder + 'DATA.DBF') and not DeleteFile(outputFolder + 'DATA.DBF')) or
        (FileExists(outputFolder + 'TAXA.DBF') and not DeleteFile(outputFolder + 'TAXA.DBF')) then begin
      Msg(_('The existing files cannot be deleted at this point in time, either because they are '+
          'already open or you don''t have permission.'));
      Abort;
    end;
  end;
  // We do a double check, because it seems that if held open by 16 bit dbase, Windows reports the file
  // as deleted even though it is only mark deleted until dbase closes!
  if FileExists(outputFolder + 'DATA.DBF') or FileExists(outputFolder + 'TAXA.DBF') then begin
    Msg(_('The existing files cannot be deleted at this point in time, either because they are '+
          'already open or you don''t have permission.'));
    Abort;
  end;
  // dialog can close when we are done, as all checks are complete.
  modalResult := mrOk;
  btnFinish.Enabled := false;
  btnPrev.Enabled := false;
  // hide the page control to show the progress bar beneath
  pcWizard.Visible := false;
  FOutputGridSystem := rgGridSystem.Items[rgGridSystem.ItemIndex];
  // find the Recorder grid system code we are converting to
  if FOutputGridSystem='MTBQYX' then
    FOutputGridCode := 'QYX'
  else
    FOutputGridCode := 'QQQ';
  // Set a value to 0 for MTB through to 3 for MTBQQQ or QYX
  FOutputResolution := Length(FOutputGridSystem)-3;
  BuildAtlasOutputData;
  try
    outputConn := TADOConnection.Create(nil);
    try

      outputConn.ConnectionString := 'Provider=Microsoft.Jet.OLEDB.4.0;Data Source=' +
                                          outputFolder +
                                          ';Extended Properties=dBASE III;User ID=Admin;Password=;';
      outputConn.Open;
      outputConn.Connected := true;
      SetProgressLabel(_('Exporting data table'));
      if rgOutputMode.ItemIndex=0 then begin
        outputConn.Execute(SQL_CREATE_DATA_OUTPUT_TABLE_FINEST, affected);
        OutputDataTableFinest(outputConn);
      end
      else begin
        outputConn.Execute(SQL_CREATE_DATA_OUTPUT_TABLE_AGGREGATED, affected);
        OutputDataTableAggregated(outputConn);
      end;
      SetProgressLabel(_('Exporting taxon table'));
      outputConn.Execute(SQL_CREATE_TAXA_OUTPUT_TABLE, affected);
      OutputTaxaTable(outputConn);
      // Save a copy of the settings for reference
      SaveSettings(outputFolder + 'settings.ini');
    finally
      outputConn.Free;
    end;
  finally
    FConnection.Execute(
      'IF EXISTS(SELECT 1 FROM tempdb.dbo.sysobjects WHERE Name LIKE ''#interim_%'' AND Type=''U'') '+
      'DROP TABLE #interim',
      affected);
  end;
  if FClosing then
    Msg(_('Export was cancelled'))
  else begin
    Msg(_('Export complete'));
    if (FMessages.Count>0) then
      Msg(FMessages.Text);
  end;
end;

procedure TdlgExportWizard.OutputDataTableFinest(outputConn: TADOConnection);
var
  affected: integer;
  rs: _Recordset;
  query: string;
begin
  rs := FConnection.Execute('SELECT * FROM #interim');
  while not rs.EOF do begin
    query := 'INSERT INTO data VALUES (''' +
        VarToStr(rs.Fields[0].Value) + ''', ''' +
        VarToStr(rs.Fields[1].Value) + ''', ''' +
        VarToStr(rs.Fields[2].Value) + ''', ''' +
        VarToStr(rs.Fields[3].Value) + ''', ''' +
        VarToStr(rs.Fields[4].Value) + ''', ''' +
        VarToStr(rs.Fields[5].Value) + ''', ''' +
        VarToStr(rs.Fields[6].Value) + ''', ''' +
        VarToStr(rs.Fields[7].Value) + ''', ''' +
        // replace , with . as we don't want European number format for lat long.
        LeftStr(StringReplace(VarToStr(rs.Fields[8].Value), ',', '.', [rfReplaceAll]), 16) + ''', ''' +
        LeftStr(StringReplace(VarToStr(rs.Fields[9].Value), ',', '.', [rfReplaceAll]), 16) + ''', ''' +
        VarToStr(rs.Fields[10].Value) + ''', ''' +
        VarToStr(rs.Fields[11].Value) + ''')';
    try
      outputConn.Execute(query, affected);
    except on E:Exception do
      begin
        Msg(_('Failure on query: ') + query);
        raise;
      end;
    end;
    ProgressBar.Position := rs.AbsolutePosition * 100 div rs.RecordCount;
    rs.MoveNext;
    // allow window to respond once every so often
    if rs.AbsolutePosition mod 100 = 0 then begin
      Application.ProcessMessages;
      if FClosing then break;
    end;
  end;
end;

procedure TdlgExportWizard.OutputDataTableAggregated(outputConn: TADOConnection);
var
  affected: integer;
  rs: _Recordset;
  query: string;
begin
  // #output is the table created by the aggregation script
  rs := FConnection.Execute('SELECT * FROM #output');
  while not rs.EOF do begin
    query := 'INSERT INTO data VALUES (''' +
        VarToStr(rs.Fields[0].Value) + ''', ''' +
        VarToStr(rs.Fields[1].Value) + ''', ''' +
        VarToStr(rs.Fields[2].Value) + ''', ''' +
        VarToStr(rs.Fields[3].Value) + ''', ''' +
        VarToStr(rs.Fields[4].Value) + ''')';
    try
      outputConn.Execute(query, affected);
    except on E:Exception do
      begin
        Msg(_('Failure on query: ') + query);
        raise;
      end;
    end;
    ProgressBar.Position := rs.AbsolutePosition * 100 div rs.RecordCount;
    rs.MoveNext;
    // allow window to respond once every so often
    if rs.AbsolutePosition mod 100 = 0 then begin
      Application.ProcessMessages;
      if FClosing then break;
    end;
  end;
end;

procedure TdlgExportWizard.OutputTaxaTable(outputConn: TADOConnection);
var
  affected: integer;
  rs: _Recordset;
  query: string;
  taxonName, commonName, authority: array[0..255] of Char;
begin
  rs := FConnection.Execute(
      'SELECT DISTINCT itn.Taxon_List_Item_Key, ITN.Actual_Name + ISNULL('' '' +TV.Attribute, ''''), ITN.Common_Name, ITN.Authority ' +
      'FROM #interim o '+
      'INNER JOIN Index_Taxon_Name itn ON itn.Taxon_List_Item_Key=o.Taxon_List_Item_Key '+
      'INNER JOIN Taxon_List_Item TLI ON TLI.Taxon_List_Item_Key=ITN.Taxon_List_Item_Key '+
      'INNER JOIN Taxon_Version TV ON TV.Taxon_Version_Key=TLI.Taxon_Version_Key');
  while not rs.EOF do begin
    // Use CharToOem to provide data in expected MSDOS (OEM) code page for DBase. Also
    // escape any single quotes to ensure valid SQL.
    CharToOem(PChar(StringReplace(VarToStr(rs.Fields[1].Value), '''', '''''', [rfReplaceAll])), taxonName);
    CharToOem(PChar(StringReplace(VarToStr(rs.Fields[2].Value), '''', '''''', [rfReplaceAll])), commonName);
    CharToOem(PChar(StringReplace(VarToStr(rs.Fields[3].Value), '''', '''''', [rfReplaceAll])), authority);
    // Don't export common name if the same as latin (which Recorder defaults to).
    if commonName=taxonName then
      commonName := '';
    query := 'INSERT INTO taxa VALUES (''' +
        VarToStr(rs.Fields[0].Value) + ''', ''' +
        taxonName + ''', ''' +
        commonName + ''', ''' +
        authority + ''')';
    try
      outputConn.Execute(query, affected);
    except on E:Exception do
      begin
        Msg(_('Failure on query: ') + query);
        raise;
      end;
    end;
    ProgressBar.Position := rs.AbsolutePosition * 100 div rs.RecordCount;
    rs.MoveNext;
    // allow window to respond once every so often
    if rs.AbsolutePosition mod 100 = 0 then begin
      Application.ProcessMessages;
      if FClosing then break;
    end;
  end;
end;

procedure TdlgExportWizard.BuildAtlasOutputData;
var
  affected: integer;
begin
  // Populate the data table - the query is dependent on how the underlying report supplies the
  // taxon occurrence key
  FConnection.Execute(format(SQL_CLEANUP_TEMP, ['interim', 'interim']));
  case FOccKeyMode of
    kmWizard:    FConnection.Execute(SQL_POPULATE_DATA_WIZARD_REPORT, affected);
    kmXmlReport: FConnection.Execute(SQL_POPULATE_DATA_XML_REPORT, affected);
  end;
  ConvertOrDiscardGridData;
  ProcessSynonyms;
  SetProgressLabel(_('Processing additional data columns'));
  // Handle Status
  FConnection.Execute(
      Format(SQL_UPDATE_DATA, ['STATUS', 'BFNSYS0000000013', 'BFNSYS0000000048'])+
      ' WHERE TOD.Data IN (''+'',''0'',''I'',''X'',''W'',''Z'',''A'',''E'',''S'',''U'',''K'')',
      affected);
  // Get the Unscharf data and process it
  FConnection.Execute(
      Format(SQL_UPDATE_SAMPLE_DATA, ['RAST_UN', 'BFNSYS0000000028', 'BFNSYS0000000077']),
      affected);
  FConnection.Execute(SQL_UPDATE_RAST_UN_1, affected);
  FConnection.Execute(SQL_UPDATE_RAST_UN_2, affected);
  // Todo: processing of the RAST_UN values to set to U in some circumstance - see docs
  // some measurements only output when not aggregating the data
  if rgOutputMode.ItemIndex=0 then begin
    FConnection.Execute(
        Format(SQL_UPDATE_DATA, ['FE_U', 'BFNSYS0000000033', 'BFNSYS0000000100']),
        affected);
    FConnection.Execute(
        Format(SQL_UPDATE_DATA, ['FE_H', 'BFNSYS0000000027', 'BFNSYS0000000074']),
        affected);
    FConnection.Execute(
        Format(SQL_UPDATE_DATA, ['USR', 'BFNSYS0000000032', 'BFNSYS0000000099']),
        affected);
    // This reduces the precision of output data to the selected precision, but ONLY
    // for non-German grids. If aggregating data then this is done as part of the aggregation
    // scripts.
    FConnection.Execute(
        Format(SQL_REDUCE_PRECISION, [FOutputResolution+4]),
        affected);
  end else
    ProcessAggregationValues;
end;

(**
 * Run the scripts which generate the selection and aggregate values for output.
 *)
procedure TdlgExportWizard.ProcessAggregationValues;
var
  script: TStringList;
  affected: integer;
  query: string;
  fileIdx: integer;
  discard: string;
begin
  SetProgressLabel(_('Aggregating data'));
  script := TStringList.Create;
  try
    // run the 6 script files in sequence
    for fileIdx := 1 to 6 do begin
      script.Clear;
      script.LoadFromFile(addinPath + '\Atlas Exporter\processing'+IntToStr(fileIdx)+'.sql');
      query := script.GetText;
      query := StringReplace(query, '#outputprecision#', IntToStr(FOutputResolution), [rfReplaceAll]);
      // set a flag in the script to discard conflicts if the user selected to do so
      if (rgResolveConflicts.ItemIndex=1) then
        discard:='1' // bitwise true
      else
        discard:='0'; // false
      query := StringReplace(query, '#discardconflicts#', discard, [rfReplaceAll]);
      try
        FConnection.Execute(query, affected);
      except
        on e:Exception do begin
          Msg(Format(_('Error during processing of script %d'), [fileIdx]));
          Msg(FConnection.Errors.Item[0].Description);
          raise e;
        end;
      end;
    end;
  finally
    script.free;
  end;
end;

(**
 * Handles the task of working out which data to convert to the output system
 * or discard depending on the options selected. Delegates the actual tasks
 * to the appropriate methods.
 *)
procedure TdlgExportWizard.ConvertOrDiscardGridData;
var
  affected: integer;
begin
  FConnection.Execute(SQL_STRIP_MTB_SLASHES, affected);
  if rgOutputMode.ItemIndex = 0 then begin
    // finest resolution selected. MTB QQQ/QYX data stays as it is. Other
    // data is converted.
    ConvertNonGermanDataToOutputSystem;
  end else begin
    // aggregated output selected
    if rgNonGermanGridconversion.ItemIndex=1 then
      DiscardNonGermanGridData;
    ConvertNonGermanDataToOutputSystem;
    ConvertOrDiscardGermanData;
  end;
  DiscardOutOfRangeGridData;
  // Recorder uses 6501/123, whereas we want 65011B3
  FConnection.Execute(SQL_SWITCH_QYX_FORMAT, affected);
end;

procedure TdlgExportWizard.rgGridSystemClick(Sender: TObject);
var
  showRadios: boolean;
begin
  // This option turns on the radio group and hides the checkboxes, or vice versa.
  // If selection is MTB or MTBQ (0 or 1) then the options are different because
  // all data is compatible.
  showRadios := rgGridSystem.ItemIndex >= 2;
  rgGermanGridConversions.Visible:=showRadios;
  gbGermanGridConversions.Visible:=not showRadios;
  rgGermanGridConversions.Items.Clear;
  if rgGridSystem.ItemIndex in [2, 3] then begin
    rgGermanGridConversions.Items.Add(_('Convert MTBQYX data resulting in some imprecise data'));
    rgGermanGridConversions.Items.Add(_('Discard MTBQYX data'));
  end else if rgGridSystem.ItemIndex=4 then begin
    rgGermanGridConversions.Items.Add(_('Convert MTBQQ and MTBQQQ data resulting in some imprecise data'));
    rgGermanGridConversions.Items.Add(_('Discard MTBQQ and MTBQQQ data'));
  end;
  // default is to only include precise data
  if rgGermanGridConversions.Items.Count>0 then
    rgGermanGridConversions.ItemIndex := 0;
end;

(**
 * Depending on the options selected, this either converts or discards the various
 * German grid records. Should only be called for aggregated data.
 *)
procedure TdlgExportWizard.ConvertOrDiscardGermanData;
var
  affected: integer;
begin
  SetProgressLabel(_('Processing German grid data'));
  if (FOutputGridSystem = 'MTB') or (FOutputGridSystem='MTBQ') then begin
    if chkDiscardQYX.Checked then
      FConnection.Execute(Format(SQL_DISCARD_INCOMPATIBLE_GERMAN_DATA, ['QYX']), affected);
    if chkDiscardQQQ.Checked then
      FConnection.Execute(Format(SQL_DISCARD_INCOMPATIBLE_GERMAN_DATA, ['QQQ']), affected);
  end else if (FOutputGridSystem = 'MTBQQ') or (FOutputGridSystem='MTBQQQ') then begin
    if rgGermanGridConversions.ItemIndex=1 then
      // user has opted to discard non-compatible qyx data
      FConnection.Execute(Format(SQL_DISCARD_INCOMPATIBLE_GERMAN_DATA, ['QYX']), affected);
  end else if (FOutputGridSystem = 'MTBQYX') then begin
    if rgGermanGridConversions.ItemIndex=1 then
      // user has opted to discard non-compatible qqq data
      FConnection.Execute(Format(SQL_DISCARD_INCOMPATIBLE_GERMAN_DATA, ['QQQ']), affected);
  end;
  // Anything not discarded by now must be converted
  ConvertGermanDataToOutputSystem;
end;

procedure TdlgExportWizard.DiscardOutOfRangeGridData;
var
  list: TStringList;
  affected: integer;
begin
  SetProgressLabel(_('Removing out of range data'));
  list := TStringList.Create;
  try
    list.Delimiter := ';';
    list.DelimitedText := CleanupRegions(mmMTBsToInclude.Text);
    if list.Count>0 then
      FConnection.Execute('DELETE FROM #interim WHERE LEFT(MTBQ, 4) NOT IN (''' +
          StringReplace(list.CommaText, ',', ''',''', [rfReplaceAll]) +
          ''')', affected)
    else
      // remove anything outside the default valid range for MTB
      FConnection.Execute('DELETE FROM #interim '+
          'WHERE LEFT(MTBQ, 2) NOT BETWEEN ''09'' AND ''87'' '+
          'OR SUBSTRING(MTBQ, 3, 2) NOT BETWEEN ''00'' AND ''56'' ', affected);
  finally
    list.Free;
  end;
end;

procedure TdlgExportWizard.DiscardNonGermanGridData;
var
  notDeleted: string;
  coordSys: TStringList;
  i: integer;
begin
  SetProgressLabel(_('Discarding non-German grid data'));
  coordSys := TStringList.Create;
  if not FileExists(addinPath + '\Atlas Exporter\coordsys.ini') then
    raise Exception.Create('coordsys.ini file missing from ' + addinpath);
  // load the list of non grid systems
  coordSys.LoadFromFile(addinPath + '\Atlas Exporter\coordsys.ini');
  // we also want to keep German grids
  coordSys.Add('QQQ');
  coordSys.Add('QYX');  
  // build a string suitable for an SQL in clause of these systems which are not to be deleted.
  notDeleted := '';
  for i:=0 to coordSys.Count-1 do begin
    if notDeleted <> '' then
      notDeleted := notDeleted + ', ';
    notDeleted := notDeleted + '''' + coordSys[i] + '''';
  end;
  FConnection.Execute(Format(SQL_DISCARD_NONGERMAN_GRID_DATA, [notDeleted]));
end;

procedure TdlgExportWizard.ConvertNonGermanDataToOutputSystem;
var
  rs: _Recordset;
  list: TSpatialSystemList;
  toSys: TSpatialSystem;
  lat, long, newref: WideString;
  affected: integer;

    procedure DeleteDataForCurrentRef;
    begin
      FConnection.Execute('DELETE FROM #interim ' +
          'WHERE SPATREF=''' + rs.Fields['SpatRef'].Value + ''' AND '+
          'SPATREFSYS=''' + rs.Fields['SpatRefSys'].Value + '''', affected);
    end;

begin
  SetProgressLabel(_('Converting non-German grid data'));
  rs := FConnection.Execute('SELECT DISTINCT Lat, Long, SpatRef, SpatRefSys '+
      'FROM #interim '+
      'WHERE SPATREFSYS NOT IN (''QYX'', ''QQQ'')');
  list := TSpatialSystemList.Create;
  toSys := list.GetSystem(FOutputGridCode);
  while not rs.eof do begin
    // triangulate via lat long
    lat := rs.Fields['lat'].Value;
    long := rs.Fields['long'].Value;
    newref := toSys.ConvertFromLatLong(lat, long);
    if Copy(newref, 1, 4)='LTLN' then begin
      // this sref is outside the bounds of the destination system, so we must remove it
      // and notify the user
      DeleteDataForCurrentRef;
      FMessages.Add(_('Some of the data in the output is outside the range of the selected output grid system '+
         'so has been removed from the export.'));
    end else begin
      FConnection.Execute('UPDATE #interim ' +
          'SET MTBQ=''' + newref + ''', '+
          'OUTPUTSYS=''' + FOutputGridCode + ''' '+
          'WHERE SpatRef=''' + rs.Fields['SpatRef'].Value + ''' AND '+
          'SpatRefSys=''' + rs.Fields['SpatRefSys'].Value + '''', affected);
    end;
    ProgressBar.Position := rs.AbsolutePosition * 100 div rs.RecordCount;
    rs.MoveNext;
    // allow window to respond once every so often
    if rs.AbsolutePosition mod 100 = 0 then begin
      Application.ProcessMessages;
      if FClosing then break;
    end;
  end;
end;

procedure TdlgExportWizard.ConvertGermanDataToOutputSystem;
var
  affected: integer;
begin
  if (FOutputGridSystem = 'MTB') or (FOutputGridSystem = 'MTBQ') then begin
    // when outputting MTBQ - the conversion is simple as there is no difference
    // apart from lopping off the last 2 characters (XY or QQ). We won't lop anything
    // off yet though, as that needs to be handled after aggregation
    // todo: check lopping off occurs when not aggregating
    FConnection.Execute('UPDATE o '+
          'SET o.OUTPUTSYS = ''' + FOutputGridCode + ''' '+
          'FROM #interim o '+
          'WHERE o.SPATREFSYS IN (''QYX'', ''QQQ'')', affected);
  end else
  begin
    // conversion is a bit more complex so we use a lookup table
    PrepareConversionTable;
    try
      FConnection.Execute('UPDATE o '+
          'SET o.MTBQ = LEFT(o.MTBQ, 4) + c.toRef, '+
          'o.OUTPUTSYS = ''' + FOutputGridCode + ''' '+
          'FROM #interim o '+
          'INNER JOIN #convertor c ON RIGHT(o.SPATREF, LEN(c.fromRef)) = c.fromRef AND LEN(o.SPATREF) = LEN(c.fromRef)+4'+
          'WHERE o.SPATREFSYS IN (''QYX'', ''QQQ'') '+
          'AND o.SPATREFSYS<>'''+FOutputGridCode+'''', affected);
      // MTBQ conversion is not done by the lookup table, as there is no need. So we just leave this data.
    finally
      FConnection.Execute('DROP TABLE #convertor', affected);
    end;
  end;
end;

procedure TdlgExportWizard.PrepareConversionTable;
var affected, idx: integer;
  conversions: TStringList;

    procedure AddConversion(fromRef, toRef: string);
    begin
      FConnection.Execute('INSERT INTO #convertor VALUES(''' + fromRef + ''',''' + toRef + ''')', affected);
    end;

begin
  conversions := TStringList.Create;
  try
    FConnection.Execute('CREATE TABLE #convertor (fromRef VARCHAR(3) COLLATE Database_Default PRIMARY KEY , '+
        'toref VARCHAR(3) COLLATE Database_Default)', affected);
    if FOutputGridSystem = 'MTBQYX' then
      // converting to QYX
      conversions.Text :=
        '11=111'#13#10'12=114'#13#10'13=131'#13#10'14=134'#13#10'21=211'#13#10+
        '22=214'#13#10'23=231'#13#10'24=234'#13#10'31=311'#13#10'32=314'#13#10+
        '33=331'#13#10'34=334'#13#10'41=411'#13#10'42=414'#13#10'43=431'#13#10+
        '44=434'#13#10'111=111'#13#10'112=112'#13#10'113=121'#13#10'114=122'#13#10+
        '121=114'#13#10'122=115'#13#10'123=124'#13#10'124=125'#13#10'131=121'#13#10+
        '132=122'#13#10'133=131'#13#10'134=132'#13#10'141=124'#13#10'142=125'#13#10+
        '143=134'#13#10'144=135'#13#10'211=211'#13#10'212=212'#13#10'213=221'#13#10+
        '214=222'#13#10'221=214'#13#10'222=215'#13#10'223=224'#13#10'224=225'#13#10+
        '231=221'#13#10'232=222'#13#10'233=231'#13#10'234=232'#13#10'241=224'#13#10+
        '242=225'#13#10'243=234'#13#10'244=235'#13#10'311=311'#13#10'312=312'#13#10+
        '313=321'#13#10'314=322'#13#10'321=314'#13#10'322=315'#13#10'323=324'#13#10+
        '324=325'#13#10'331=321'#13#10'332=322'#13#10'333=331'#13#10'334=332'#13#10+
        '341=324'#13#10'342=325'#13#10'343=334'#13#10'344=335'#13#10'411=411'#13#10+
        '412=412'#13#10'413=421'#13#10'414=422'#13#10'421=414'#13#10'422=415'#13#10+
        '423=424'#13#10'424=425'#13#10'431=421'#13#10'432=422'#13#10'433=431'#13#10+
        '434=432'#13#10'441=424'#13#10'442=425'#13#10'443=434'#13#10'444=435'
    else if FOutputGridSystem = 'MTBQQQ' then
      // converting to QQQ
      conversions.Text :=
        '111=111'#13#10'112=112'#13#10'113=112'#13#10'114=121'#13#10+
        '121=113'#13#10'122=132'#13#10'123=132'#13#10'124=123'#13#10+
        '125=142'#13#10'131=133'#13#10'132=134'#13#10'133=134'#13#10+
        '134=143'#13#10'135=144'#13#10'211=211'#13#10'212=212'#13#10+
        '213=212'#13#10'214=221'#13#10'215=222'#13#10'221=213'#13#10+
        '222=232'#13#10'223=232'#13#10'224=223'#13#10'225=242'#13#10+
        '231=233'#13#10'232=234'#13#10'233=234'#13#10'234=243'#13#10+
        '235=244'#13#10'311=311'#13#10'312=312'#13#10'313=312'#13#10+
        '314=321'#13#10'315=322'#13#10'321=313'#13#10'322=332'#13#10+
        '323=332'#13#10'324=323'#13#10'325=342'#13#10'331=333'#13#10+
        '332=334'#13#10'333=334'#13#10'334=343'#13#10'335=344'#13#10+
        '411=411'#13#10'412=412'#13#10'413=412'#13#10'414=421'#13#10+
        '415=422'#13#10'421=413'#13#10'422=432'#13#10'423=432'#13#10+
        '424=423'#13#10'425=442'#13#10'431=433'#13#10'432=434'#13#10+
        '433=434'#13#10'434=443'#13#10'435=444'
    else if FOutputGridSystem = 'MTBQQ' then
      // converting to QQ
      conversions.Text :=
        '111=11'#13#10'112=11'#13#10'113=11'#13#10'114=12'#13#10'115=12'#13#10+
        '121=11'#13#10'122=13'#13#10'123=13'#13#10'124=12'#13#10'125=14'#13#10+
        '131=13'#13#10'132=13'#13#10'133=13'#13#10'134=14'#13#10'135=14'#13#10+
        '211=21'#13#10'212=21'#13#10'213=21'#13#10'214=22'#13#10'215=22'#13#10+
        '221=21'#13#10'222=23'#13#10'223=23'#13#10'224=22'#13#10'225=24'#13#10+
        '231=23'#13#10'232=23'#13#10'233=23'#13#10'234=24'#13#10'235=24'#13#10+
        '311=31'#13#10'312=31'#13#10'313=31'#13#10'314=32'#13#10'315=32'#13#10+
        '321=31'#13#10'322=33'#13#10'323=33'#13#10'324=32'#13#10'325=34'#13#10+
        '331=33'#13#10'332=33'#13#10'333=33'#13#10'334=34'#13#10'335=34'#13#10+
        '411=41'#13#10'412=41'#13#10'413=41'#13#10'414=42'#13#10'415=42'#13#10+
        '421=41'#13#10'422=43'#13#10'423=43'#13#10'424=42'#13#10'425=44'#13#10+
        '431=43'#13#10'432=43'#13#10'433=43'#13#10'434=44'#13#10'435=44';
    for idx := 0 to conversions.Count-1 do
      AddConversion(conversions.Names[idx], conversions.ValueFromIndex[idx]);
  finally
    conversions.free;
  end;
end;

procedure TdlgExportWizard.ProcessSynonyms;
var
  registry: TRegistry;
  affected: integer;
begin
  SetProgressLabel(_('Processing synonyms'));
  registry := TRegistry.Create;
  try
    registry.RootKey := HKEY_CURRENT_USER;
    registry.OpenKeyReadOnly('Software\Dorset Software\Recorder 6\Settings');
    // Handle synonyms if the option is set
    if rgSynonymHandling.ItemIndex = 1 then begin
      if (registry.ReadBool('Use Recommended Taxon Names')) then
        FConnection.Execute(SQL_CONVERT_TO_PREFERRED_NAMES_NS, affected)
      else
        FConnection.Execute(SQL_CONVERT_TO_PREFERRED_NAMES_ITS, affected);
    end
    else begin
      // even without the option, we still convert common names to preferred names
      if (registry.ReadBool('Use Recommended Taxon Names')) then
        FConnection.Execute(SQL_CONVERT_COMMON_TO_PREFERRED_NAMES_NS, affected)
      else
        FConnection.Execute(SQL_CONVERT_COMMON_TO_PREFERRED_NAMES_ITS, affected);
    end;
  finally
    registry.free;
  end;
end;

procedure TdlgExportWizard.pcWizardChange(Sender: TObject);
begin
  SetButtonState;
end;

procedure TdlgExportWizard.rgOutputModeClick(Sender: TObject);
begin
  lblOutputFinestInstruct.Visible := false;
  lblOutputAggregatedInstruct.Visible := false;
  case rgOutputMode.ItemIndex of
    0: lblOutputFinestInstruct.Visible := true;
    1: lblOutputAggregatedInstruct.Visible := true;
  end;
  // Non-compatible grid handling required when the output mode is aggregation.
  tsNonCompatibleGrids.TabVisible := rgOutputMode.ItemIndex=1;
end;

procedure TdlgExportWizard.pcWizardChanging(Sender: TObject;
  var AllowChange: Boolean);
begin
  try
    ValidateCurrentPage;
  except on e:EValidation do
    begin
      MessageBox(self.Handle, PChar(e.message), PChar(AnsiString(_('Atlas Exporter'))), MB_OK);
      AllowChange := false;
    end;
  end;
end;

procedure TdlgExportWizard.btnSelectDirClick(Sender: TObject);
var
  dir: string;
begin
  dir := eOutputDir.Text;
  FileCtrl.SelectDirectory(dir, [sdAllowCreate], 0);
  eOutputDir.Text := dir;
end;

destructor TdlgExportWizard.Destroy;
begin
  FTranslations.Free;
  FMessages.Free;
  // We MUST set the connection back to its previous state, otherwise the Recorder connection
  // gets destroyed when our connection is garbage collected.
  FConnection.ConnectionObject := FOldConn;
  FConnection.Free;
  if ModalResult <> mrCancel then
    SaveSettings;
  inherited;
end;


procedure TdlgExportWizard.SetProgressLabel(const text: string);
begin
  lblExportProgress.Caption := text;
  Application.ProcessMessages;
end;

(**
 * Outputs a message box. Equivalent of ShowMessage but ensures it is a child of the
 * main window so can't go behind.
 *)
procedure TdlgExportWizard.Msg(const text: string);
begin
  MessageBox(self.Handle, PChar(text), PChar(AnsiString(_('Atlas Exporter'))), MB_OK);
end;

end.
