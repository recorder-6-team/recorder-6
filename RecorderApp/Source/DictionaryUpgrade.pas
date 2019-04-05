unit DictionaryUpgrade;

{The dictionary file requires a check string as the first line. This must start with --}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ImageListButton,FolderBrowser,strUtils,ApplicationSettings, ExtCtrls,DatabaseUtilities;

type
  TdlgDictionaryUpgrade = class(TForm)
    lblFileLocation: TLabel;
    lblCurrentDict: TLabel;
    lblCurrentKey: TLabel;
    btnAction: TButton;
    bbCancel: TImageListButton;
    lblDictStatus: TLabel;
    lblDictCurrentStatus: TLabel;
    lblBlockCaption: TLabel;
    edFileLocation: TEdit;
    lblUpdate: TLabel;
    lblUpdateFile: TLabel;
    Bevel1: TBevel;
    btnDictionaryUpgradeFolder: TButton;
    dlgFolder: TFolderBrowser;
    FolderBrowser1: TFolderBrowser;
    edLicenceKey: TEdit;
    lLicenceKey: TLabel;
    edBlockSize: TEdit;
    procedure btnActionClick(Sender: TObject);
    procedure bbCancelClick(Sender: TObject);
    procedure btnDictionaryUpgradeFolderClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure edBlockSizeKeyPress(Sender: TObject; var Key: Char);

  private
    function AddBackSlash(AFolder: string): string;
    function SelectFolder(const AFolder, AMsg: String): String;
    function LicenceKeyCheck(ALicenceKey,ALicenceKeyCheck : string): boolean;
    function LicenceScramble: String;
    function GetFinancialYear : integer;
    procedure WriteLog(const logentry: ansistring; const app: boolean);
    function GetBlockSize(): integer;
    function GetLogFilename: string;
  public
    { Public declarations }
  end;

var
  dlgDictionaryUpgrade: TdlgDictionaryUpgrade;

implementation

uses DatabaseAccessADO, ADODB, Maintbar, GeneralFunctions,GeneralData;

resourcestring
  ResStr_SelectFolder = 'Select the folder for %s';
  ResStr_DictionaryUpdate = 'Dictionary Updates';
  ResStr_DictionaryUpdateComplete = 'Dictionary update is complete. Indexes were rebuilt if requested.';
  ResStr_DictionaryFileProblem = 'Failing to open files. Check that it is not open by another application';
  ResStr_DictionaryFailed  = 'The Dictionary update has completed with errors. ' +
                             'Please try again using a block size of 1 ';
  ResStr_DictionaryFileMissing = 'The required file is not in the specified folder.';
  ResStr_Rebuild_Index = 'Do you wish to rebuild the indexes now? If you have other dictionary updates ' +
                         'you may leave the index updates until after they are processed.' +
                         'You may also update the the indexes later as a separate process.';
  ResStr_DictionaryFailedOne  = 'The Dictionary update has completed with %s errors. ' + CHAR(13) + CHAR(10) + CHAR(13) + CHAR(10)+
                             'If there are only a few errors it may be safe to use this dictionary, but ' +
                             'you should seek assistance to confirm this.' +  CHAR(13) + CHAR(10) + CHAR(13) + CHAR(10) +
                             'See log file %s ' ;
  ResStr_LicenceKeyInvalid = 'Licence Key Invalid for this update';

  ResStr_BlockSize = 'The block size entered is invalid and will default to 1 ';
  ResStr_All_Indexes_Rebuilt = 'All index table rebuilt';
  ResStr_Licence_Word = 'DINTYGRID';
{$R *.dfm}

procedure TdlgDictionaryUpgrade.btnActionClick(Sender: TObject);
var lCursor : TCursor;
    lUpdateFile:  TextFile;
    lRecord: string;
    lErrorCount: integer;
    lUpdatedRecords: integer;
    lBlock: string;
    lBlockSize: integer;
    lUpGradeKey : string;
    lUpdateFileName :string;
    lTablesProcessed :integer;
    lTablesToProcess :integer;
    lLicenceKeyCheck : string;
begin
  lBlockSize := getBlockSize;
  writelog('[Block Size]' + edBlockSize.Text,false);
  lUpdatedRecords := 0;
  lErrorCount := 0;
  lTablesToProcess := 0;
  lLicenceKeyCheck := '';
  lUpgradeKey :=  dmGeneralData.IdGenerator.GetNextID(lblCurrentKey.Caption);
  lblUpDateFile.Caption := lUpgradeKey + '.sql';
  lTablesProcessed := 0 ;
  lUpdateFileName :=  AddBackSlash(edFileLocation.Text) + lUpgradeKey + '.sql';
  AssignFile(lUpdateFile,lUpdateFileName);
  frmMain.SetStatus(ResStr_DictionaryUpdateCounting);
  Reset(lUpdateFile);
  if not Eof(lUpdateFile) then begin
    while not Eof(lUpdateFile) do
      begin
        Try
          Readln(lUpdateFile, lRecord);
          if leftstr(lrecord,2) = '--' then begin
            if lLicenceKeyCheck = '' then
              lLicenceKeyCheck := lRecord
            else
              inc(lTablesToProcess);
          end;
        Except
          ModalResult := mrNone;
          ShowInformation (ResStr_DictionaryFileProblem);
          lTablesToProcess := 0;
       end;
    end;
  end
  else begin
    ModalResult := mrNone;
    ShowInformation (ResStr_DictionaryFileMissing);
  end;

  if not LicenceKeyCheck(edLicencekey.Text,lLicenceKeyCheck) then begin
    ShowInformation (ResStr_LicenceKeyInvalid);
    lTablesToProcess := 0;
  end;
  if lTablesToProcess > 0   then begin
    lCursor := HourGlassCursor;
    AppSettings.DictionaryUpgradePath := AddBackSlash(edFileLocation.text);
    CloseFile(lUpdateFile);
    Reset(lUpdateFile);
    frmMain.SetStatus(ResStr_DictionaryUpdateStatus);
    lblock := '';
    frmMain.SetProgress (0);
    while not Eof(lUpdateFile) do
    begin
      Readln(lUpdateFile, lRecord);
      if leftstr(lRecord,2) = '--' then begin
        inc(lTablesProcessed);
        frmMain.setProgress(lTablesProcessed * 100 div (lTablesToProcess+2));
        frmMain.SetStatus(ResStr_DictionaryUpdateStatus + rightstr(lRecord,length(lRecord)-2),true);
      end
      else begin
        inc(lUpdatedRecords);
        lblock := lblock + lRecord + char(13) + char(10);
      end;
      if (lUpdatedRecords = lblocksize) and (lblock <> '') then
      begin
        try
          with dmGeneralData.qryAllPurpose do begin
            ParseSQL := false;
            SQL.Text := lBlock;
            ExecSQL;
            ParseSQL := true;
          end;
        except
          WriteLog(lBlock,true);
          inc(lErrorCount);
        end;
        lUpdatedRecords := 0;
        lblock  := '';

      end;
    end;
    if lBlock <> '' then
    begin
      try
        with dmGeneralData.qryAllPurpose do begin
          ParseSQL := false;
          SQL.Text := lBlock;
          ExecSQL;
          ParseSQL := true;
        end;
      except
          WriteLog(lBlock,true);
          inc(lErrorCount);
      end;
    end;

    frmMain.SetProgress(0);
    DefaultCursor(lCursor);
    if lErrorCount = 0 then begin
      with dmGeneralData.qryAllPurpose do begin
        ParseSQL := false;
        SQL.Text := 'Update Setting Set DATA = ''' + lUpgradeKey + '''' +
                    ' WHERE NAME = ''Dict Seq ''';
        ExecSQL;
        SQL.Text := 'Update Setting Set DATA = ''Updated''' +
                    ' WHERE NAME = ''DictStat''';
        ExecSQL;
        ParseSQL := true;
        frmMain.SetStatus('');
      end;

      frmMain.SetStatus('Complete');
      if MessageDlg(ResStr_Rebuild_Index, mtConfirmation, [mbYes,mbNo], 0) = mrYes then begin
       writelog('[Building Indexes]',true);
       lCursor := HourGlassCursor;
        RebuildIndexTaxonName(dmGeneralData.qryAllPurpose,frmMain.SetStatus,frmMain.SetProgress);
        PopulateTaxonGroupIndex(dmGeneralData.qryAllPurpose,
                               frmMain.SetStatus, frmMain.SetProgress);
        RebuildIndexTaxonSynonym(dmGeneralData.qryAllPurpose,frmMain.SetStatus ,frmMain.SetProgress);
        frmMain.SetStatus(ResStr_DesignationRebuildingStatus);
        frmMain.SetProgress(50);
        dmDatabase.RunStoredProc('usp_Index_Taxon_Designation_Rebuild', []);
        frmMain.SetProgress(100);
        frmMain.SetStatus(ResStr_All_Indexes_Rebuilt);
        writelog('[Index Build Complete]',true);
        frmMain.SetProgress(0);
      end;
      writelog('Completed with no Errors to Update ' + lUpgradeKey,true);
      ShowInformation(ResStr_DictionaryUpdateComplete);
      ModalResult:= mrOK;
    end
    else begin
      frmMain.SetStatus('Complete with errors');
      writelog('Completed with errors to Update ' + lUpgradekey,true);
      with dmGeneralData.qryAllPurpose do begin
        ParseSQL := false;
        SQL.Text := 'Update Setting Set DATA = ''' + lUpgradeKey + ' - Partial''' +
                    ' WHERE NAME = ''DictStat''';
        ExecSQL;
        ParseSQL := true;
        lblDictCurrentStatus.caption := lUpgradeKey + ' - Partial';
      end;

      if lBlockSize > 1 then
        ShowInformation(ResStr_DictionaryFailed)
      else
        ShowInformation(Format(ResStr_DictionaryFailedOne,[inttostr(lErrorCount),GetLogFilename]));
      ModalResult:= mrNone;
    end;
    CloseFile(lUpdateFile);

  end;

frmMain.SetStatus('');
frmMain.SetProgress(0);
end;

procedure TdlgDictionaryUpgrade.bbCancelClick(Sender: TObject);
begin
  ModalResult:= mrCancel;
end;

function TdlgDictionaryUpgrade.AddBackSlash(AFolder: string): string;
begin
  Result := AFolder;
  if rightstr(AFolder,1) <> '\' then Result := AFolder + '\';

end;
//==============================================================================
function TdlgDictionaryUpgrade.SelectFolder(const AFolder, AMsg: String): String;
var
  stFolder: String;
begin
  Result := AFolder;
  dlgFolder.Title := Format(ResStr_SelectFolder, [AMsg]);
  stFolder        := AFolder;
  if stFolder <> '' then
	  if stFolder[Length(stFolder)] = '\' then stFolder := Copy(stFolder, 1, Length(stFolder) - 1);
  dlgFolder.Folder := stFolder;
  if dlgFolder.Execute then
    Result := dlgFolder.Folder + '\';
end;  // SelectFolder

procedure TdlgDictionaryUpgrade.btnDictionaryUpgradeFolderClick(
  Sender: TObject);
begin
  edfilelocation.text := SelectFolder(edfilelocation.text, ResStr_DictionaryUpdate);
end;

procedure TdlgDictionaryUpgrade.FormActivate(Sender: TObject);
var  rs : _Recordset;
     lUpgradeKey: string;
begin
  rs := dmDatabase.ExecuteSQL('SELECT Data FROM [Setting] WHERE Name=''Dict Seq''', true);
  lblCurrentKey.Caption := rs.Fields[0].Value;
  rs := dmDatabase.ExecuteSQL('SELECT Data FROM [Setting] WHERE Name=''DictStat''', true);
  if not rs.EOF then
    lblDictCurrentStatus.Caption := rs.Fields[0].Value;
  rs := dmDatabase.ExecuteSQL('SELECT Data FROM [Setting] WHERE Name=''DictBlk''', true);
  if not rs.EOF then
    edBlockSize.text := rs.Fields[0].Value;
  edFileLocation.Text := AppSettings.DictionaryUpgradePath;
  lUpgradeKey :=  dmGeneralData.IdGenerator.GetNextID(lblCurrentKey.Caption);
  lblUpDateFile.Caption := lUpgradeKey + '.sql';
end;

function TdlgDictionaryUpgrade.LicenceKeyCheck(ALicenceKey, ALicenceKeyCheck: string): boolean;
var lScrambleResult: string ;
begin
  Result:= false;
  writelog('[Licence Key]' + edLicenceKey.Text,true);
  writelog('[Licence Check]' + ALicenceKeyCheck,true);
  writelog('[Errors]',true);
  try
    lScrambleResult := LicenceScramble;
    if (pos(lScrambleResult,ALicenceKey) > 0) OR (ALicenceKeyCheck = '--' +  ResStr_Licence_Word)  then
      Result := true;
  except
    Result:= false;
  end;

end;
function TdlgDictionaryUpgrade.LicenceScramble: String;
var
	i: Integer;
	lTotal: Int64;
	lString: String;
	lSpare: String;
  lFinancialYear : integer;
begin
	lTotal := 0;
	Result := '0000';
	// Function is not case sensitive.
	lString  := ResStr_Licence_Word;
  lFinancialYear := GetFinancialYear;
  // Get the FinancialYear
  //Not using a Random number so that algorithm does not depend on delphi
	//  Creates an integer total from the sum of (the ASCII values of each character
	//  in the Key Sting multiplied by the adjusted financial year
  for i := 1 to Length(lString) do
		lTotal := lTotal + (lFinancialYear * Ord(lString[i]));

	// if lTotal is greater than FFFF we want to use the integer remainder of 1Total/4093
	if lTotal > 65535 then
		lTotal := lTotal mod 4093;

	//Convert to Hexadeciamal
	lSpare := IntToHex(lTotal, 4);
	// Swaps the order of the characters round in lSpare
	for i := 1 to 4 do
		Result[i] := lSpare[5 - i];
end;  // LicenceScramble

       //------------------------------------------------------------------------------
// Write logentry to a log file
// The file is opened and closed each time, so should be there even
// if it crashes
//------------------------------------------------------------------------------
procedure TdlgDictionaryUpgrade.WriteLog(const logentry: ansistring; const app: boolean);
var f: TextFile;
    work: string;

begin
  work := GetLogFilename; // file name
  AssignFile(f, work);
  if FileExists(work) then
  begin
    if app then
      Append(f) // append it to end of file
    else
      Rewrite(f) // start at the beginning of the file
  end
  else
  begin
    Rewrite(f)
  end;
  Writeln(f, logentry);
  CloseFile(f);
end;
procedure TdlgDictionaryUpgrade.edBlockSizeKeyPress(Sender: TObject;
  var Key: Char);
begin
  If not (Key in [#8, '0'..'9']) then
    Key := #0;
end;

function TdlgDictionaryUpgrade.GetBlockSize: integer;
begin
  Result := 1;
  try
  if (edBlocksize.Text <> '') and (edBlocksize.Text <> '0') then
    Result := strtoint(edBlocksize.Text);
  except
    ShowInformation(ResStr_BlockSize);
    edBlocksize.text := '1';
  end;
end;

function TdlgDictionaryUpgrade.GetLogFilename: string;
begin
  Result := AddBackSlash(edFileLocation.Text) + 'DictionaryLog.txt'; // file name

end;
function TdlgDictionaryUpgrade.GetFinancialYear : integer;
var
  myDate : TDateTime;
  myYear, myMonth, myDay : Word;
begin
  // Set up the myDate to be now
  myDate := date;
  DecodeDate(myDate, myYear, myMonth, myDay);
  if MyMonth < 4 then
     MyYear := MyYear -1 ;
  Result := MyYear - 1700;

end;
end.
