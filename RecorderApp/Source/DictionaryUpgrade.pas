unit DictionaryUpgrade;

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
    lblBlockSize: TLabel;
    edFileLocation: TEdit;
    lblUpdate: TLabel;
    lblUpdateFile: TLabel;
    Bevel1: TBevel;
    btnDictionaryUpgradeFolder: TButton;
    dlgFolder: TFolderBrowser;
    FolderBrowser1: TFolderBrowser;
    procedure btnActionClick(Sender: TObject);
    procedure bbCancelClick(Sender: TObject);
    procedure btnDictionaryUpgradeFolderClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);

  private
    function AddBackSlash(AFolder: string): string;
    function SelectFolder(const AFolder, AMsg: String): String;
  public
    { Public declarations }
  end;

var
  dlgDictionaryUpgrade: TdlgDictionaryUpgrade;

implementation

uses DatabaseAccessADO, ADODB, Maintbar, GeneralFunctions,GeneralData ;

resourcestring
  ResStr_SelectFolder = 'Select the folder for %s';
  ResStr_DictionaryUpdate = 'Dictionary Updates';
  ResStr_DictionaryUpdateComplete = 'Dictionary update is complete and all indexes rebuilt.';
  ResStr_DictionaryFileProblem = 'Failing to open files. Check that it is not open by another application';
  ResStr_DictionaryFailed  = 'The Dictionary update has completed with errors. ' +
                             'Please try again and if it still fails restore from a backup ' +
                             ' and ask for support. ' ;
  ResStr_DictionaryFileMissing = 'The required file is not in the specified folder.';
  ResStr_Rebuild_Index = 'Do you wish to rebuild the indexes now? If you have other dictionary updates ' +
                         'you may leave the index updates until after they are processed.' +
                         'You may also update the the indexes later as a separate process.';
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
begin
  lBlockSize := strtoint(lblBlockSize.Caption);
  lUpdatedRecords := 0;
  lErrorCount := 0;
  lTablesToProcess := 0;
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
          if leftstr(lrecord,2) = '--' then lTablesToProcess := lTablesToProcess +1;
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
  if lTablesToProcess > 0 then begin
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
      inc(lUpdatedRecords);
      if leftstr(lRecord,2) = '--' then begin
        inc(lTablesProcessed);
        frmMain.setProgress(lTablesProcessed * 100 div lTablesToProcess);
        frmMain.SetStatus(ResStr_DictionaryUpdateStatus + rightstr(lRecord,length(lRecord)-2),true);
      end
      else
          lblock := lblock + lRecord + char(13) + char(10);

      if lUpdatedRecords = lblocksize then
      begin
        try
          with dmGeneralData.qryAllPurpose do begin
            ParseSQL := false;
            SQL.Text := lBlock;
            ExecSQL;
            ParseSQL := true;
          end;
        except
          inc(lErrorCount);
        end;
        lUpdatedRecords := 0;
        lblock  := '';

      end;
    end;
    with dmGeneralData.qryAllPurpose do begin
      ParseSQL := false;
      SQL.Text := lBlock;
      ExecSQL;
      ParseSQL := true;
    end;

    frmMain.SetProgress(0);

    if lErrorCount = 0 then begin
      with dmGeneralData.qryAllPurpose do begin
        ParseSQL := false;
        SQL.Text := 'Update Setting Set DATA = ''' + lUpgradeKey + '''' +
                    ' WHERE NAME = ''Dict Seq ''';
        ExecSQL;
        ParseSQL := true;
        frmMain.SetStatus('');
      end;
      DefaultCursor(lCursor);
      if MessageDlg(ResStr_Rebuild_Index, mtConfirmation, [mbYes,mbNo], 0) = mrYes then begin
        lCursor := HourGlassCursor;
        RebuildIndexTaxonName(dmGeneralData.qryAllPurpose,frmMain.SetStatus,frmMain.SetProgress);
        PopulateTaxonGroupIndex(dmGeneralData.qryAllPurpose,
                               frmMain.SetStatus, frmMain.SetProgress);
        RebuildIndexTaxonSynonym(dmGeneralData.qryAllPurpose,frmMain.SetStatus ,frmMain.SetProgress);
        frmMain.SetStatus(ResStr_DesignationRebuildingStatus);
        frmMain.SetProgress(50);
        dmDatabase.RunStoredProc('usp_Index_Taxon_Designation_Rebuild', []);
        frmMain.SetProgress(0);
      end;
      ShowInformation(ResStr_DictionaryUpdateComplete);
    end
    else begin
      with dmGeneralData.qryAllPurpose do begin
        ParseSQL := false;
        SQL.Text := 'Update Setting Set DATA = ''' + lUpgradeKey + 'Has Errors''' +
                    ' WHERE NAME = ''DictStat''';
        ParseSQL := false;
      end;
      ShowInformation(ResStr_DictionaryFailed);
    end;
    frmMain.SetStatus('');
    CloseFile(lUpdateFile);
    DefaultCursor(lCursor);
    ModalResult:= mrOK;

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
    lblBlockSize.Caption := rs.Fields[0].Value;
  edFileLocation.Text := AppSettings.DictionaryUpgradePath;
  lUpgradeKey :=  dmGeneralData.IdGenerator.GetNextID(lblCurrentKey.Caption);
  lblUpDateFile.Caption := lUpgradeKey + '.sql';
end;

end.
