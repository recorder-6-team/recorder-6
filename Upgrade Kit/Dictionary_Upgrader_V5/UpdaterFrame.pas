unit UpdaterFrame;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,DatabaseAccessADO,
  StdCtrls, ExtCtrls, Registry,Dialogs, BaseFrameUnit,Settings,FolderBrowser, UpgradeFrame,strutils,ADoDb;



resourcestring
  ResStr_FormatWrong = 'The Licence key is not in the correct format. ';
  ResStr_InvalidKey = 'The Licence key entered is not valid for this upgrade. ';
  ResStr_Key_Required = 'A licence key is required to install this upgrade';
  ResStr_SelectFolder = 'Select the folder for %s';
  ResStr_DictionaryUpdate = 'Dictionary Updates';
  ResStr_DictionaryUpdateComplete = 'Dictionary update is complete. Indexes were rebuilt if requested.';
  ResStr_DictionaryFileProblem = 'Failing to open files. Check that it is not open by another application';
  ResStr_DictionaryFailed  = 'The Dictionary update has completed with errors. ' +
                             'Please try again using a block size of 1 ';
  ResStr_DictionaryFileMissing = 'The required file is not in the specified folder.';
  ResStr_DictionaryFileCorrupt = 'The required file is corrupt.';

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
  ResStr_Folder_Issue = 'Failing to write to selected folder. Make sure you have write permission on the folder';
  ResStr_Licence_Word = 'DINTYGRID';
  // Registry constants
  REG_KEY_DICTIONARY_PATH   = '\Software\Dorset Software\Recorder 6\Settings';
  ResStr_CannotUpdateRegistry = 'Failing to update registry check permission';

type
  TfraDicUpdater = class(TBaseFrame)
    Label2: TLabel;
    lblFileLocation: TLabel;
    lblCurrentDict: TLabel;
    lblCurrentKey: TLabel;
    lblDictStatus: TLabel;
    lblDictCurrentStatus: TLabel;
    lblBlockCaption: TLabel;
    lblUpdate: TLabel;
    lblUpdateFile: TLabel;
    lLicenceKey: TLabel;
    edFileLocation: TEdit;
    btnDictionaryUpgradeFolder: TButton;
    edLicenceKey: TEdit;
    edBlockSize: TEdit;
    dlgFolder: TFolderBrowser;
    procedure btnDictionaryUpgradeFolderClick(Sender: TObject);
    procedure edBlockSizeKeyPress(Sender: TObject; var Key: Char);
 private
    { Private declarations }
    function LicenceScramble: String;
    function GetFinancialYear : integer;
    function CharCheck(const ACode: String): Boolean;
    procedure InternalValidate(ASettings: TSettings; var ACanProceed: boolean);
    function  SelectFolder(const AFolder, AMsg: String): String;
    function GetPreviousFolder:string;
    function UpdateRegistryFolder:boolean;
    function GetNextKey(ID:string):string;
    function IncrementChar(const IncChar: Char): Char;
    function GetBlockSize: integer;
    function AddBackSlash(AFolder: string): string;
    function GetLogFilename: string;
    procedure WriteLog(const logentry: ansistring; const app: boolean);
    function LicenceKeyCheck(ALicenceKey, ALicenceKeyCheck: string): boolean;

 public
    { Public declarations }

   function CreateNextFrame(AOwner: TComponent): TBaseFrame; override;
   procedure Execute(ASettings: TSettings); override;
   procedure Validate(ASettings: TSettings; var ACanProceed: boolean); override;
end;


var FCurrentFile : string;
    FCurrentBlock : integer;
    FUpdateKey : string;

implementation

uses DicProgress;


{$R *.dfm}
 {-------------------------------------------------------------------------------
}
function TfraDicUpdater.CreateNextFrame(AOwner: TComponent): TBaseFrame;
begin
  Result := TfraProgress.Create(AOwner);
end;

function TfraDicUpdater.GetPreviousFolder:string ;
var
  lReg: TRegistry;
begin
  lReg := Tregistry.Create;
  Result :='';
  try
    lReg.RootKey := HKEY_CURRENT_USER;
    if lReg.OpenKeyReadOnly(REG_KEY_DICTIONARY_PATH)then begin
      if lReg.ValueExists('Dictionary Path') then
        Result := lReg.ReadString('Dictionary Path')
    end;
  finally
    lReg.Free;
  end; //Try

end;
function TfraDicUpdater.LicenceScramble: String;
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


function TfraDicUpdater.GetFinancialYear : integer;
var
  myDate : TDateTime;
  myYear, myMonth, myDay : Word;
begin
  // Set up the myDate to be now
  myDate := date;
  DecodeDate(myDate, myYear, myMonth, myDay);
  if MyMonth < 4 then
     MyYear := MyYear - 1 ;
  Result := MyYear - 1700;

end;

function TfraDicUpdater.CharCheck(const ACode: String): Boolean;
var
	i: Integer;
begin
    Result := true;
	  if Length(ACode) <> 8 then Result:= false;
    for i := 1 to length(Acode) do
		  if NOT (ACode[i] in ['0'..'9', 'A'..'Z']) then begin
		    Result := false;
		    Exit;
		  end;
end;  // CharCheck

procedure TfraDicUpdater.Execute(ASettings: TSettings);
Var
  curretconnect: TADOConnection;
  rs : _Recordset;
  lSQL : string;
begin

  curretconnect := TADOConnection.Create(nil);
  curretconnect.ConnectionString := ASettings.GetConnectionString;
  curretconnect.Connected := True;

  lSql :=  ' IF  NOT EXISTS(SELECT * FROM SETTING WHERE ' +
           ' [NAME] = ''DictStat'')' +
           ' Insert Into SETTING ([NAME],DATA,DATA_DEFAULT) VALUES ' +
           ' (''DictStat'',''Original'',''Original'')';
           curretconnect.Execute(lsql, cmdText);

  lSql :=  ' IF  NOT EXISTS(SELECT * FROM SETTING WHERE ' +
           ' [NAME] = ''Dictblk'')' +
           ' Insert Into SETTING ([NAME],DATA,DATA_DEFAULT) VALUES ' +
           ' (''Dictblk'',''20'',''20'')';
           curretconnect.Execute(lsql, cmdText);

  rs :=  curretconnect.Execute('SELECT Data FROM [Setting] WHERE Name=''Dict Seq''', cmdText);
  lblCurrentKey.Caption := rs.Fields[0].Value;
  rs := curretconnect.Execute('SELECT Data FROM [Setting] WHERE Name=''DictStat''', cmdText);
  if not rs.EOF then
    lblDictCurrentStatus.Caption := rs.Fields[0].Value;
  rs := curretconnect.Execute('SELECT Data FROM [Setting] WHERE Name=''DictBlk''', cmdText);
  if not rs.EOF then
    edBlockSize.text := rs.Fields[0].Value;
  FUpdateKey :=  GetNextKey(lblCurrentKey.Caption);
  lblUpDateFile.Caption := FUpdateKey + '.sql';
  edFileLocation.Text :=  GetPreviousFolder;

  curretconnect.free;

end;

procedure TfraDicUpdater.Validate(ASettings: TSettings; var ACanProceed: boolean);
begin
  InternalValidate(ASettings, ACanProceed);
end;

procedure TfraDicUpdater.InternalValidate(ASettings: TSettings; var ACanProceed: boolean);
var
  lTablesToProcess :integer;
  lLicenceKeyCheck : string;
  lUpdateFile: TextFile;
  lRecord: string;
begin
  FCurrentBlock := getBlockSize;
  Try
     writelog('[Block Size]' + edBlockSize.Text,false);
  Except
     MessageDlg(ResStr_Folder_Issue, mtError, [mbOk], 0);
     ACanProceed := false;
  end;
  lTablesToProcess := 0;
  lLicenceKeyCheck := '';
  FUpdateKey := GetNextKey(lblCurrentKey.Caption);
  lblUpDateFile.Caption := FUpdateKey + '.sql';
  FCurrentFile :=  AddBackSlash(edFileLocation.Text) + FUpdateKey + '.sql';
  AssignFile(lUpdateFile,FCurrentFile);
  Try
    Reset(lUpdateFile);
    if not Eof(lUpdateFile) then begin
      while not Eof(lUpdateFile) do
        begin
          Try
            Readln(lUpdateFile, lRecord);
            if leftstr(lrecord,2) = '--' then begin
              if lLicenceKeyCheck = '' then
                lLicenceKeyCheck := lRecord
            end;
          Except
            ACanProceed := false;
            MessageDlg(ResStr_DictionaryFileProblem, mtError, [mbOk], 0);
            lTablesToProcess := 0;
        end;
      end;
      exit;
    end
    else begin
      ACanProceed := false;
      MessageDlg(ResStr_DictionaryFileCorrupt, mtError, [mbOk], 0);
    end;
  Except
    ACanProceed := false;
    MessageDlg(ResStr_DictionaryFileMissing, mtError, [mbOk], 0);
  end;
  if (ACanProceed) and (not LicenceKeyCheck(edLicencekey.Text,lLicenceKeyCheck)) then begin
    ACanProceed := false;
    MessageDlg(ResStr_LicenceKeyInvalid, mtError, [mbOk], 0);
    lTablesToProcess := 0;
  end;
  if (ACanProceed) and (not UpdateRegistryFolder) then begin
    ACanproceed := false;
    MessageDlg(ResStr_CannotUpdateRegistry, mtError, [mbOk], 0);
  end;


end;

procedure TfraDicUpdater.btnDictionaryUpgradeFolderClick(Sender: TObject);
begin
  edfilelocation.text := SelectFolder(edfilelocation.text, ResStr_DictionaryUpdate);
end;

 //==============================================================================
function TfraDicUpdater.SelectFolder(const AFolder, AMsg: String): String;
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


function TfraDicUpdater.UpdateRegistryFolder:boolean;
var
  lReg: TRegistry;
begin
  Result := true;
  lReg := Tregistry.Create;
  try
    lReg.RootKey := HKEY_CURRENT_USER;
    if lReg.OpenKey(REG_KEY_DICTIONARY_PATH,True)then begin
      if lReg.ValueExists('Dictionary Path') then
         lReg.WriteString('Dictionary Path',edFileLocation.text)
      else
         Result := false;
    end;
  finally
    lReg.Free;

  end; //Try

end;
function TfraDicUpdater.GetBlockSize: integer;
begin
  Result := 1;
  try
  if (edBlocksize.Text <> '') and (edBlocksize.Text <> '0') then
    Result := strtoint(edBlocksize.Text);
  except
    MessageDlg(ResStr_BlockSize, mtError, [mbOk], 0);
    edBlocksize.text := '1';
  end;
end;
function TfraDicUpdater.GetNextKey(ID:string):string;
var
  iCurrentChar : Integer;
  chNewChar    : Char;
  newID        : string;
begin
  newID:= '';
  iCurrentChar:= Length(ID);  //Increment last character of ID
  chNewChar:= IncrementChar(ID[iCurrentChar]);
  while (chNewChar = '0') and (iCurrentChar > 0) do //If incremented character was rolled around, increment previous character
  begin
    newID:= chNewChar + newID; //Add rolled around character to the new ID
    Dec(iCurrentChar);
    chNewChar:= IncrementChar(ID[iCurrentChar]);
  end;
  //Add new character to the new ID
  newID:= chNewChar + newID;
  //Add previous characters to the new ID
  NewID:= Copy(ID, 0, iCurrentChar - 1) + newID;
  //Return new ID
  Result:= newID;
end;

//==============================================================================
//Increment a character
function TfraDicUpdater.IncrementChar(const IncChar: Char): Char;
begin
  case IncChar of
    '9': Result:= 'A';
    'Z': Result:= '0'; //Roll around and inc previous char
  else
    Result:= Chr(Ord(IncChar) + 1);
  end;
end;

function TfraDicUpdater.AddBackSlash(AFolder: string): string;
begin
  Result := AFolder;
  if rightstr(AFolder,1) <> '\' then Result := AFolder + '\';

end;
procedure TfraDicUpdater.edBlockSizeKeyPress(Sender: TObject;
  var Key: Char);
begin
  If not (Key in [#8, '0'..'9']) then
  Key := #0;
end;
//------------------------------------------------------------------------------
// Write logentry to a log file
// The file is opened and closed each time, so should be there even
// if it crashes
//------------------------------------------------------------------------------
procedure TfraDicUpdater.WriteLog(const logentry: ansistring; const app: boolean);
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

function TfraDicUpdater.LicenceKeyCheck(ALicenceKey, ALicenceKeyCheck: string): boolean;
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

function TfraDicUpdater.GetLogFilename: string;
begin
  Result := AddBackSlash(edFileLocation.Text) + 'DictionaryLog.txt'; // file name
end;

end.
