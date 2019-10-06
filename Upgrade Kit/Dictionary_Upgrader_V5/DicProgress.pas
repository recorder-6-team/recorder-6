unit DicProgress;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,Math,
  BaseFrameUnit, Dialogs, ComCtrls, StdCtrls, ExtCtrls,Settings,AdoDB,strUtils;

type
  TfraProgress = class (TBaseFrame)
    lblWait: TLabel;
    lblProgress: TLabel;
  private
    { Private declarations }
    function GetLogName: string;
    procedure WriteLog(const logentry: ansistring; const app: boolean);
   
  public
    { Public declarations }
    procedure Execute(ASettings: TSettings); override;
  end;

implementation

uses UpdaterFrame, Main;

resourcestring
  ResStr_DictionaryUpdateCounting = 'Checking the update file';
  ResStr_MainProcess = 'Starting main process';
  ResStr_Complete_With_Error = 'Complete with errors';
  ResStr_Complete_No_Errors = 'Dictionary update is complete with no errors.';

{$R *.dfm}
procedure TfraProgress.Execute(ASettings: TSettings);
Var
  curretconnect: TADOConnection;
  rs : _Recordset;
  lUpdateKey: string;
  lUpdateFileText : string;
  lBlockSize : integer;
  lBlock : string;
  lCursor : TCursor;
  lErrorCount : integer;
  lUpdatedRecords: integer;
  lTotalRecords : integer;
  lRecordsDone : integer;
  lUpdateFile:  TextFile;
  lContinue : Boolean;
  lRecord : string;
  lSQl : string;
  lTablesToProcess : integer;
  lTablesProcessed : integer;
begin
  Repaint;
  lContinue := true;
  lUpdateFileText := UpdaterFrame.FCurrentFile;
  writelog('[Update -  Connecting]',true);
  curretconnect := TADOConnection.Create(nil);
  curretconnect.ConnectionString := ASettings.GetConnectionString;
  curretconnect.Connected := True;
  lUpdateKey := UpdaterFrame.FUpdateKey;
  lBlockSize := UpdaterFrame.FCurrentBlock;
  lUpdatedRecords := 0;
  lErrorCount := 0;
  lTablesToProcess := -1;
  lRecord := '';
  AssignFile(lUpdateFile,lUpdateFileText);
  lblProgress.Caption := ResStr_DictionaryUpdateCounting;
  try
    screen.Cursor := crHourGlass;
    Reset(lUpdateFile);
    while not Eof(lUpdateFile) do begin
      Readln(lUpdateFile, lRecord);
      if leftstr(lrecord,2) = '--' then
        inc(lTablesToProcess)
      else
        inc(lTotalRecords)

    end;
    CloseFile(lUpdateFile);
  except
    Screen.Cursor := crArrow;
    MessageDlg(ResStr_DictionaryFileCorrupt, mtError, [mbOk], 0);
    lblProgress.Caption := ResStr_DictionaryFileCorrupt;
    writelog('[Faled to read file]',true);
    lContinue := false;
  end;
  if (lTablesToProcess > 0)  and (lContinue) then begin
    Reset(lUpdateFile);
    lblProgress.Caption := ResStr_MainProcess;
    lblock:= '';
    while not Eof(lUpdateFile) do begin
      Readln(lUpdateFile, lRecord);
        if leftstr(lRecord,2) = '--' then begin
          inc(lTablesProcessed);
          lblProgress.Caption := rightstr(lRecord,length(lRecord)-2);
          Repaint;
        end
        else begin
          inc(lUpdatedRecords);
          lblock := lblock + lRecord + char(13) + char(10);
        end;
        if (lUpdatedRecords = lblocksize) and (lblock <> '') then begin
          try
            lRecordsDone := lRecordsDone * lblocksize;
            curretconnect.Execute(lBlock, cmdText);
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
          curretconnect.Execute(lBlock,cmdText);
        except
          WriteLog(lBlock,true);
          inc(lErrorCount);
        end;
    end;
    Screen.Cursor := crArrow;
      if lErrorCount = 0 then begin
        lSql := 'Update Setting Set DATA = ''' + lUpdateKey + '''' +
                    ' WHERE NAME = ''Dict Seq ''';
        curretconnect.Execute(lsql, cmdText);
        lSql :=  'Update Setting Set DATA = ''Updated''' +
                    ' WHERE NAME = ''DictStat''';
        curretconnect.Execute(lsql, cmdText);
        writelog('Completed with no Errors to Update ' + lUpdateKey,true);
        MessageDlg(ResStr_Complete_No_Errors, mtInformation, [mbOk], 0);
        lblProgress.Caption := ResStr_Complete_No_Errors;
      end
      else begin
        lblProgress.Caption := ResStr_Complete_With_Error;
        writelog('Completed with errors to Update ' + lUpdatekey,true);
        lSQL:= 'Update Setting Set DATA = ''' + lUpdateKey + ' - Partial''' +
                    ' WHERE NAME = ''DictStat''';
        curretconnect.Execute(lsql, cmdText);
        if lBlockSize > 1 then
          MessageDlg(ResStr_DictionaryFailed, mtInformation, [mbOk], 0)
        else
          MessageDlg(Format(ResStr_DictionaryFailedOne,[inttostr(lErrorCount),GetLogName]),mtInformation,[MbOk],0);
     end;
     CloseFile(lUpdateFile);
  end;

  curretconnect.free;
  frmMain.btnProceed.Click;
end; //begin

function  TfraProgress.GetLogName: string;
begin
   Result := leftstr(UpdaterFrame.FCurrentFile,length(UpdaterFrame.FCurrentFile)- 12) +  'DictionaryLog.txt' ;
end;

//------------------------------------------------------------------------------
// Write logentry to a log file
// The file is opened and closed each time, so should be there even
// if it crashes
//------------------------------------------------------------------------------
procedure TfraProgress.WriteLog(const logentry: ansistring; const app: boolean);
var f: TextFile;
    work: string;
begin
  work := GetLogname; // file name
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



end.
