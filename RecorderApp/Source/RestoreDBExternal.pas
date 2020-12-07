unit RestoreDBExternal;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs,strUtils, StdCtrls, ImageListButton, ExtCtrls,FolderBrowser,ApplicationSettings;

type
  TdlgRestoreDBEXternal = class(TForm)
    lblDBLocation: TLabel;
    btnAction: TButton;
    bbCancel: TImageListButton;
    edFileLocation: TEdit;
    btnDatabaseFolder: TButton;
    dlgFolder: TFolderBrowser;
    Label1: TLabel;
    edBackupFile: TEdit;
    procedure btnDatabaseFolderClick(Sender: TObject);
    procedure btnActionClick(Sender: TObject);
    procedure bbCancelClick(Sender: TObject);
  private
    function SelectFolder(const AFolder, AMsg: String): String;
    function TidyPath(AFolder: string): string;
  public
    { Public declarations }
  end;

const
  SQL_RESTORE_EXTERNAL =  'Restore Database NBNDATA from DISK = ''%s'' ' +
                          ' WITH MOVE ''NbnData_data'' TO ''%s'',' +
                          ' MOVE ''NbnData_Log'' TO ''%s''';

resourcestring
  ResStr_DBLocation = 'Database Location';
  ResStr_Restore_Complete = 'Restore Complete';
  ResStr_Restore_Failed = 'Restore Failed. Check file name and path';
  ResStr_CloseWindows = 'Close all Windows befoe restoring';

var
  dlgRestoreDBEXternal: TdlgRestoreDBEXternal;
  lCursor: TCursor;
implementation

uses DatabaseAccessADO,GeneralFunctions, Maintbar;

{$R *.dfm}

procedure TdlgRestoreDBEXternal.btnDatabaseFolderClick(Sender: TObject);
begin
  begin
  edfilelocation.text := SelectFolder(edfilelocation.text, ResStr_DBLocation);
end;
end;
//==============================================================================
function TdlgRestoreDBEXternal.SelectFolder(const AFolder, AMsg: String): String;
var
  stFolder: String;
begin
  Result := AFolder;
  dlgFolder.Title := Format(ResStr_DBLocation, [AMsg]);
  stFolder        := AFolder;
  if stFolder <> '' then
	  if stFolder[Length(stFolder)] = '\' then stFolder := Copy(stFolder, 1, Length(stFolder) - 1);
  dlgFolder.Folder := stFolder;
  if dlgFolder.Execute then
    Result := dlgFolder.Folder + '\';
end;  // SelectFolder

procedure TdlgRestoreDBEXternal.btnActionClick(Sender: TObject);
var backupFile : String;
    databaseFile : String;
    logFile : String;
    dbPath : string;
begin
 lCursor := HourGlassCursor;
 dbPath := TidyPath(edFileLocation.text);
 backupFile := dbPath + 'backup\' + edBackupFile.text;
 databaseFile := dbPath +'data' + '\NBNData_Data.mdf';
 logfile := dbPath +'data' + '\NBNData_Log.ldf';
 if frmMain.MDIChildCount > 0 then
   ShowInformation(ResStr_CloseWindows)
 else begin
   try
   // drop the connection for batch updates so we can restore
     if Assigned(AppSettings.BatchUpdateConnection) then begin
       AppSettings.BatchUpdateConnection.Close;
       AppSettings.BatchUpdateConnection.Free;
       AppSettings.BatchUpdateConnection := nil;
     end;
     dmDatabase.DoRestoreandmove(Format(SQL_RESTORE_EXTERNAL, [backupfile,databaseFile,logFile]));
     AppSettings.UpdateMapWindowSelectors;
     ShowInformation(ResStr_Restore_Complete);
     ModalResult:= mrOK;
   Except
     DefaultCursor(lCursor);
     ShowInformation(ResStr_Restore_Failed);
     ModalResult := MrNone;
   end;
 end;
 DefaultCursor(lCursor);
end;


function TdlgRestoreDBEXternal.TidyPath(AFolder: string): string;
begin
  Result := AFolder;
  if uppercase(rightstr(AFolder,6)) =  '\DATA\' then Result := leftstr(AFolder,length(Afolder)-5);
  if uppercase(rightstr(AFolder,8)) =  '\BACKUP\' then Result := leftstr(AFolder,length(Afolder)-7);
  if rightstr(AFolder,1) <> '\' then Result := AFolder + '\';

end;

procedure TdlgRestoreDBEXternal.bbCancelClick(Sender: TObject);
begin
  ModalResult:= mrCancel;
end;

end.

