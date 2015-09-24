unit BulkLoaderD7;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  ComObj, Classes, ActiveX, MapToolsD7_TLB, StdVcl, Recorder2000_TLB, Dialogs, Registry,
  SysUtils, INIFiles, Forms, Windows, Variants, VersionInfo, ADODB;

type
  TBulkLoadMaps = class(TAutoObject, IBulkLoadMaps, IRecorderAddin, INewAction, IExecuteAction)
  private
    // Interface to Rec2K
    FRecorder2000: IRecorder2000;
    FConnection : TADOConnection;
    FQuery : TAdoQuery;
    // Other stuff
    FMapPath: string;
    FAddinPath: string;
    FINIFile: TINIFile;
    FCopied, FInstalled: integer;

    function UserSecurityLevel: string;
    procedure LoadFiles;
    procedure InsertMapSetting(const lFileName: string);
    function IsMapInstalled(const lFileName: string): boolean;
    function NextMapOrder: string;
    function GetComputerID: string;
    function OpenNBNdb: boolean;
    function Get_MapPath: string;
    function Get_AddinPath: string;
    function LoadINI: boolean;
  protected
    {IRecorderAddin}
    function Get_Name: WideString; safecall;
    function Get_Description: WideString; safecall;
    function Get_ImageFileName: WideString; safecall;
    procedure Install(const iInstalledFilePath: WideString); safecall;
    {INewAction}
    function Get_ActionCaption: WideString; safecall;
    function Get_Hint: WideString; safecall;
    function Get_DimmedImageFilename: WideString; safecall;
    function Get_DisabledImageFileName: WideString; safecall;
    function Get_ParentMenu: WideString; safecall;
    function Get_CanAddToToolbar: WordBool; safecall;
    {IExecuteAction}
    procedure Execute; safecall;
  public
    destructor Destroy; override;
    property MapPath: string read Get_MapPath;
    property AddinPath: string read Get_AddinPath;
  end;

implementation

uses ComServ;

const BULK_INI = 'MapTools.ini';
      QUOTE = Char(39);

//=========================================================================
// Implement Recorder 2000 interfaces
//=========================================================================
procedure TBulkLoadMaps.Install(const iInstalledFilePath: WideString);
var fromName: string;
begin
     {Copy the ini file}
     fromName := ExtractFilePath(iInstalledFilePath) + BULK_INI;
     If FileExists(fromName) then
     begin
        CopyFile(PChar(fromName), PChar(AddinPath + BULK_INI), False);
        // turn off read-only attribute if it comes from CD
        SetFileAttributes(PChar(AddinPath + BULK_INI), FILE_ATTRIBUTE_NORMAL);
     end;
end;

function TBulkLoadMaps.Get_ActionCaption: WideString;
begin
     Result := 'Bulk load';
end;

function TBulkLoadMaps.Get_CanAddToToolbar: WordBool;
begin
     Result := False;
end;

function TBulkLoadMaps.Get_Description: WideString;
begin
     Result := 'Bulk load GSF files.' + AddinVersion('MapToolsD7.dll');
end;

function TBulkLoadMaps.Get_DimmedImageFilename: WideString;
begin
     Result := '';
end;

function TBulkLoadMaps.Get_DisabledImageFileName: WideString;
begin
     Result := '';
end;

function TBulkLoadMaps.Get_Hint: WideString;
begin
     Result := 'Bulk load maps.';
end;

function TBulkLoadMaps.Get_ImageFileName: WideString;
begin
     Result := '';
end;

function TBulkLoadMaps.Get_Name: WideString;
begin
     Result := 'Bulk load maps';
end;

function TBulkLoadMaps.Get_ParentMenu: WideString;
begin
     Result := 'Map';
end;

//=========================================================================
// This is the call from the menu item that actually does the work
//=========================================================================
procedure TBulkLoadMaps.Execute;
begin
     FRecorder2000 := CreateOLEObject('Recorder2000.AutoApplicationSettings') as IRecorder2000;
     If OpenNBNdb then
         If LoadINI then
         begin
            LoadFiles;
            MessageDlg(IntToStr(FCopied) + ' files were copied to ' + MapPath + #13 +
                       IntToStr(FInstalled) + ' maps were registered.', mtInformation,
                       [mbOK], 0);
            FConnection.Close;
         end;
end;

Function TBulkLoadMaps.OpenNBNdb: boolean;
begin
     try
         // open the connection to the database
         FConnection := TADOConnection.Create(nil);
         FConnection.LoginPrompt := False;
         FConnection.ConnectionString := FRecorder2000.ConnectionString;
         FConnection.Open;
         FRecorder2000.SetApplicationSecurity(FConnection.ConnectionObject);
         //set up a query object
         FQuery := TADOQuery.Create(nil);
         FQuery.Connection := FConnection;
         Result := True;
     except
         Result := False;
     end;
end;

Function TBulkLoadMaps.LoadINI: boolean;
begin
  try
     FINIfile := TINIFile.Create(AddinPath + BULK_INI);
     Result := True;
  except
     Result := False;
  end;
end;

//=========================================================================
// Local routines
//=========================================================================
Procedure TBulkLoadMaps.LoadFiles;
var lOpenDialog: TOpenDialog;
    i: integer;
begin
     lOpenDialog := TopenDialog.Create(nil);
     try
         with lOpenDialog do
         begin
              DefaultExt := 'GSF';
              Filter := 'MapServer files (*.gsf)|*.GSF';
              Options := [ofFileMustExist, ofHideReadOnly, ofAllowMultiSelect,
                         ofPathMustExist];
              If Execute then
              begin
                   FCopied := 0;
                   FInstalled := 0;
                   FRecorder2000.RecorderMainForm.StartProgressBar;
                   For i:= 0 to Files.Count-1 do
                       If not IsMapInstalled(Files[i]) then
                       begin
                            If not FileExists(MapPath + ExtractFileName(Files[i])) then
                            begin
                                 FRecorder2000.RecorderMainForm.StatusText := 'Loading ' + Files[i] + ' ...';
                                 Application.ProcessMessages;
                                 CopyFile(PChar(Files[i]), PChar(MapPath + ExtractFileName(Files[i])), False);
                                 inc(FCopied);
                            end;
                            InsertMapSetting(ExtractFileName(Files[i]));
                            FRecorder2000.RecorderMainForm.Progress := trunc(i/Files.Count*100.0);
                       end;
                   FRecorder2000.RecorderMainForm.StopProgressBar;
              end;
         end;
     finally
         lOpenDialog.Free;
         FRecorder2000.RecorderMainForm.StatusText := '';
     end;
end;

//-----------------------------------------------------------------------------
// Insert the entry into MAP_SHEET for the current user
//-----------------------------------------------------------------------------
Procedure TBulkLoadMaps.InsertMapSetting(const lFileName: string);
var lKey, lName, lExt, lComputerID, lSQL: string;
begin
     lName := lFileName;
     lExt := ExtractFileExt(lName);
     lComputerID := GetComputerID;
     If Length(lExt) > 0 then
        lName := Copy(lName, 1, Length(lName) - Length(lExt));
     lKey := Frecorder2000.GetNextKey('MAP_SHEET');
     lSQL := 'INSERT INTO MAP_SHEET ';
     lSQL := lSQL + '(MAP_SHEET_KEY, USER_ID, SHEET_NAME, FILE_NAME, SHEET_TYPE, ';
     lSQL := lSQL + 'CUT_IN_SCALE, CUT_OUT_SCALE, SHEET_DISPLAYED, ENTERED_BY, ';
     lSQL := lSQL + 'ENTRY_DATE, NEW_DATA, MODIFIED_DATA, DATASET_SHEET_ORDER, COMPUTER_ID) ';
     lSQL := lSQL + 'VALUES ';
     lSQL := lSQL + '(' + Quote + lKey + Quote + ', ';
     lSQL := lSQL + Quote + FRecorder2000.CurrentSettings.UserIDKey + Quote + ', ';
     lSQL := lSQL + Quote + lName + Quote + ',';
     lSQL := lSQL + Quote + MapPath + lFileName + Quote + ',';
     lSQL := lSQL + Quote + '1' + Quote + ', ';
     lSQL := lSQL + Quote + FINIFile.ReadString('BulkLoader', 'CutInScale', '1:250,000') + Quote + ', ';
     lSQL := lSQL + Quote + FINIFile.ReadString('BulkLoader', 'CutOutScale', '1:2,500') + Quote + ', ';
     lSQL := lSQL + Quote + '1' + Quote + ', ';
     lSQL := lSQL + Quote + FRecorder2000.CurrentSettings.UserIDKey + Quote + ', ';
     lSQL := lSQL + Quote + DateToStr(now) + Quote + ', ';
     lSQL := lSQL + Quote + '1' + Quote + ', ';
     lSQL := lSQL + Quote + '1' + Quote + ', ';
     lSQL := lSQL + Quote + NextMapOrder + Quote + ', ';
     lSQL := lSQL + Quote + lComputerID + Quote + ')';

     // now we can do the query
     FQuery.SQL.Text := lSQL;
     If FQuery.ExecSQL > 0 then
        inc(FInstalled);
end;

//-----------------------------------------------------------------------------
// Get user's security level
//-----------------------------------------------------------------------------
function TBulkLoadMaps.UserSecurityLevel: string;
begin
     FQuery.SQL.Text := 'SELECT USER.SECURITY_LEVEL ' +
                        'FROM USER ' +
                        'WHERE (((USER.NAME_KEY)=''' + FRecorder2000.CurrentSettings.UserIDKey + '''));';
     try
        FQuery.Open;
        If FQuery.RecordCount > 0 then
           Result := FQuery.FieldByName('SECURITY_LEVEL').AsString
        else
           Result := '';
     finally
         FQuery.Close;
     end;
end;

//-----------------------------------------------------------------------------
// If this map already installed by the current user
//-----------------------------------------------------------------------------
function TBulkLoadMaps.IsMapInstalled(const lFileName: string): boolean;
begin
     Result := False;
     FQuery.SQL.Text := 'SELECT MAP_SHEET.MAP_SHEET_KEY ' +
                        'FROM MAP_SHEET ' +
                        'WHERE (((MAP_SHEET.USER_ID)=''' + FRecorder2000.CurrentSettings.UserIDKey + ''') ' +
                        'AND ((MAP_SHEET.FILE_NAME)='''  + MapPath + ExtractFileName(lFileName) + '''));';

     try
        FQuery.Open;
        Result := (FQuery.RecordCount > 0);
     finally
        FQuery.Close;
     end;
end;

//-----------------------------------------------------------------------------
//  Get the order number for the next sheet
//-----------------------------------------------------------------------------
function TBulkLoadMaps.NextMapOrder: string;
var iMax: integer;
begin
     iMax := 0;
     FQuery.SQL.Text :=  'SELECT Max(MAP_SHEET.DATASET_SHEET_ORDER) AS MaxOrd ' +
                         'FROM MAP_SHEET ' +
                         'WHERE (((MAP_SHEET.DATASET_SHEET_ORDER)<2000000000));';
     try
        FQuery.Open;
        If FQuery.RecordCount > 0 then
           iMax := FQuery.FieldByName('MaxOrd').AsInteger
        else
           iMax := 0;
     finally
        FQuery.Close;
     end;
     Result := IntToStr(iMax+1);
end;

//-----------------------------------------------------------------------------
//  Get the unique name of this computer
//-----------------------------------------------------------------------------
function TBulkLoadMaps.GetComputerID: string;
var
  len : DWORD;
begin
  len := MAX_COMPUTERNAME_LENGTH + 1;
  SetLength (result, len);
  Windows.GetComputerName (PChar (result), len);
  SetLength (result, len);
end;

destructor TBulkLoadMaps.Destroy;
begin
  If FINIFile <> nil then
     FINIFile.Free;
  If FQuery <> nil then
     FQuery.Free;
  If FConnection <> nil then
     FConnection.Free;
  inherited;
end;

function TBulkLoadMaps.Get_MapPath: string;
var lReg: TRegistry;
begin
  If FMapPath = '' then
  begin
     lReg := TRegistry.Create;
     try
        lReg.OpenKey('Software\JNCC\Recorder\Settings',False);
        FMapPath := lReg.ReadString('Map File Path');
     finally
        lReg.Free;
     end;
  end;
  Result := FMapPath;
end;

function TBulkLoadMaps.Get_AddinPath: string;
var lReg: TRegistry;
begin
  if FAddinPath = '' then
  begin
     lReg := TRegistry.Create;
     try
        lReg.OpenKey('Software\JNCC\Recorder\Settings',False);
        FAddinPath := lReg.ReadString('Addin Path');
     finally
        lReg.Free;
     end;
  end;
  Result := FAddinPath;
end;

initialization
  TAutoObjectFactory.Create(ComServer, TBulkLoadMaps, Class_BulkLoadMaps,
    ciSingleInstance, tmApartment);
end.
