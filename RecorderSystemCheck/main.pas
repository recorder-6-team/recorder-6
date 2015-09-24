unit main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Registry, ComObj, ComCtrls, ExtCtrls, ShellAPI,
  Recorder2000_TLB, APIUtils;

type
  TFixerMethod = function(param: string): string of object;

  TfrmMain = class(TForm)
    report: TRichEdit;
    pnlButtons: TPanel;
    btnAttemptFixes: TButton;
    procedure btnAttemptFixesClick(Sender: TObject);
  private
    FErrorCount : integer;
    FPossibleFixCount: integer;
    FAttemptingFix: boolean;
    FInstallPath: string;
    FStandalone: boolean;
    FBaseMapPath: string;
    FMapFilePath: string;
    FObjectSheetFilePath: string;
    FBatchUpdatePath: string;
    FDictImagesPath: string;
    FDTDPath: string;
    FErrorPath: string;
    FHelpPath: string;
    FImportTemplatePath: string;
    FPolygonFilterPath: string;
    FRecordingCardPath: string;
    FReportPath: string;
    FReportTemplatePath: string;
    FRucksackPath: string;
    FSnapshotPath: string;
    function Check(test: boolean; success, fail: string; fixMethod: TFixerMethod=nil; fixParam: string=''): boolean;
    procedure RunAsAdmin;
    function RegisterAddinInstaller(param: string): string;
    function RegisterFile(filename: string): string;
    function ExecAndWait(const ExecuteFile, ParamString: string): boolean;
    function Fixing: boolean;
    function TestAddinInstaller: boolean;
    function TestRecorderServer: boolean;
    procedure TestMapPaths;
    procedure TestOtherPaths;
    procedure ShowEndResult;
    function RegisterRecorderServer(param: string): string;
    function IsDirectoryWriteable(const AName: string): Boolean;
    function DeleteRegistrySetting(param: string): string;
    function AddTrailingSlash(param: string): string;
    function HasCmdLineParam(name: string): boolean;
    procedure CheckRegistryPath(const path, name: string;
      slash, writeable: boolean);
    function RemoveTrailingSlash(param: string): string;
  public
    constructor Create(AOwner: TComponent); override;
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

{ TForm1 }

constructor TfrmMain.Create(AOwner: TComponent);
var
  i: integer;
  reg: TRegistry;
  addinKeys: TStringList;
  stdValLibInstalled: boolean;
  obj: IUnknown;
  validator: IValidation;
  validator6: IValidation6;
begin
  inherited;
  reg := TRegistry.Create;
  addinKeys := TStringList.Create;
  try
    FErrorCount := 0;
    FPossibleFixCount := 0;
    FAttemptingFix := false;
    stdValLibInstalled := false; // check for this one as it is important
    reg.RootKey := HKEY_LOCAL_MACHINE;
    if Check(reg.KeyExists('\SOFTWARE\Dorset Software\Recorder 6\'),
      'Recorder installation detected', 'Recorder installation not detected') then begin
      reg.OpenKeyReadOnly('\SOFTWARE\Dorset Software\Recorder 6');
      if reg.ValueExists('Installation Path') then
        FInstallPath := reg.ReadString('Installation Path')
      else begin
        // installation path not in registry, which is the case on a workstation
        // so use addin path to work it out
        FInstallPath := reg.ReadString('Addin Path');
        FInstallPath := ExtractFilePath(Copy(FInstallPath, 0, Length(FInstallPath)-2));
      end;
      FStandalone := reg.ReadBool('Standalone');
      reg.OpenKeyReadOnly('\SOFTWARE\Dorset Software\Recorder 6\Installed Addins');
      reg.GetKeyNames(addinKeys);
      for i := 0 to addinKeys.Count-1 do begin
        Check(reg.OpenKeyReadOnly('\SOFTWARE\Dorset Software\Recorder 6\Installed Addins\' + addinKeys[i]),
            'Checking addin ' + addinKeys[i],
            'Failed to open registry key HKEY_LOCAL_MACHINE\SOFTWARE\Dorset Software\Recorder 6\Installed Addins\' + addinKeys[i]);
        reg.GetDataType('Installed');
        if reg.ReadString('Installed')='0' then
          report.Lines.Add('Addin is currently disabled')
        else begin
          try
            obj := CreateComObject(StringToGUID(reg.ReadString('ClsID')));
            Check(true, 'This addin is correctly installed.', '');
            if addinKeys[i] = 'StdValLib.Recorder 6 Standard Validation' then begin
              // Do a thorough test on the validation object
              if not Supports(obj, IID_IValidation) then
                raise Exception.Create('The validation library does not support the IValidation interface.');
              if not Supports(obj, IID_IValidation6) then
                raise Exception.Create('The validation library does not support the IValidation6 interface.');
              validator := obj as IValidation;
              validator6 := obj as IValidation6;
              stdValLibInstalled := true;
            end;
          except on E:Exception do
            Check(false, '', 'This addin is not correctly installed. ' +E.Message);
          end;
        end;
      end;
      reg.RootKey := HKEY_CURRENT_USER;
      reg.OpenKeyReadOnly('\Software\Dorset Software\Recorder 6\Settings');
      FBaseMapPath := reg.ReadString('Base Map Path');
      FMapFilePath := reg.ReadString('Map File Path');
      FObjectSheetFilePath := reg.ReadString('Object Sheet File Path');
      FBatchUpdatePath := reg.ReadString('Batch Update Path');
      FDictImagesPath := reg.ReadString('Dict Images Path');
      FDTDPath := reg.ReadString('DTD Path');
      FErrorPath := reg.ReadString('Error Path');
      FHelpPath := reg.ReadString('Help Path');
      FImportTemplatePath := reg.ReadString('Import Template Path');
      FPolygonFilterPath := reg.ReadString('Polygon Filter Path');
      FRecordingCardPath := reg.ReadString('Recording Card Path');
      FReportPath := reg.ReadString('Report Path');
      FReportTemplatePath := reg.ReadString('Report Template Path');
      FRucksackPath := reg.ReadString('Rucksack Path');
      FSnapshotPath := reg.ReadString('Snapshot Path');
      Check(stdValLibInstalled,
          'The Standard Validation Library has been tested and supports the correct interfaces.',
          'The Standard Validation Library addin is not installed, which may allow incorrect data '+
          'to be imported into this database.');
      TestAddinInstaller;
      TestRecorderServer;
      TestMapPaths;
      TestOtherPaths;
    end;
  finally
    reg.Free;
    addinKeys.Free;
  end;
  if FPossibleFixCount>0 then pnlButtons.Visible := true;
  ShowEndResult;
end;

function TfrmMain.TestAddinInstaller: boolean;
begin
  try
    CreateComObject(StringToGUID('{4D0275AE-3517-4BC3-95B1-86645CCEB1EC}'));
    Check(true, 'The Recorder addin installer is correctly installed.', '');
    result := true;
  except on E:EOleSysError do
    begin
      Check(false, '', 'The Recorder addin installer is not correctly installed. ' +E.Message,
          RegisterAddinInstaller);
      result := false;
    end;
  end;
end;

function TfrmMain.TestRecorderServer: boolean;
var
  reg: TRegistry;
begin
  result := false;
  reg := TRegistry.Create;
  try
    reg.RootKey:=HKEY_CLASSES_ROOT;
    if reg.OpenKeyReadOnly('\Recorder2000.AutoApplicationSettings\Clsid') and
       reg.OpenKeyReadOnly('\CLSID\{801EBE85-91CE-11D3-B564-005004B0B698}') then begin
      reg.RootKey := HKEY_LOCAL_MACHINE;
      if  reg.OpenKeyReadOnly('\SOFTWARE\Classes\Recorder2000.AutoApplicationSettings') and
          reg.OpenKeyReadOnly('\SOFTWARE\Classes\CLSID\{801EBE85-91CE-11D3-B564-005004B0B698}') then
        result := true;
    end;
  finally
    reg.free;
  end;
  Check(result, 'The Recorder OLE Server is correctly installed.',
      'The Recorder OLE Server is not correctly installed.', RegisterRecorderServer);
end;

function TfrmMain.RegisterRecorderServer(param: string): string;
begin
  WinExecAndWait32(
      FInstallPath + 'RecorderApp.exe' + ' /regserver',
      FInstallPath,
      SW_SHOW);
  result := 'Registered the Recorder OLE Server';
end;

(**
 * Performs a check and reports the success or fail message. Returns true if OK.
 *)
function TfrmMain.Check(test: boolean; success, fail: string; fixMethod: TFixerMethod=nil; fixParam: string=''): boolean;
begin
  if test then begin
    report.SelAttributes.Color := clWindowText;
    report.SelAttributes.Style := [];
    report.Lines.Add('OK - ' + success);
    result := true;
  end
  else begin
    // when fixing things, don't display problems till we've tried to fix them
    if Fixing and (not FAttemptingFix) and assigned(fixMethod) then
      report.SelAttributes.Color := clWindowText
    else
      report.SelAttributes.Color := clRed;
    report.SelAttributes.Style := [fsBold];
    report.Lines.Add('Error - ' + fail);
    Inc(FErrorCount);
    // Is an auto-fix available
    if Fixing and not FAttemptingFix then begin
      if assigned(fixMethod) then begin
        // set a flag so a failed fix does not try to self-fix!
        FAttemptingFix := true;
        report.SelAttributes.Color := clGreen;
        report.Lines.Add(fixMethod(fixParam));
        FAttemptingFix := false;
      end
      else
        report.Lines.Add('No automatic fix for this problem is available.');
    end else if assigned(fixMethod) then begin
      report.Lines.Add('Click Attempt Fixes to try to fix this problem automatically.');
      FPossibleFixCount := FPossibleFixCount + 1;
    end;
    result := false;
  end;
end;

procedure TfrmMain.ShowEndResult;
begin
  if FErrorCount=0 then begin
    report.SelAttributes.Color := clGreen;
    report.SelAttributes.Style := [fsBold];
    report.Lines.Add('No problems were detected');
  end
  else begin
    report.SelAttributes.Color := clRed;
    report.SelAttributes.Style := [fsBold];
    report.Lines.Add(IntToStr(FErrorCount) + ' error(s) were encountered during this check. Please review the details above.');
  end;
end;

function TfrmMain.Fixing: boolean;
begin
  result := HasCmdLineParam('/f');
end;

procedure TfrmMain.btnAttemptFixesClick(Sender: TObject);
begin
  RunAsAdmin;
end;

procedure TfrmMain.RunAsAdmin;
{
    See Step 3: Redesign for UAC Compatibility (UAC)
    http://msdn.microsoft.com/en-us/library/bb756922.aspx
}
var
  Info: TShellExecuteInfo;
begin
  FillChar(Info, SizeOf(Info), 0);
  Info.cbSize := SizeOf(TShellExecuteInfoW);
  Info.fMask := SEE_MASK_FLAG_NO_UI or SEE_MASK_FLAG_DDEWAIT or
    SEE_MASK_NOCLOSEPROCESS;
  Info.lpVerb := 'runas';
  Info.lpFile := PChar(Application.Exename);
  Info.lpParameters := PChar('/f');
  Info.nShow := SW_SHOWNORMAL;
  if not ShellExecuteEx(@Info) then begin
    { Don't display error message if user clicked Cancel at UAC dialog }
    if GetLastError = ERROR_CANCELLED then
      Abort
    else
      raise Exception.Create('Failed to restart the application running as administrator');
  end;
  if Info.hProcess = 0 then
    raise Exception.Create('ShellExecuteEx returned hProcess=0');
  Close;
end;

function TfrmMain.RegisterAddinInstaller(param: string): string;
begin
  RegisterFile(FInstallPath + 'RecorderAddInInstaller.dll');
  result := 'Addin Installer library registered';
end;

function TfrmMain.RegisterFile(filename: string): string;
type
  TRegFunc = function : HResult; stdcall;
var
  ARegFunc : TRegFunc;
  aHandle  : THandle;
begin
  aHandle := LoadLibrary(PChar(filename));
  if aHandle <> 0 then
  begin
    ARegFunc := GetProcAddress(aHandle,'DllRegisterServer');
    if Assigned(ARegFunc) then
    begin
      if not ExecAndWait('regsvr32','/s "' + filename + '"') then
        report.Lines.Add('Could not call regsvr32');
    end else
      report.Lines.Add('No DLLRegisterServer method found in ' + filename);
    FreeLibrary(aHandle);
  end;
  result := 'File registered';
end;

function TfrmMain.ExecAndWait(const ExecuteFile, ParamString : string): boolean;
var
  SEInfo: TShellExecuteInfo;
  ExitCode: DWORD;
begin
  FillChar(SEInfo, SizeOf(SEInfo), 0);
  SEInfo.cbSize := SizeOf(TShellExecuteInfo);
  with SEInfo do begin
    fMask := SEE_MASK_NOCLOSEPROCESS;
    Wnd := Application.Handle;
    lpFile := PChar(ExecuteFile);
    lpParameters := PChar(ParamString);
    nShow := SW_HIDE;
  end;
  if ShellExecuteEx(@SEInfo) then
  begin
    repeat
      Application.ProcessMessages;
      GetExitCodeProcess(SEInfo.hProcess, ExitCode);
    until (ExitCode <> STILL_ACTIVE) or Application.Terminated;
    Result:=True;
  end
  else Result:=False;
end;

(**
 * Method that checks that the map paths all point to the correct places with appropriate
 * access rights.
 *)
procedure TfrmMain.TestMapPaths;
var
  sr: TSearchRec;
  installdrive, drive: PAnsiChar;
  iniFile: TStringList;
begin
  Check(FBaseMapPath<>'',
      'The Base Maps Path setting is set.',
      'The Base Maps Path registry setting is not set. Please log in to Recorder then close it before continuing with the '+
          'Recorder System Check tool so that it can update it''s settings.');
  Check(FMapFilePath<>'',
      'The Map File Path setting is set.',
      'The Map File Path registry setting is not set. Please log in to Recorder then close it before continuing with the '+
          'Recorder System Check tool so that it can update it''s settings.');
  Check(FObjectSheetFilePath<>'',
      'The Object Sheet File Path setting is set.',
      'The Object Sheet File Path registry setting is not set. Please log in to Recorder then close it before continuing with the '+
          'Recorder System Check tool so that it can update it''s settings.');

  (*====
  In the following tests, do NOT nest the 'If ...<>'' then' statements into a single begin..end block
  because the fixer methods may set the values back to empty therefore the subsequent tests become
  unnecessary.
  ====*)

  // Check base maps dir exists, has a trailing slash and contains at least 1 gsf file
  if FBaseMapPath<>'' then
    Check(DirectoryExists(FBaseMapPath),
        'The Base Maps Path exists.',
        'The Base Maps Path registry setting points to a missing directory.',
        DeleteRegistrySetting, 'Base Map Path');
  if (FBaseMapPath<>'') and DirectoryExists(FBaseMapPath) then
    Check(IncludeTrailingPathDelimiter(FBaseMapPath)=FBaseMapPath,
        'The Base Map Path setting includes the trailing slash.',
        'The Base Map Path setting should include the trailing slash or maps cannot work.',
        AddTrailingSlash, 'Base Map Path');
  if (FBaseMapPath<>'') and DirectoryExists(FBaseMapPath) and (IncludeTrailingPathDelimiter(FBaseMapPath)=FBaseMapPath) then
    Check(FindFirst(FBaseMapPath + '*.gsf', faAnyFile, sr) = 0,
        'The Base Maps Path points to a folder containing base maps.',
        'The Base Maps Path points to a folder that does not contain any base maps. '+
        'Please check why the path '+FBaseMapPath+' does not contain the map files.');
  // Check map files dir exists, is writeable and has a trailing slash
  if FMapFilePath<>'' then
    Check(DirectoryExists(FMapFilePath),
        'The Map File Path exists.',
        'The Map File Path registry setting points to a missing directory.',
        DeleteRegistrySetting, 'Map File Path');
  if (FMapFilePath<>'') and DirectoryExists(FMapFilePath) then
    Check(IsDirectoryWriteable(FMapFilePath),
        'The Map File Path is writeable.',
        'The Map File Path registry setting points to a directory that cannot be written to, so maps will not work correctly.');
  if (FMapFilePath<>'') and DirectoryExists(FMapFilePath)  then
    Check(IncludeTrailingPathDelimiter(FMapFilePath)=FMapFilePath,
        'The Map File Path setting includes the trailing slash.',
        'The Map File Path setting should include the trailing slash or maps cannot work.',
        AddTrailingSlash, 'Map File Path');
  // Check object sheet dir exists, is writeable and has a trailing slash
  if FObjectSheetFilePath<>'' then
    // Check object sheets dir exists and is writeable
    Check(DirectoryExists(FObjectSheetFilePath),
        'The Object Sheet File Path exists.',
        'The Object Sheet File Path registry setting points to a missing directory.',
        DeleteRegistrySetting, 'Object Sheet File Path');
  if (FObjectSheetFilePath<>'') and DirectoryExists(FObjectSheetFilePath) then
    Check(IsDirectoryWriteable(FObjectSheetFilePath),
        'The Object Sheet File Path is writeable.',
        'The Object Sheet File Path registry setting points to a directory that cannot be written to, so maps will not work correctly.');
  if (FObjectSheetFilePath<>'') and DirectoryExists(FObjectSheetFilePath) then
    Check(IncludeTrailingPathDelimiter(FObjectSheetFilePath)=FObjectSheetFilePath,
        'The Object Sheet File Path setting includes the trailing slash.',
        'The Object Sheet File Path setting should include the trailing slash or maps cannot work.',
        AddTrailingSlash, 'Object Sheet File Path');
  if not FStandalone then begin
    if (FBaseMapPath<>'') and DirectoryExists(FBaseMapPath) then begin
      if IncludeTrailingPathDelimiter(FBaseMapPath)=FBaseMapPath then begin
        // test we can write to the base maps dir on startup of Recorder (in case the
        // maps have been upgraded centrally). Do this by opening one of the ini files
        // and re-writing it, since ACL code is a mess.
        if FindFirst(FBaseMapPath + '*.ini', faAnyFile, sr) = 0 then begin
          iniFile := TStringList.Create;
          try
            try
              // if a force parameter for this test has been passed on the command line, then
              // simlulate failure. This is because when running as admin we cannot detect
              // permissions issues properly.
              if HasCmdLineParam('/b') then
                raise EFCreateError.CreateFmt('', []);
              iniFile.LoadFromFile(FBaseMapPath + sr.Name);
              iniFile.SaveToFile(FBaseMapPath + sr.Name);
              iniFile.Clear;
              iniFile.LoadFromFile(FBaseMapPath + sr.Name);
              Check(true, 'Files in the Base Maps folder are writeable and can be upgraded as required.', '');
            except
              on EFCreateError do begin
                Check(false, '', 'The files in the Base Maps directory are not writeable without admin permissions. This means '+
                    'that normal users will not be able to start Recorder. It could be because you initially ran '+
                    'Recorder on this workstation using admin permissions. One way to fix this is to '+
                    'delete the '+FBaseMapPath+' folder from the disk and also delete the Base Map Path setting '+
                    'from the Recorder registry settings. Then you must run Recorder as a normal user (i.e. do '+
                    'not Run as Administrator) to copy the files back with the correct permissions.');
              end;
            end;
          finally
            iniFile.Free;
          end;
        end else
          Check(false, '', 'The Base Map folder does not appear to be configured correctly as it contains '+
              'no *.ini files to define the base maps.');
      end;
      // for network install, base maps should be writeable so it can copy over files after upgrade
      Check(IsDirectoryWriteable(FBaseMapPath),
          'The local copy of the Base Map Path is writeable.',
          'The Base Map Path registry setting points to a directory that cannot be written to, so Recorder will fail when updating base maps on startup.');
      // there should be a networked copy of the folder to copy from
      Check(DirectoryExists(FInstallPath + 'Base Maps\'),
          'The network Recorder installation folder contains a Base Maps directory.',
          'The network Recorder installation folder should contain a Base Maps directory. Please replace '+
          'this folder and include the base map files you want to make available.');
      // the network base map folder should be populated with maps
      if DirectoryExists(FInstallPath + 'Base Maps\') then
        Check(FindFirst(FInstallPath + 'Base Maps\*.gsf', faAnyFile, sr) = 0,
            'The Base Maps folder in the network installation folder contains base maps.',
            'The Base Maps folder in the network installation folder does not contain any base maps. '+
            'Please check why the path '+FBaseMapPath+' does not contain the map files.');
      // the local and network copies should not be the same folder.
      Check(FBaseMapPath <> FInstallPath + 'Base Maps\',
          'The Base Maps path is pointing to the installation Base Maps folder.',
          'The Base Maps path is pointing to the installation Base Maps folder. Normally each workstation should have its own Base Maps folder.',
          DeleteRegistrySetting, 'Base Map Path');
      // The local copy should not be on a network drive for performance reasons. Recheck if it is
      // empty in case the previuos fixer method emptied it.
      if FBaseMapPath<>'' then begin
        drive := PAnsiChar(Copy(FBaseMapPath, 1, 3));
        Check(GetDriveType(drive)<>DRIVE_REMOTE,
            'The Base Maps Path setting is pointing to a local folder.',
            'The Base Maps Path is pointing to a network folder. Normally each workstation should have a local copy of the Base Maps folder.');
      end;
    end;
    if FObjectSheetFilePath<>'' then begin
      installdrive := PAnsiChar(Copy(FInstallPath, 1, 3));
      drive := PAnsiChar(Copy(FObjectSheetFilePath, 1, 3));
      // we expect the object sheet files to be on the network if this really is a network install
      // (i.e. not a standalone pc which used the network installer).
      Check((GetDriveType(drive)=DRIVE_REMOTE) or (GetDriveType(installdrive)<>DRIVE_REMOTE) ,
          'The Object Sheet File Path setting is pointing to a network folder.',
          'The Object Sheet File Path is pointing to a local folder. Normally each workstation be setup to share a single network copy of the Object Sheet File Path folder.',
          DeleteRegistrySetting, 'Object Sheet File Path');
    end;
    if FMapFilePath<>'' then begin
      drive := PAnsiChar(Copy(FMapFilePath, 1, 3));
      Check(GetDriveType(drive)<>DRIVE_REMOTE,
          'The Map File Path setting is pointing to a local folder.',
          'The Map File Path is pointing to a network folder. Normally each workstation be setup to have a local copy of the Map File Path folder.',
          DeleteRegistrySetting, 'Map File Path');
    end;
  end;
end;

function TfrmMain.HasCmdLineParam(name: string): boolean;
var
  i: integer;
begin
  result := false;
  for i := 1 to ParamCount do
    if ParamStr(i)=name then
      result := true;
end;

(**
 * Method to check write access to a directory, by writing a temporary file.
 *)
function TfrmMain.IsDirectoryWriteable(const AName: string): Boolean;
var
  FileName: String;
  H: THandle;
begin
  FileName := IncludeTrailingPathDelimiter(AName) + 'chk.tmp';
  H := CreateFile(PChar(FileName), GENERIC_READ or GENERIC_WRITE, 0, nil,
    CREATE_NEW, FILE_ATTRIBUTE_TEMPORARY or FILE_FLAG_DELETE_ON_CLOSE, 0);
  Result := H <> INVALID_HANDLE_VALUE;
  if Result then CloseHandle(H);
end;

(**
 * Generic fixer method that removes a registry setting. Recorder should then be
 * run as it recreates any missing registry settings on startup.
 *)
function TfrmMain.DeleteRegistrySetting(param: string): string;
var
  reg: TRegistry;
begin
  reg := TRegistry.Create;
  try
    reg.RootKey := HKEY_CURRENT_USER;
    reg.OpenKey('\Software\Dorset Software\Recorder 6\Settings', false);
    reg.DeleteValue(param);
  finally
    reg.Free;
  end;
  if param='Base Map Path' then
    FBaseMapPath:='';
  if param='Map File Path' then
    FMapFilePath:='';
  if param='Object Sheet File Path' then
    FObjectSheetFilePath:='';
  result := 'This Recorder setting has been removed. It will be recreated when you next run Recorder. '+
      'Please close this application, then log in to Recorder and then close it to update the settings. '+
      'Finally run this Recorder System Check tool again to check that the fix has worked.';
end;

(**
 * Generic fixer method that adds a trailing slash to a setting.
 *)
function TfrmMain.AddTrailingSlash(param: string): string;
var
  reg: TRegistry;
  val: string;
begin
  reg := TRegistry.Create;
  try
    reg.RootKey := HKEY_CURRENT_USER;
    reg.OpenKey('\Software\Dorset Software\Recorder 6\Settings', false);
    val := reg.ReadString(param);
    reg.WriteString(param, IncludeTrailingPathDelimiter(val));
  finally
    reg.Free;
  end;
  if param='Base Map Path' then
    FBaseMapPath:=IncludeTrailingPathDelimiter(val);
  if param='Map File Path' then
    FMapFilePath:=IncludeTrailingPathDelimiter(val);
  if param='Object Sheet File Path' then
    FObjectSheetFilePath:=IncludeTrailingPathDelimiter(val);
  if param='Batch Update Path' then
    FBatchUpdatePath:=IncludeTrailingPathDelimiter(val);
  if param='DTD Path' then
    FDTDPath:=IncludeTrailingPathDelimiter(val);
  if param='Error Path' then
    FErrorPath:=IncludeTrailingPathDelimiter(val);
  if param='Help Path' then
    FHelpPath:=IncludeTrailingPathDelimiter(val);
  if param='Import Template Path' then
    FImportTemplatePath:=IncludeTrailingPathDelimiter(val);
  if param='Polygon Filter Path' then
    FPolygonFilterPath:=IncludeTrailingPathDelimiter(val);
  if param='Recording Card Path' then
    FRecordingCardPath:=IncludeTrailingPathDelimiter(val);
  if param='Report Path' then
    FReportPath:=IncludeTrailingPathDelimiter(val);
  if param='Report Template Path' then
    FReportTemplatePath:=IncludeTrailingPathDelimiter(val);
  if param='Rucksack Path' then
    FRucksackPath:=IncludeTrailingPathDelimiter(val);
  if param='Snapshot Path' then
    FSnapshotPath:=IncludeTrailingPathDelimiter(val);
  result := 'The Recorder setting has been corrected.';
end;

(**
 * Generic fixer method that removes a trailing slash from a setting.
 *)
function TfrmMain.RemoveTrailingSlash(param: string): string;
var
  reg: TRegistry;
  val: string;
begin
  reg := TRegistry.Create;
  try
    reg.RootKey := HKEY_CURRENT_USER;
    reg.OpenKey('\Software\Dorset Software\Recorder 6\Settings', false);
    val := reg.ReadString(param);
    reg.WriteString(param, ExcludeTrailingPathDelimiter(val));
  finally
    reg.Free;
  end;
  if param='Dict Images Path' then
    FDictImagesPath:=ExcludeTrailingPathDelimiter(val);
  result := 'The Recorder setting has been corrected.';
end;

procedure TfrmMain.TestOtherPaths;
begin
  CheckRegistryPath(FBatchUpdatePath, 'Batch Update Path', true, true);
  CheckRegistryPath(FDictImagesPath, 'Dict Images Path', false, false);
  CheckRegistryPath(FDTDPath, 'DTD Path', true, false);
  CheckRegistryPath(FErrorPath, 'Error Path', true, true);
  CheckRegistryPath(FHelpPath, 'Help Path', true, false);
  CheckRegistryPath(FImportTemplatePath, 'Import Template Path', true, true);
  CheckRegistryPath(FPolygonFilterPath, 'Polygon Filter Path', true, true);
  CheckRegistryPath(FRecordingCardPath, 'Recording Card Path', true, true);
  CheckRegistryPath(FReportPath, 'Report Path', true, true);
  CheckRegistryPath(FRucksackPath, 'Rucksack Path', true, true);
  CheckRegistryPath(FSnapshotPath, 'Snapshot Path', true, true);
end;

procedure TfrmMain.CheckRegistryPath(const path: string; const name: string; slash, writeable: boolean);
begin
  if path<>'' then
    Check(DirectoryExists(path),
        'The ' + name + ' exists.',
        'The ' + name + ' registry setting points to a missing directory.',
        DeleteRegistrySetting, name);
  if (path<>'') and DirectoryExists(path) then
    if slash then
      Check(IncludeTrailingPathDelimiter(path)=path,
          'The ' + name + ' setting includes the trailing slash.',
          'The ' + name + ' setting should include the trailing slash.',
          AddTrailingSlash, name)
    else
      Check(ExcludeTrailingPathDelimiter(path)=path,
          'The ' + name + ' setting has no trailing slash as expected.',
          'The ' + name + ' setting should not include the trailing slash.',
          RemoveTrailingSlash, name);
  if (path<>'') and DirectoryExists(path) and writeable then
    Check(IsDirectoryWriteable(path),
        'The ' + name + ' is writeable.',
        'The ' + name + ' registry setting points to a directory that cannot be written to, which might break related functionality in Recorder.');
end;

end.
