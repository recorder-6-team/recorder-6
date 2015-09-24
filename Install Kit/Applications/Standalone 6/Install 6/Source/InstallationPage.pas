{===============================================================================
  Unit:        InstallationPage

  Defines:     TfraInstallation

  Description:

  Model:

  Created:     March 2004

  Last revision information:
    $Revision: 15 $
    $Date: 3/07/09 15:26 $
    $Author: Ericsalmon $

===============================================================================}

unit InstallationPage;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, BasePage, ExtCtrls, ComCtrls, StdCtrls, ExceptionForm, InstallFiles,
  InstallAddins, ShortCuts, ServiceHandler, WinSvc;

resourcestring
  ResStr_RecorderRemoveOnly = 'Recorder (remove only)';

type
  EInstallError = class (TExceptionPath);

  TfraInstallation = class (TBasePage)
    Animation: TAnimate;
    lblCancel: TLabel;
    lblFile: TLabel;
    lblInstallStep: TLabel;
    lblOverall: TLabel;
    pbFiles: TProgressBar;
    pbOverall: TProgressBar;
    tmrAnim: TTimer;
    procedure tmrAnimTimer(Sender: TObject);
  private
    FInstallAddins: TInstallAddins;
    FInstallFiles: TInstallFiles;
    procedure ContinueInstall;
    procedure CreateRegistryEntries;
    procedure DoCreateShortcuts(const APathToExe, APathToHelp: String);
    procedure DoInstallAddins;
    procedure DoInstallFiles;
  protected
    function GetNext: TBasePageClass; override;
    function GetResourceImage: String; override;
    procedure LoadContent; override;
  public
    procedure Cancel; override;
    function Execute: Boolean; override;
  end;

//==============================================================================
implementation

{$R *.dfm}

uses
  CompletionPage, GeneralFunctions, Registry,
  SetupConstants, TextMessages, Settings, ComObj, VersionInfo, SHFolder, Functions;

{-==============================================================================
    TfraInstallation
===============================================================================}
{-------------------------------------------------------------------------------
}
procedure TfraInstallation.Cancel;
begin
  lblCancel.Visible := True;
  Refresh;
  
  // Stop copying files now
  if Assigned(FInstallFiles) then
    FInstallFiles.Cancel;
  
  // Stop doing stuff with Addins
  if Assigned(FInstallAddins) then
    FInstallAddins.Cancel;

end;  // TfraInstallation.Cancel

{-------------------------------------------------------------------------------
}
procedure TfraInstallation.CreateRegistryEntries;
var
  reg: TRegistry;
  dataFilePath, userFilePath: String;

  procedure WriteRegString(const AName, AValue: String);
  begin
    lblFile.Caption := AName;
    reg.WriteString(AName, AValue);
    pbFiles.Position := pbFiles.Position + 1;
    Application.ProcessMessages;
  end;

  procedure WriteRegBool(const AName: String; AValue: Boolean);
  begin
    lblFile.Caption := AName;
    reg.WriteBool(AName, AValue);
    pbFiles.Position := pbFiles.Position + 1;
    Application.ProcessMessages;
  end;
  
  procedure WriteDatabaseSetupBatchFile;
  begin
    // Save to batch file, in case registry fails.
    with TStringList.Create do
      try
        Add(Settings.DatabaseSetupCommand);
        SaveToFile(Settings.InstallFolder + 'DatabaseSetup.bat');
        Settings.AddFileName(Settings.InstallFolder + 'DatabaseSetup.bat');
      finally
        Free;
      end;
    // These 2 will be added/created by DatabaseSetup, but will need to be removed
    // by the iuninstaller too.
    Settings.AddFolderName(Settings.InstallFolder + 'Database');
    Settings.AddFileName(Settings.InstallFolder + 'Database\nbndata.mdb');
  end;

  procedure RegisterUninstaller(const ALocationFolder: String);
  begin
    with reg do begin
      RootKey := HKEY_LOCAL_MACHINE;
      Access  := KEY_WRITE;
      if OpenKey(REG_KEY_UNINSTALLER, True) then begin
        WriteString('DisplayName', ResStr_RecorderRemoveOnly);
        WriteString('DisplayIcon', ALocationFolder + STR_RECORDER_SPLASH_EXE);
        WriteString('DisplayVersion', GetFileVersion(Application.ExeName));
        WriteString('InstallDate', DateToStr(Now));
        WriteString('HelpLink', 'http://www.jncc.gov.uk/');
        WriteString('Publisher', 'JNCC');
        WriteString('UninstallString', ALocationFolder + STR_UNINSTALLER);
        WriteBool('NoModify', True);
        WriteBool('NoRepair', True);
        CloseKey;
      end;
    end;
  end;
  
  procedure SetFolderAndRegistry(const ARegKeyName, AFolderName: String);
  begin
    Settings.ForceFolders(AFolderName);
    WriteRegString(ARegKeyName, AFolderName);
  end;
  
begin
  if Settings.Cancelled then Exit;
  
  lblInstallStep.Caption := ResStr_CreateRegistry;
  lblFile.Visible    := True;
  pbFiles.Visible    := True;
  pbFiles.Position   := 0;
  lblOverall.Visible := False;
  pbOverall.Visible  := False;

  Settings.RegistryInstalled := True;
  if Settings.OSVersion >= wvVista then begin
    dataFilePath :=
        GetFolder(CSIDL_COMMON_APPDATA)
        + ExtractFileName(ExcludeTrailingPathDelimiter(Settings.InstallFolder))
        + '\';
    userFilePath :=
        GetFolder(CSIDL_COMMON_DOCUMENTS)
        + ExtractFileName(ExcludeTrailingPathDelimiter(Settings.InstallFolder))
        + '\';
  end else begin
    dataFilePath := Settings.InstallFolder;
    userFilePath := Settings.InstallFolder;
  end;

  reg := TRegistry.Create;
  try
    pbFiles.Max := 27;
    // Current User settings
    if reg.OpenKey(REG_KEY_R6_SETTINGS, True) then begin
      WriteRegString('Base Map Path', Settings.InstallFolder + 'Base Maps\');
      WriteRegString('Dict Images Path', Settings.RootFolder + 'Dictionary Images\');
      WriteRegString('Current Checklist', DEFAULT_CHECKLIST);
      WriteRegBool('Display Common Names', True);
      WriteRegString('DTD Path', Settings.InstallFolder + 'DTD\');
      WriteRegBool('Graduated Menus', True);
      WriteRegString('Help Path', Settings.InstallFolder + 'Help\Rec20HLP.chm');
      WriteRegString('Local Images File Path', userFilePath + 'User Files\User Dictionary Images');
      WriteRegBool('Plain Background', True);
      WriteRegBool('Tool Tips', True);
      WriteRegString('Spatial Ref System', Settings.SpatialRefSystem);
      WriteRegString('Cut Off Date', '1980');
      //Mantis 423/554
      SetFolderAndRegistry('Map File Path',          userFilePath + 'Map Files\');
      SetFolderAndRegistry('Object Sheet File Path', userFilePath + 'Object Sheet\');
      SetFolderAndRegistry('Polygon Filter Path',    userFilePath + 'User Files\Polygon Filter\');
      SetFolderAndRegistry('Recording Card Path',    userFilePath + 'User Files\Recording Cards\');
      SetFolderAndRegistry('Report Path',            userFilePath + 'User Files\Reports\');
      SetFolderAndRegistry('Report Template Path',   userFilePath + 'User Files\Templates\');
      SetFolderAndRegistry('Rucksack Path',          userFilePath + 'User Files\Rucksacks\');
      SetFolderAndRegistry('Snapshot Path',          userFilePath + 'User Files\Snapshots\');

      // Backward compatibility, or addins requirements.
      WriteRegString('Database Password', '');
      reg.CloseKey;
    end else
      raise EInstallError.Create(ResStr_RegistryError);
  
    // Local Machine settings
    reg.RootKey := HKEY_LOCAL_MACHINE;
    reg.Access  := KEY_WRITE;
    if reg.OpenKey(REG_KEY_R6, True) then begin
      WriteRegString('Addin Path', Settings.InstallFolder + 'Addins\');
      WriteRegString('Installation Path', Settings.InstallFolder);
      WriteRegString('Local Data Path', Settings.InstallFolder);
      WriteRegBool('Standalone', True);
      WriteRegString('Database Name', DATABASE_NAME);
      WriteRegString('Server Name', Settings.ProperServerName);
      WriteRegBool('Trusted Security', False);
      reg.CloseKey;
    end else
      raise EInstallError.Create(ResStr_RegistryError);
    // Setup DatabaseSetup batch file for re-run process, if needed.
    WriteDatabaseSetupBatchFile;
    // Uninstaller.
    RegisterUninstaller(Settings.InstallFolder);
  finally
    reg.Free;
  end;
  lblFile.Caption  := '';
  pbFiles.Max      := 100;
  pbFiles.Position := 0;

  Application.ProcessMessages;
end;  // TfraInstallation.CreateRegistryEntries 

{-------------------------------------------------------------------------------
}
procedure TfraInstallation.DoCreateShortcuts(const APathToExe, APathToHelp: String);
begin
  if Settings.Cancelled then Exit;

  lblInstallStep.Caption := ResStr_CreateShortcuts;
  lblFile.Visible    := False;
  pbFiles.Visible    := False;
  lblOverall.Visible := False;
  pbOverall.Visible  := False;
  Refresh;
  Settings.ShortcutsInstalled := True;
  with TShortcuts.Create(R6_START_MENU) do
    try
      ShortcutToGettingStarted(APathToHelp, R6_GUIDE_LINK);
      ShortcutToExe(APathToExe, R6_PROGRAM_LINK);
    finally
      Free;
    end;
  Application.ProcessMessages;
end;  // TfraInstallation.DoCreateShortcuts 

{-------------------------------------------------------------------------------
}
procedure TfraInstallation.DoInstallAddins;
begin
  if Settings.Cancelled then Exit;
  
  try
    lblInstallStep.Caption := ResStr_InstallAddins;
    lblFile.Visible    := True;
    pbFiles.Visible    := True;
    pbFiles.Position   := 0;
    lblOverall.Visible := False;
    pbOverall.Visible  := False;
    Refresh;
  
    FInstallAddins := TInstallAddins.Create(Settings, lblFile, pbFiles);
    try
      FInstallAddins.InstallAddins;
      FInstallAddins.FixAddinButtons;
    finally
      FreeAndNil(FInstallAddins);
    end;
    Application.ProcessMessages;
  except
    on Exception do;
  end;
  Application.ProcessMessages;
end;  // TfraInstallation.DoInstallAddins

{-------------------------------------------------------------------------------
}
procedure TfraInstallation.DoInstallFiles;
var
  winDir: String;
begin
  if Settings.Cancelled then Exit;
  
  lblInstallStep.Caption := ResStr_InstallFiles;
  lblFile.Visible        := True;
  pbFiles.Visible        := True;
  pbFiles.Position       := 0;
  lblOverall.Visible     := True;
  pbOverall.Visible      := True;
  Refresh;
  FInstallFiles := TInstallFiles.Create(Settings, lblFile, pbFiles, pbOverall);
  try
    winDir := IncludeTrailingPathDelimiter(GetWinDir);

    // Copy main files, no dest folder, all go to InstallFolder.
    FInstallFiles.GetFilesIn(Settings.RootFolder + 'Install Files', '');
    FInstallFiles.GetFilesIn(Settings.RootFolder + 'System', '', STR_UNINSTALLER);
    FInstallFiles.GetFilesIn(Settings.RootFolder + 'System', '', STR_UNINSTALLER_LANG);
    FInstallFiles.ZipFiles.Add(
        Settings.RootFolder + STR_ZIPPED_FILES + '=' + Settings.InstallFolder);
    FInstallFiles.ZipFiles.Add(
        Settings.RootFolder + STR_ZIPPED_USER_FILES + '=' + Settings.InstallFolder);        
    FInstallFiles.ZipFiles.Add(
        Settings.RootFolder + 'System\' + STR_BORLAND_PACKAGES + '=' + winDir + 'System32\');

    FInstallFiles.CopyFiles;

    RegisterRecorder(Settings.InstallFolder);
  finally
    FreeAndNil(FInstallFiles);
  end;
  lblFile.Caption  := '';
  pbFiles.Position := 0;
  Application.ProcessMessages;
end;  // TfraInstallation.DoInstallFiles

{-------------------------------------------------------------------------------
}
function TfraInstallation.Execute: Boolean;
var
  lCursor: TCursor;
begin
  Result := False;
  lCursor := AppStartCursor;
  try
    try
      // Proceed with installing the various bits
      CreateRegistryEntries;
      ContinueInstall;
      Result := true;
    except
      on Exception do;
    end;
  finally
    DefaultCursor(lCursor);
  end;
end;  // TfraInstallation.Execute

{-------------------------------------------------------------------------------
  Continues the remaining parts of the install
}
procedure TfraInstallation.ContinueInstall;
var
  lCursor: TCursor;
begin
  lCursor := AppStartCursor;
  try
    DoInstallFiles;
    DoCreateShortcuts(Settings.InstallFolder, Settings.RootFolder);
    DoInstallAddins;
  finally
    DefaultCursor(lCursor);
  end;
end;  // TfraInstallation.ContinueInstall

{-------------------------------------------------------------------------------
}
function TfraInstallation.GetNext: TBasePageClass;
begin
  Result := TfraCompletion;
end;  // TfraInstallation.GetNext

{-------------------------------------------------------------------------------
}
function TfraInstallation.GetResourceImage: String;
begin
  Result := ResImg_Installation;
end;  // TfraInstallation.GetResourceImage

{-------------------------------------------------------------------------------
}
procedure TfraInstallation.LoadContent;
begin
  if Settings.OSVersion >= wvVista then begin
    Settings.SetAnimationFromResource(Animation, ResAvi_VistaFileCopy);
    tmrAnim.Enabled  := False;
    Animation.Active := True;
  end;
end;  // TfraInstallation.LoadContent

{-------------------------------------------------------------------------------
}
procedure TfraInstallation.tmrAnimTimer(Sender: TObject);
begin
  inherited;
  with Animation do begin
    StartFrame := (StartFrame + 1) mod FrameCount;
    Play(StartFrame, StartFrame, 1);
  end;
end;  // TfraInstallation.tmrAnimTimer 


end.
