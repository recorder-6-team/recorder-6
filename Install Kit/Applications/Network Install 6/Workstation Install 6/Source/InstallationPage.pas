{===============================================================================
  Unit:        InstallationPage

  Defines:     TfraInstallation

  Description:

  Model:       Workstation Install 6.mpb

  Created:     November 2004

  Last revision information:
    $Revision: 8 $
    $Date: 12/05/09 14:00 $
    $Author: Ericsalmon $

===============================================================================}

unit InstallationPage;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, BasePage, ExtCtrls, ComCtrls, StdCtrls, ExceptionForm, InstallFiles,
  InstallAddins, ShortCuts, WinSvc;

type
  EInstallError = class(TExceptionPath)
  end;
  
  TfraInstallation = class(TBasePage)
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
  CompletionPage, GeneralFunctions, Registry, SetupConstants, TextMessages,
  Settings, Functions;

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
  lReg: TRegistry;
  
  procedure WriteRegString(const AName, AValue: String);
  begin
    lblFile.Caption := AName;
    lReg.WriteString(AName, AValue);
    pbFiles.Position := pbFiles.Position + 1;
    Application.ProcessMessages;
  end;
  
  procedure WriteRegBool(const AName: String; AValue: Boolean);
  begin
    lblFile.Caption := AName;
    lReg.WriteBool(AName, AValue);
    pbFiles.Position := pbFiles.Position + 1;
    Application.ProcessMessages;
  end;
  
  procedure RegisterUninstaller(const ARootFolder, ALocationFolder: String);
  begin
    with lReg do begin
      RootKey := HKEY_LOCAL_MACHINE;
      Access  := KEY_ALL_ACCESS;
      if OpenKey(REG_KEY_UNINSTALLER, True) then begin
        WriteString('DisplayName', 'Recorder 6 Workstation (remove only)');
        WriteString('DisplayIcon', ARootFolder + 'Recorder.exe');
        WriteString('DisplayVersion', '1.0.0.0');
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
  lblFile.Visible        := True;
  pbFiles.Visible        := True;
  pbFiles.Position       := 0;
  lblOverall.Visible     := False;
  pbOverall.Visible      := False;
  
  Settings.RegistryInstalled := True;
  lReg := TRegistry.Create;
  try
    pbFiles.Max := 27;
    // Current User settings
    if lReg.OpenKey(REG_KEY_R6_SETTINGS, True) then begin
      WriteRegString('Dict Images Path', Settings.RootFolder + 'Dictionary Images\');
      WriteRegString('Current Checklist', 'NBNSYS0000000074');
      WriteRegBool('Display Common Names', True);
      WriteRegString('DTD Path', Settings.RootFolder + 'DTD\');
      WriteRegBool('Graduated Menus', True);
      WriteRegString('Help Path', Settings.RootFolder + 'Help\Rec20HLP.chm');
      WriteRegString('Local Images File Path',
                     Settings.RootFolder + 'User Files\User Dictionary Images');
      WriteRegBool('Plain Background', True);
      WriteRegBool('Tool Tips', True);
      WriteRegString('Spatial Ref System', Settings.SpatialRefSystem);
      WriteRegString('Cut Off Date', '1980');
      WriteRegString('Polygon Filter Path',
                     Settings.RootFolder + 'User Files\Polygon Filter\');
      WriteRegString('Recording Card Path',
                     Settings.RootFolder + 'User Files\Recording Cards\');
      WriteRegString('Report Path',
                     Settings.RootFolder + 'User Files\Reports\');
      WriteRegString('Report Template Path',
                     Settings.RootFolder + 'User Files\Templates\');
      WriteRegString('Rucksack Path',
                     Settings.RootFolder + 'User Files\Rucksacks\');
      WriteRegString('Snapshot Path',
                     Settings.RootFolder + 'User Files\Snapshots\');
  
      // Backward compatibility, or addins requirements.
      WriteRegString('Database Password', '');
      lReg.CloseKey;
    end else
      raise EInstallError.Create(ResStr_RegistryError);
  
    // Local Machine settings
    lReg.RootKey := HKEY_LOCAL_MACHINE;
    lReg.Access  := KEY_ALL_ACCESS;
    if lReg.OpenKey(REG_KEY_R6, True) then begin
      WriteRegString('Addin Path', Settings.RootFolder + 'Addins\');
      WriteRegString('Local Data Path', Settings.InstallFolder);
      WriteRegBool('Standalone', False);
      WriteRegString('Database Name', DATABASE_NAME);
      WriteRegString('Server Name', Settings.ProperServerName);
      WriteRegBool('Trusted Security', Settings.TrustedLogin);
      lReg.CloseKey;
    end else
      raise EInstallError.Create(ResStr_RegistryError);
  
    // Uninstaller.
    RegisterUninstaller(Settings.RootFolder, Settings.InstallFolder);
  finally
    lReg.Free;
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
  lblFile.Visible        := False;
  pbFiles.Visible        := False;
  lblOverall.Visible     := False;
  pbOverall.Visible      := False;
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
    lblFile.Visible        := True;
    pbFiles.Visible        := True;
    pbFiles.Position       := 0;
    lblOverall.Visible     := False;
    pbOverall.Visible      := False;
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
    // Copy Base Map files and uninstaller.
    FInstallFiles.GetFilesIn(Settings.RootFolder + 'Base Maps', '\Base Maps');
    FInstallFiles.GetFilesIn(Settings.RootFolder + 'Workstation Setup',
                             '', STR_UNINSTALLER_EX);
    FInstallFiles.ZipFiles.Add(
        Settings.RootFolder + 'Workstation Setup\' + STR_BORLAND_PACKAGES + '=' +
        IncludeTrailingPathDelimiter(GetWinDir) + 'System32\');

    FInstallFiles.CopyFiles;
    RegisterRecorder(Settings.RootFolder);
    RenameFile(Settings.InstallFolder + STR_UNINSTALLER_EX,
               Settings.InstallFolder + STR_UNINSTALLER);
    Settings.AddFileName(Settings.InstallFolder + STR_UNINSTALLER);
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
      DoInstallFiles;
      DoCreateShortcuts(Settings.RootFolder, Settings.RootFolder);
      DoInstallAddins;
      Result := True;
    except
      on Exception do;
    end;
  finally
    DefaultCursor(lCursor);
  end;
end;  // TfraInstallation.Execute 

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
  if Settings.OSVersion = wvVista then begin
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



