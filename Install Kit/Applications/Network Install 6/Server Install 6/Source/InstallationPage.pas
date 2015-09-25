{===============================================================================
  Unit:        InstallationPage

  Defines:     TfraInstallation

  Description:

  Model:       Server Install 6.mpb

  Created:     November 2004

  Last revision information:
    $Revision: 29 $
    $Date: 14/07/09 14:59 $
    $Author: Ericsalmon $

===============================================================================}

unit InstallationPage;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, BasePage, ExtCtrls, ComCtrls, StdCtrls, ExceptionForm, InstallFiles,
  AttachDB;

const
  ST_BATCHFILE_NAME = 'CreateAccessLinkedTables.bat';

resourcestring
  ResStr_EnsureCorrectInstance =
      'Please ensure that you have selected the correct MSDE/SQL Express/SQL Server instance.';
  ResStr_NBNDatabaseMissing =
      'The NBNData database could not be found on the server.  ' +
      'Please ensure that the Recorder 6 database has been created ' +
      'and that you have selected the correct MSDE/SQL Express/SQL Server instance.';
  ResStr_CheckNetworkAndInstance =
      'Please ensure that there are no problems with your network and/or the MSDE/SQL Express/SQL ' +
      'Server instance server you want to connect to before trying again.';

  ResStr_StartingSQLExpress  = 'Starting SQL Express...';
  ResStr_CreatingDatabase    = 'Creating database file...';
  ResStr_ConfiguringDatabase = 'Configuring database...';

  ResStr_MDBFailed =
      'Setup failed to create the linked Access database, due to the following error: '#13#10
      + '%s'#13#10#13#10
      + 'This is a minor issue and will not affect core Recorder functionality.'#13#10#13#10
      + 'You can retry creating the Access Database now or at a later date by '
      + 'running the'#13#10' ''' + ST_BATCHFILE_NAME + ''' file located in ''%s''.';

type
  EInstallError = class(TExceptionPath)
  end;

  TfraInstallation = class(TBasePage)
    Animation: TAnimate;
    lblInstallRecorder: TLabel;
    lblCancel: TLabel;
    lblFile: TLabel;
    lblInstallStep: TLabel;
    lblOverall: TLabel;
    pbFiles: TProgressBar;
    pbOverall: TProgressBar;
    lblInstallDatabase: TLabel;
  private
    FInstallFiles: TInstallFiles;
    procedure CleanDatabaseProgress(AValue: Integer);
    procedure DoInstallAccessMDB;
    procedure DoInstallDatabase;
    procedure DoInstallFiles;
    procedure DoRegistryEntries;
    procedure PrepareRerunProcess(const installDir, cmdLine: String);
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
  SetupConstants, TextMessages, Settings, PrepareMDB, ComObj, ServerSelectPage,
  DatabaseServerSelectPage, StrUtils, Functions;

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

end;  // TfraInstallation.Cancel 

{-------------------------------------------------------------------------------
}
procedure TfraInstallation.CleanDatabaseProgress(AValue: Integer);
begin
  pbFiles.Position := AValue;
  Application.ProcessMessages;
end;  // TfraInstallation.CleanDatabaseProgress 

{-------------------------------------------------------------------------------
}
procedure TfraInstallation.DoInstallAccessMDB;
var
  lAttributes: Word;
  lDestMDB: String;
begin
  // If MigrateDatabase was skipped, the panel is still hidden!!!
  lblInstallStep.Caption := ResStr_CreatingAccessMDB;
  lblFile.Visible        := True;
  lblFile.Caption        := ResStr_PleaseWait;
  pbFiles.Visible        := True;
  lblOverall.Visible     := False;
  pbOverall.Visible      := False;
  Refresh;
  Application.ProcessMessages;

  // Ensure folder exists before trying to copy file across.
  ForceDirectories(Settings.InstallFolder + 'Database\');

  // Filename of Access database file in [InstallFolder]
  lDestMDB := Settings.InstallFolder + 'Database\' + ACCESS_MAIN_DB;

  // Now copy new empty MDB
  CopyFile(PChar(Settings.RootFolder + STR_NBNDATA_MDB), PChar(lDestMDB), False);

  try
    // Remove readonly flag from copied file
    lAttributes := FileGetAttr(lDestMDB);
    lAttributes := lAttributes and not SysUtils.faReadOnly;
    FileSetAttr(lDestMDB, lAttributes);

    // Create the links
    with TPrepareMDB.Create(Settings.ServerName, lDestMDB, Settings.TrustedLogin,
                            Settings.Username, Settings.Password) do
      try
        Execute(pbFiles, nil);
      finally
        Free;
      end;
  except
    on E:EFailedConnect do begin
      MessageDlg(E.Message + #10#13#10#13 + ResStr_EnsureCorrectInstance, mtWarning, [mbOk], 0);
      ForceNextPage(TfraDatabaseServerSelect);
    end;
    on E:ENBNDataMissing do begin
      MessageDlg(ResStr_NBNDatabaseMissing, mtWarning, [mbOk], 0);
      ForceNextPage(TfraDatabaseServerSelect);
    end;
    on E:EOleException do begin
      PrepareRerunProcess(
          Settings.InstallFolder,
          Format('"%sSystem\DatabaseSetup.exe" /server "/installDir:%s"',
                 [Settings.RootFolder, Settings.InstallFolder]));
      if MessageDlg(
          Format(ResStr_MDBFailed, [E.Message, Settings.InstallFolder]),
          mtInformation, [mbRetry, mbIgnore], 0) = mrRetry then
        DoInstallAccessMDB;
    end;
    on E:Exception do begin
      MessageDlg(E.Message, mtWarning, [mbOk], 0);
      ForceNextPage(TfraDatabaseServerSelect);
    end;
  end;
end;  // TfraInstallation.DoInstallAccessMDB

{-------------------------------------------------------------------------------
}
procedure TfraInstallation.DoInstallDatabase;
var
  lAttachDB: TAttachDB;
  path: string;
begin
  if Settings.Cancelled then Exit;

  if Settings.SpecifyOwnDatabasePath then begin
    path := Settings.DatabasePath;
    if RightStr(path, 1) <> PathDelim then path := path + PathDelim;
  end else
    path := '';
  
  lblInstallStep.Caption := ResStr_AttachingDB;
  lblFile.Visible        := True;
  lblFile.Caption        := ResStr_PleaseWait;
  pbFiles.Visible        := True;
  lblOverall.Visible     := False;
  pbOverall.Visible      := False;
  Refresh;
  Application.ProcessMessages;
  
  lAttachDB := TAttachDB.Create(Settings.ServerName, Settings.RegistryInstanceName,
       CleanDatabaseProgress, Settings.UserName, Settings.Password, Settings.TrustedLogin);
  try
    try
      lblInstallStep.Caption := ResStr_StartingSQLExpress;
      if lAttachDB.ConnectAndCheckDb(true) then begin
        lblInstallStep.Caption := ResStr_CreatingDatabase;
        lAttachDB.CopyMDF(Settings.RootFolder + STR_NBNDATA_ZIP_STD, path);
        lblInstallStep.Caption := ResStr_ConfiguringDatabase;
        lAttachDB.InitDB(false, Settings.SiteID);
      end else begin
        MessageDlg(lAttachDB.ErrorMessage, mtWarning, [mbOk], 0);
        if Settings.Mode = moInstallDatabase then
          ForceNextPage(TfraServerSelect)
        else
        if Settings.Mode = moInstallR6 then
          ForceNextPage(TfraDatabaseServerSelect);
      end;
    finally
      lAttachDB.Free;
    end;
  except
    on EAttachAborted do begin
      Application.Terminate;
      Abort;
    end;
  end; // try
  
  lblFile.Caption  := '';
  pbFiles.Position := 0;
  Application.ProcessMessages;
end;  // TfraInstallation.DoInstallDatabase 

{-------------------------------------------------------------------------------
}
procedure TfraInstallation.DoInstallFiles;
begin
  if csDestroying in ComponentState then
    raise EAbort.Create('');
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
    // Copy main files, no dest folder, all go to InstallFolder
    FInstallFiles.GetFilesIn(Settings.RootFolder + 'Install Files', '');
    FInstallFiles.GetFilesIn(Settings.RootFolder + 'System', '', STR_UNINSTALLER);
    FInstallFiles.GetFilesIn(Settings.RootFolder + 'System', '', STR_WORKSTATION_SETUP);
    FInstallFiles.GetFilesIn(Settings.RootFolder + 'System', '', STR_ALLOWED_ERRORS);
    // Additional files required for workstation setup to be able to run all options.
    FInstallFiles.GetFilesIn(Settings.RootFolder + 'System',
                             '\Workstation Setup', STR_BORLAND_PACKAGES);
    FInstallFiles.GetFilesIn(Settings.RootFolder + 'System',
                             '\Workstation Setup', STR_SPATIAL_SYSTEMS);
    FInstallFiles.GetFilesIn(Settings.RootFolder + 'System',
                             '\Workstation Setup', STR_TABLE_MAPPINGS_INI);
    FInstallFiles.GetFilesIn(Settings.RootFolder + 'System',
                             '\Workstation Setup', STR_UNINSTALLER_EX);
    FInstallFiles.GetFilesIn(Settings.RootFolder + 'System\Database',
                             '\Workstation Setup\Database', STR_MAPUPGRADE_SQL);
    FInstallFiles.GetFilesIn(Settings.RootFolder + 'System\Database',
                             '\Workstation Setup\Database', STR_NBNDATA_DATA_ZIP);
    FInstallFiles.GetFilesIn(Settings.RootFolder + 'System\Database',
                             '\Workstation Setup\Database', STR_NBNDATA_MDB_FILE);
    FInstallFiles.GetFilesIn(Settings.RootFolder + 'System\System Components',
                             '\Workstation Setup\System Components');

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
procedure TfraInstallation.DoRegistryEntries;
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
  
begin
  if Settings.Cancelled then Exit;
  
  lblInstallStep.Caption := ResStr_CreateRegistry;
  lblFile.Visible    := True;
  pbFiles.Visible    := True;
  pbFiles.Position   := 0;
  lblOverall.Visible := False;
  pbOverall.Visible  := False;
  
  Settings.RegistryInstalled := True;
  lReg := TRegistry.Create;
  try
    pbFiles.Max := 7;
    // Local Machine settings
    lReg.RootKey := HKEY_LOCAL_MACHINE;
    lReg.Access  := KEY_ALL_ACCESS;
    if lReg.OpenKey(REG_KEY_R6_SERVER, True) then
    begin
      WriteRegString('Addin Path', Settings.InstallFolder + 'Addins\');
      WriteRegString('Installation Path', Settings.InstallFolder);
      WriteRegString('Local Data Path', Settings.InstallFolder);
      WriteRegBool('Standalone', False);
      WriteRegString('Database Name', DATABASE_NAME);
      WriteRegString('Server Name', Settings.ProperServerName);
      WriteRegBool('Trusted Security', Settings.TrustedLogin);
      lReg.CloseKey;
    end else
      raise EInstallError.Create(ResStr_RegistryError);
  finally
    lReg.Free;
  end;
  lblFile.Caption  := '';
  pbFiles.Max      := 100;
  pbFiles.Position := 0;
  
  Application.ProcessMessages;
end;  // TfraInstallation.DoRegistryEntries

{-------------------------------------------------------------------------------
}
function TfraInstallation.Execute: Boolean;
var
  lCursor: TCursor;
begin
  Animation.Active := True;
  Application.ProcessMessages;

  Result := False;
  lCursor := AppStartCursor;
  try
    try
      lblInstallRecorder.Visible   := Settings.Mode = moInstallR6;
      lblInstallDatabase.Visible   := Settings.Mode = moInstallDatabase;

      if Settings.Mode = moInstallDatabase then begin
        DoInstallDatabase;
        Result := True;
      end else begin
        // Proceed with installing the various bits
        DoInstallFiles;
        DoInstallAccessMDB;
        DoRegistryEntries;
        Result := True;
      end;
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
  Prepare batch file to rerun the Access db linking.
}
procedure TfraInstallation.PrepareRerunProcess(const installDir, cmdLine: String);
begin
  with TStringList.Create do
    try
      if Pos('/mdb', cmdLine) = 0 then
        Add(cmdLine + ' /mdb')
      else
        Add(cmdLine);
      SaveToFile(installDir + ST_BATCHFILE_NAME);
      Settings.AddFileName(installDir + ST_BATCHFILE_NAME);
    finally
      Free;
    end;
end;

{-------------------------------------------------------------------------------
  Sets the animation to the vista file copy animation if the user is using vista or higher.
}
procedure TfraInstallation.LoadContent;
begin
  if Settings.OSVersion >= wvVista then
    Settings.SetAnimationFromResource(Animation, ResAvi_VistaFileCopy);
end;  // TfraInstallation.LoadContent

end.
