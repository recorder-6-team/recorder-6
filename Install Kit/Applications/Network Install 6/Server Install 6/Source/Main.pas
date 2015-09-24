{===============================================================================
  Unit:        Main

  Defines:     TfrmMain

  Description:

  Model:       Server Install 6.mpb

  Created:     November 2004

  Last revision information:
    $Revision: 19 $
    $Date: 29/07/09 10:03 $
    $Author: Ericsalmon $

===============================================================================}

unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, SqlList, Settings, TextMessages, BasePage, HotLabel,
  VCLUnZip, Htmlview;

type
  TfrmMain = class(TForm)
    btnBack: TButton;
    btnCancel: TButton;
    btnNext: TButton;
    imgBanner: TImage;
    imgPage: TImage;
    lblBack: THotLabel;
    lblCopyData: THotLabel;
    lblExit: THotLabel;
    lblGettingStarted: THotLabel;
    lblInstallDatabase: THotLabel;
    lblInstallOptions: THotLabel;
    lblInstallRecorder: THotLabel;
    lblRemoveR2K: THotLabel;
    lblWelcome: TLabel;
    lblWelcomeInfo1: TLabel;
    lblWelcomeInfo2: TLabel;
    pnlImage: TPanel;
    pnlInstallExtra: TPanel;
    pnlIntroduction: TPanel;
    pnlPages: TPanel;
    pnlTop: TPanel;
    lblAttachDBInfo: TLabel;
    Label1: TLabel;
    lblBrowseDVD: THotLabel;
    lblInstallRecorderInfo: TLabel;
    hvInfo: THTMLViewer;
    procedure btnBackClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure btnNextClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure lblBackClick(Sender: TObject);
    procedure lblCopyDataClick(Sender: TObject);
    procedure lblExitClick(Sender: TObject);
    procedure lblGettingStartedClick(Sender: TObject);
    procedure lblInstallDatabaseClick(Sender: TObject);
    procedure lblInstallSQLExpressClick(Sender: TObject);
    procedure lblInstallOptionsClick(Sender: TObject);
    procedure lblInstallRecorderClick(Sender: TObject);
    procedure lblRemoveR2KClick(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure lblBrowseDVDClick(Sender: TObject);
    procedure hvInfoHotSpotClick(Sender: TObject; const SRC: String;
      var Handled: Boolean);
  private
    FCurrentPage: TBasePage;
    FSettings: TSettings;
    FLastServerName: string;
    FLastTrustedLogin: boolean;
    FLastPassword: string;
    FLastUsername:string;
    procedure CancelInstall;
    procedure ChangePage(const APageClass: TBasePageClass);
    procedure CheckRecorder6Installed;
    procedure FreePageAndSettings;
    procedure HandleException(Sender: TObject; E: Exception);
    procedure PageContentChanged(Sender: TObject);
    procedure PageForceNextPage(Sender: TObject; APageClass: TBasePageClass);
    procedure ShowIntroduction;
    procedure ShowPages;
    procedure SynchronizeNavigationButtons;
    procedure RecallDbSettings;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  frmMain: TfrmMain;

//==============================================================================
implementation

{$R *.dfm}

uses
  SiteInfoPage, InstallFolderPage, InstallationPage, EasyShell,
  MigrateWelcomePage, RemoveWelcomePage, APIUtils, GeneralFunctions, Registry,
  SetupConstants, Functions, MigrationSettings, RemovalSettings;

{-==============================================================================
    TfrmMain
===============================================================================}
{-------------------------------------------------------------------------------
}
constructor TfrmMain.Create(AOwner: TComponent);
begin
  inherited;
  // Global exception handler
  Application.OnException := HandleException;
  pnlIntroduction.Visible := True;
  pnlIntroduction.BringToFront;
  imgPage.Visible := False;
  CheckRecorder6Installed;
  hvInfo.LoadFromString(ResStr_SqlServerInstall);
end;  // TfrmMain.Create

{-------------------------------------------------------------------------------
}
destructor TfrmMain.Destroy;
begin
  // Seems to be XPMenu having a fit every now and then. But as we're leaving, ignore it.
  try
    FreePageAndSettings;
    inherited;
  except
    on Exception do;
  end;
end;  // TfrmMain.Destroy

{-------------------------------------------------------------------------------
}
procedure TfrmMain.btnBackClick(Sender: TObject);
begin
  // Back button enabled and not previous page if first page in route.
  // So go back to main menu
  if FCurrentPage.Previous = nil then
    ShowIntroduction
  else
    ChangePage(FCurrentPage.Previous);
end;  // TfrmMain.btnBackClick 

{-------------------------------------------------------------------------------
}
procedure TfrmMain.btnCancelClick(Sender: TObject);
begin
  if pnlInstallExtra.Visible then
    lblBackClick(Sender)
  else
    Close;
end;  // TfrmMain.btnCancelClick

{-------------------------------------------------------------------------------
}
procedure TfrmMain.btnNextClick(Sender: TObject);
begin
  CheckRecorder6Installed;
  if FCurrentPage.IsFinal then begin
    FSettings.Complete := True;
  
    case FSettings.Mode of
      moInstallR6:
        begin
          FSettings.SaveLogToDisk;
          FSettings.SaveToIniFile;
          ShowIntroduction;
        end;

      moInstallDatabase, moUninstallR2K2, moMigrate:
        begin
          if FSettings.Mode = moInstallDatabase then
            FSettings.SaveSiteID;
          ShowIntroduction;
        end;
    end;
  end else begin
    FCurrentPage.ValidateContent;
    ChangePage(FCurrentPage.Next);
  end;
end;  // TfrmMain.btnNextClick 

{-------------------------------------------------------------------------------
}
procedure TfrmMain.CancelInstall;
begin
  btnCancel.Enabled := False;   // Cancel once only!
  Application.ProcessMessages;
  FSettings.Complete  := False;
  FSettings.Cancelled := True;
  // Tell current page to stop what it's doing.
  FCurrentPage.Cancel;
  Application.ProcessMessages;

  if (FSettings.FilesInstalled.Count > 0) or (FSettings.FoldersCreated.Count > 0) then begin
    // Install stopped in its tracks. Save log to Temp dir and run Uninstaller
    FSettings.InstallFolder := GetWindowsTempDir;
    FSettings.SaveLogToDisk;
    WinExec32(Format('"%sSystem\%s" ' + PARAM_SKIP + ' "%s"',
                     [FSettings.RootFolder, STR_UNINSTALLER, FSettings.InstallFolder]),
              FSettings.InstallFolder, SW_SHOWNORMAL);
  end;
  // Pretend all completed, so the app shuts down.
  FreePageAndSettings;
  Close;
end;  // TfrmMain.CancelInstall

{-------------------------------------------------------------------------------
}
procedure TfrmMain.ChangePage(const APageClass: TBasePageClass);
begin
  if Assigned(APageClass) then begin
    if Assigned(FCurrentPage) then
      FreeAndNil(FCurrentPage);

    DefaultCursor(crDefault);

    FCurrentPage := APageClass.Create(Self, FSettings);
    with FCurrentPage do begin
      Parent           := pnlPages;
      Align            := alClient;
      OnChangedContent := PageContentChanged;
      OnForceNextPage  := PageForceNextPage;
      TabOrder         := 0;
      LoadJPEGFromRes(ResourceImage, imgPage.Picture);
    end;
    // btnCancel is last "focusable" control on main form before moving to controls on frame.
    // So ask for the next after it. And it should be one of the frame's own.
    SelectNext(btnCancel, True, True);
    SynchronizeNavigationButtons;

    Refresh;
    if FCurrentPage.Execute then ChangePage(FCurrentPage.Next);
  end;
end;  // TfrmMain.ChangePage

{-------------------------------------------------------------------------------
}
procedure TfrmMain.CheckRecorder6Installed;
var
  lHasLocalDB: Boolean;
begin
  // Need a TSettings object to get the  local instances, then get rid of it.
  with TSettings.Create(itServer) do
    try
      lHasLocalDB := LocalInstanceNames.Count > 0;
    finally
      Free;
    end;

  with TRegistry.Create do
    try
      RootKey := HKEY_LOCAL_MACHINE;
      lblCopyData.Enabled  := OpenKeyReadOnly(REG_KEY_R6_SERVER) or lHasLocalDB;
      lblRemoveR2K.Enabled := lblCopyData.Enabled;
    finally
      Free;
    end;
end;  // TfrmMain.CheckRecorder6Installed 

{-------------------------------------------------------------------------------
}
procedure TfrmMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
var
  lConfirmMsg: String;
begin
  if not Assigned(FSettings) then
    CanClose := True
  else
  if pnlIntroduction.Visible or (FSettings.Mode = moUninstallR2K2) or
     FSettings.Complete or not FCurrentPage.ConfirmCancel then
  begin
    // If cancell on last page of R6 install, save logs
    if FSettings.Mode = moInstallR6 then begin
      FSettings.SaveLogToDisk;
      FSettings.SaveToIniFile;
    end;
    CancelInstall;
    CanClose := True;
  end else
  if not FSettings.Complete then
  begin
    CanClose := False;
    if FSettings.Mode in [moInstallR6, moInstallDatabase] then
      lConfirmMsg := ResStr_CloseQueryInstall
    else
    if FSettings.Mode = moMigrate then
      lConfirmMsg := ResStr_CloseQueryMigrate;
    if DefMessageDlg(lConfirmMsg, mtWarning, [mbYes, mbNo], mbNo, 0) = mrYes then
    begin
      CancelInstall;
      CanClose := True;
    end;
  end;
end;  // TfrmMain.FormCloseQuery

{-------------------------------------------------------------------------------
}
procedure TfrmMain.FormKeyPress(Sender: TObject; var Key: Char);
begin
  case Key of
    '1': if pnlInstallExtra.Visible then
           lblInstallSQLExpressClick(Sender)
         else
           lblInstallOptionsClick(Sender);
    '2': if pnlInstallExtra.Visible then
           lblInstallDatabaseClick(Sender)
         else
         if lblCopyData.Enabled then
           lblCopyDataClick(Sender);
    '3': if pnlInstallExtra.Visible then
           lblInstallRecorderClick(Sender)
         else
         if lblRemoveR2K.Enabled then
           lblRemoveR2KClick(Sender);
    '4': lblGettingStartedClick(Sender);
    '5': lblBrowseDVDClick(Sender);
    '6': lblExitClick(Sender);
  end;
end;

{-------------------------------------------------------------------------------
}
procedure TfrmMain.FreePageAndSettings;
begin
  FreeAndNil(FCurrentPage);
  // Although we clean up the settings, we persist the db login info to save the user
  // some effort if they go straight to the next step of install
  if assigned(FSettings) then begin
    FLastServerName := FSettings.ServerName;
    FLastTrustedLogin := FSettings.TrustedLogin;
    FLastPassword := FSettings.Password;
    FLastUsername := FSettings.Username;
  end;
  FreeAndNil(FSettings);
end;  // TfrmMain.FreePageAndSettings

{-------------------------------------------------------------------------------
}
procedure TfrmMain.HandleException(Sender: TObject; E: Exception);
var
  lIOResult: Integer;
begin
  // Ignore if on final page.
  if FCurrentPage.IsFinal then Exit;
  
  if E is EInOutError then begin
    lIOResult := IOResult;
    if lIOResult = ERROR_DISK_FULL then
      MessageDlg(SysErrorMessage(ERROR_DISK_FULL) + #13#10#13#10 + ResStr_DiskFull, mtError, [mbOk], 0)
    else
      MessageDlg(SysErrorMessage(lIOResult), mtError, [mbOk], 0);
    CancelInstall;
  end else
  if not (E is EAccessViolation) then begin
    MessageDlg(E.Message, mtError, [mbOk], 0);
    FSettings.Complete := True;
    Close;
  end;
end;  // TfrmMain.HandleException 

{-------------------------------------------------------------------------------
}
procedure TfrmMain.lblBackClick(Sender: TObject);
begin
  pnlInstallExtra.Visible := False;
end;  // TfrmMain.lblBackClick 

{-------------------------------------------------------------------------------
}
procedure TfrmMain.lblBrowseDVDClick(Sender: TObject);
begin
  WinExec32(
      'Explorer.exe "' + ExtractFilePath(Application.ExeName) + '"',
      ExtractFilePath(Application.ExeName),
      SW_SHOWNORMAL);
end;  // TfrmMain.lblBrowseDVDClick

{-------------------------------------------------------------------------------
}
procedure TfrmMain.lblCopyDataClick(Sender: TObject);
var
  lCursor: TCursor;
begin
  lCursor := HourglassCursor;
  try
    // Now it is relevant to create the Settings object.
    FSettings      := TMigrationSettings.Create(itServer);
    FSettings.Mode := moMigrate;
    FSettings.ResourcesLib := LoadLibrary('SetupRes.dll');
    // Load migration-specific values.
    TMigrationSettings(FSettings).LoadMigrationSettings;
    // Hide main menu panel AFTER the Settings object is there, as some stuff take a
    // little while to load.
    ShowPages;
    ChangePage(TfraMigrateWelcome);
  finally
    DefaultCursor(lCursor);
  end;
end;  // TfrmMain.lblCopyDataClick

{-------------------------------------------------------------------------------
}
procedure TfrmMain.lblExitClick(Sender: TObject);
begin
  Close;
end;  // TfrmMain.lblExitClick 

{-------------------------------------------------------------------------------
}
procedure TfrmMain.lblGettingStartedClick(Sender: TObject);
begin
  // For Network Setup, the Getting Started guide is in the Install Files folder.
  ShellFile(ExtractFilePath(Application.ExeName) + 'Install Files\' + STR_GETTING_STARTED);
end;  // TfrmMain.lblGettingStartedClick 

{-------------------------------------------------------------------------------
}
procedure TfrmMain.lblInstallDatabaseClick(Sender: TObject);
var
  lDrive: String;
  lCursor: TCursor;
begin
  lDrive := IncludeTrailingPathDelimiter(ExtractFileDrive(Application.ExeName));
  if not (GetDriveType(PChar(lDrive)) in [DRIVE_REMOVABLE, DRIVE_FIXED, DRIVE_CDROM]) then
  begin
    MessageDlg(ResStr_DatabaseInstallMustRunLocal, mtInformation, [mbOk], 0);
    Exit;
  end;

  lCursor := HourglassCursor;
  try
    // Now it is relevant to create the Settings object.
    FSettings      := TSettings.Create(itServer);
    RecallDbsettings;
    FSettings.Mode := moInstallDatabase;
    FSettings.ResourcesLib := LoadLibrary('SetupRes.dll');
    // Hide main menu panel AFTER the Settings object is there, as some stuff take a
    // little while to load.
    ShowPages;
    ChangePage(TfraSiteInfo);
  finally
    DefaultCursor(lCursor);
  end;
end;  // TfrmMain.lblInstallDatabaseClick

{-------------------------------------------------------------------------------
}
procedure TfrmMain.lblInstallSQLExpressClick(Sender: TObject);
var
  lDrive: String;
  lCursor: TCursor;
  policy: String;
begin
  // Need .Net Framework 2.0 installed in order to install SQLExpress
  policy := '';
  with TRegistry.Create do
    try
      RootKey := HKEY_LOCAL_MACHINE;
      if OpenKeyReadOnly(STR_DOTNET_REGISTRY_PATH) then
        policy := ReadString(STR_DOTNET_REGISTRY_FIELD);
    finally
      Free;
    end;

  if policy = '' then
    MessageDlg(ResStr_DotNet2NotFound, mtWarning, [mbOk], 0)
  else begin
    lDrive := IncludeTrailingPathDelimiter(ExtractFileDrive(Application.ExeName));
    if not (GetDriveType(PChar(lDrive)) in [DRIVE_REMOVABLE, DRIVE_FIXED, DRIVE_CDROM]) then
    begin
      MessageDlg(ResStr_SQLExpressInstallMustRunLocal, mtInformation, [mbOk], 0);
      Exit;
    end;
  
    lCursor := HourglassCursor;
    try
      // Now it is relevant to create the Settings object.
      FSettings      := TSettings.Create(itServer);
      FSettings.Mode := moInstallDatabase;
      FSettings.ResourcesLib := LoadLibrary('SetupRes.dll');
      // Hide main menu panel AFTER the Settings object is there, as some stuff take a
      // little while to load.
      ShowPages;
      ChangePage(TfraInstallation);
    finally
      DefaultCursor(lCursor);
    end;
  end;
end;  // TfrmMain.lblInstallSQLExpressClick 

{-------------------------------------------------------------------------------
}
procedure TfrmMain.lblInstallOptionsClick(Sender: TObject);
begin
  pnlInstallExtra.Visible := True;
end;  // TfrmMain.lblInstallOptionsClick 

{-------------------------------------------------------------------------------
}
procedure TfrmMain.lblInstallRecorderClick(Sender: TObject);
var
  lCursor: TCursor;
begin
  lCursor := HourglassCursor;
  try
    // Now it is relevant to create the Settings object.
    FSettings      := TSettings.Create(itServer);
    RecallDbsettings;
    FSettings.Mode := moInstallR6;
    FSettings.ResourcesLib := LoadLibrary('SetupRes.dll');
    // Hide main menu panel AFTER the Settings object is there, as some stuff take a
    // little while to load.
    ShowPages;
    ChangePage(TfraInstallFolder);
  finally
    DefaultCursor(lCursor);
  end;
end;  // TfrmMain.lblInstallRecorderClick 

{-------------------------------------------------------------------------------
}
procedure TfrmMain.lblRemoveR2KClick(Sender: TObject);
var
  lCursor: TCursor;
begin
  lCursor := HourglassCursor;
  try
    // Now it is relevant to create the Settings object.
    FSettings := TRemovalSettings.Create(itServer);
    FSettings.Mode := moUninstallR2K2;
    FSettings.ResourcesLib := LoadLibrary('SetupRes.dll');
    TRemovalSettings(FSettings).LoadRemovalSettings;
    // Hide main menu panel AFTER the Settings object is there, as some stuff take a
    // little while to load.
    ShowPages;
    ChangePage(TfraRemoveWelcome);
  finally
    DefaultCursor(lCursor);
  end;
end;  // TfrmMain.lblRemoveR2KClick 

{-------------------------------------------------------------------------------
}
procedure TfrmMain.PageContentChanged(Sender: TObject);
begin
  SynchronizeNavigationButtons;
end;  // TfrmMain.PageContentChanged 

{-------------------------------------------------------------------------------
}
procedure TfrmMain.PageForceNextPage(Sender: TObject; APageClass: TBasePageClass);
begin
  ChangePage(APageClass);
end;  // TfrmMain.PageForceNextPage 

{-------------------------------------------------------------------------------
}
procedure TfrmMain.ShowIntroduction;
begin
  FreePageAndSettings;
  pnlIntroduction.Visible := True;
  KeyPreview              := True;
  pnlInstallExtra.Visible := False;
  imgPage.Visible         := False;
end;  // TfrmMain.ShowIntroduction

{-------------------------------------------------------------------------------
}
procedure TfrmMain.ShowPages;
begin
  pnlIntroduction.Visible := False;
  KeyPreview              := False;
  imgPage.Visible         := True;
end;  // TfrmMain.ShowPages

{-------------------------------------------------------------------------------
}
procedure TfrmMain.SynchronizeNavigationButtons;
begin
  btnNext.Enabled := FCurrentPage.HasNext;
  btnBack.Enabled := FCurrentPage.HasPrevious;
  btnNext.Caption := FCurrentPage.NextCaption;  // Can be anything, "Next", "Install"...
  btnCancel.Visible := not FCurrentPage.IsFinal;
end;  // TfrmMain.SynchronizeNavigationButtons

procedure TfrmMain.hvInfoHotSpotClick(Sender: TObject; const SRC: String;
  var Handled: Boolean);
begin
  ShellFile(src);
end;

(**
 * If the user does several steps of install without restarting the app, remember the
 * database connection settings.
 *)
procedure TfrmMain.RecallDbSettings;
begin
  FSettings.ServerName := FLastServerName;
  FSettings.TrustedLogin := FLastTrustedLogin;
  FSettings.Password := FLastPassword;
  FSettings.Username := FLastUsername;
end;

end.
