{===============================================================================
  Unit:        Main

  Defines:     TfrmMain

  Description: Recorder Standalone Setup main form.

  Created:     March 2004

  Last revision information:
    $Revision: 17 $
    $Date: 29/07/09 10:03 $
    $Author: Ericsalmon $

===============================================================================}

unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, SqlList, Settings, TextMessages, BasePage, HotLabel,
  XPMenu, VCLUnZip, Htmlview, EasyShell;

resourcestring
  ResStr_StandaloneInstallFromLocalDriveOnly =
      'The Recorder 6 install program must be run from a local drive.'#13#13
      + 'Please see the help file for more information.';

  ResStr_DiskFull =
      'The application cannot proceed until there is enough disk space available.';

type
  TfrmMain = class (TForm)
    btnBack: TButton;
    btnCancel: TButton;
    btnNext: TButton;
    imgBanner: TImage;
    lblWelcome: TLabel;
    lblWelcomeInfo: TLabel;
    lblCopyData: THotLabel;
    lblBrowseDVD: THotLabel;
    lblGettingStarted: THotLabel;
    lblInstall: THotLabel;
    lblRemoveR2K: THotLabel;
    pnlIntroduction: TPanel;
    pnlPages: TPanel;
    pnlImage: TPanel;
    imgPage: TImage;
    pnlButtons: TPanel;
    lblChooseOptions: TLabel;
    lblExit: THotLabel;
    pnlSetup: TPanel;
    hvInfo: THTMLViewer;
    procedure btnBackClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure btnNextClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure lblCopyDataClick(Sender: TObject);
    procedure lblExitClick(Sender: TObject);
    procedure lblGettingStartedClick(Sender: TObject);
    procedure lblInstallClick(Sender: TObject);
    procedure lblRemoveR2KClick(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure lblBrowseDVDClick(Sender: TObject);
    procedure hvInfoHotSpotClick(Sender: TObject; const SRC: String;
      var Handled: Boolean);
  private
    FCurrentPage: TBasePage;
    FSettings: TSettings;
    FXPMenu: TXPMenu;
    procedure CancelInstall;
    procedure ChangePage(const APageClass: TBasePageClass);
    procedure CheckRecorder6Installed;
    procedure FreePageAndSettings;
    procedure PageContentChanged(Sender: TObject);
    procedure PageForceNextPage(Sender: TObject; APageClass: TBasePageClass);
    procedure ShowIntroduction;
    procedure SynchronizeNavigationButtons;
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
  WelcomePage, MigrateWelcomePage, RemoveWelcomePage, APIUtils, GeneralFunctions,
  Registry, SetupConstants, Functions, MigrationSettings, RemovalSettings;

{-==============================================================================
    TfrmMain
===============================================================================}
{-------------------------------------------------------------------------------
}
constructor TfrmMain.Create(AOwner: TComponent);
begin
  inherited;
  ShowIntroduction;
  CheckRecorder6Installed;
  FXPMenu := TXPMenu.Create(Self);
  FXPMenu.XPControls := FXPMenu.XPControls - [xcCombo, xcGroupBox];
  FXPMenu.Active := True;
end;  // TfrmMain.Create

{-------------------------------------------------------------------------------
}
destructor TfrmMain.Destroy;
begin
  try
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
  Close;
end;  // TfrmMain.btnCancelClick

{-------------------------------------------------------------------------------
}
procedure TfrmMain.btnNextClick(Sender: TObject);
begin
  if FCurrentPage.IsFinal then
  begin
    FSettings.Complete := True;

    if FSettings.Mode = moInstallR6 then
    begin
      FSettings.SaveLogToDisk;
      FSettings.SaveToIniFile;
      WinExec32(FSettings.DatabaseSetupCommandWithAuth, FSettings.RootFolder + 'System', SW_SHOWNORMAL);
      Close;
    end else
      ShowIntroduction;
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

  if FSettings.Mode = moInstallR6 then begin
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
  if Assigned(APageClass) then
  begin
    if Assigned(FCurrentPage) then
      FreeAndNil(FCurrentPage);

    DefaultCursor(crDefault);

    FCurrentPage := APageClass.Create(Self, FSettings);
    with FCurrentPage do begin
      Parent := pnlPages;
      Align  := alClient;
      OnChangedContent := PageContentChanged;
      OnForceNextPage  := PageForceNextPage;
      TabOrder := 0;
      LoadJPEGFromRes(ResourceImage, imgPage.Picture);
    end;
    // btnCancel is last "focusable" control on main form before moving to controls on frame.
    // So ask for the next after it. And it should be one of the frame's own.
    SelectNext(btnCancel, True, True);
    SynchronizeNavigationButtons;
    FXPMenu.InitComponent(Self);

    Refresh;
    if FCurrentPage.Execute then ChangePage(FCurrentPage.Next);
  end;
end;  // TfrmMain.ChangePage

{-------------------------------------------------------------------------------
}
procedure TfrmMain.CheckRecorder6Installed;
begin
  with TRegistry.Create do
    try
      if not OpenKeyReadOnly(REG_KEY_R6) then begin
        lblCopyData.Enabled  := False;
        lblRemoveR2K.Enabled := False;
      end;
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
    Canclose := True
  else
  if pnlIntroduction.Visible or (FSettings.Mode = moUninstallR2K2) or
     FSettings.Complete or not FCurrentPage.ConfirmCancel then
    CanClose := True
  else
  if not FSettings.Complete then begin
    CanClose := False;
    if FSettings.Mode = moInstallR6 then lConfirmMsg := ResStr_CloseQueryInstall else
    if FSettings.Mode = moMigrate   then lConfirmMsg := ResStr_CloseQueryMigrate;
    if DefMessageDlg(lConfirmMsg, mtWarning, [mbYes, mbNo], mbNo, 0) = mrYes then begin
      CancelInstall;
      CanClose := True;
    end;
  end;
end;  // TfrmMain.FormCloseQuery 

{-------------------------------------------------------------------------------
  React to keyboard for menu.
}
procedure TfrmMain.FormKeyPress(Sender: TObject; var Key: Char);
begin
  case Key of
    '1': if lblInstall.Enabled then lblInstallClick(Sender);
    '2': if lblCopyData.Enabled then lblCopyDataClick(Sender);
    '3': if lblRemoveR2K.Enabled then lblRemoveR2KClick(Sender);
    '4': lblGettingStartedClick(Sender);
    '5': lblBrowseDVDClick(Sender);
    '6': lblExitClick(Sender);
  end;
end;  // TfrmMain.FormKeyPress

{-------------------------------------------------------------------------------
}
procedure TfrmMain.FreePageAndSettings;
begin
  FreeAndNil(FCurrentPage);
  FreeAndNil(FSettings);
end;  // TfrmMain.FreePageAndSettings

{-------------------------------------------------------------------------------
}
procedure TfrmMain.lblCopyDataClick(Sender: TObject);
var
  lCursor: TCursor;
begin
  lCursor := HourglassCursor;
  try
    // Now it is relevant to create the Settings object.
    FSettings := TMigrationSettings.Create(itStandalone);
    FSettings.Mode := moMigrate;
    FSettings.ResourcesLib := LoadLibrary('SetupRes.dll');
    // Load migration-specific values.
    TMigrationSettings(FSettings).LoadMigrationSettings;
    // Hide main menu panel AFTER the Settings object is there, as some stuff take a
    // little while to load.
    pnlIntroduction.Visible := False;
    KeyPreview              := False;
    btnCancel.Enabled       := True;
    ChangePage(TfraMigrateWelcome);
  finally
    DefaultCursor(lCursor);
  end;
end;  // TfrmMain.lblCopyDataClick

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
procedure TfrmMain.lblExitClick(Sender: TObject);
begin
  Close;
end;  // TfrmMain.lblExitClick

{-------------------------------------------------------------------------------
}
procedure TfrmMain.lblGettingStartedClick(Sender: TObject);
begin
  ShellFile(ExtractFilePath(Application.ExeName) + STR_GETTING_STARTED);
end;  // TfrmMain.lblGettingStartedClick

{-------------------------------------------------------------------------------
}
procedure TfrmMain.lblInstallClick(Sender: TObject);
var
  drive: String;
  cursor: TCursor;
begin
  drive := IncludeTrailingPathDelimiter(ExtractFileDrive(Application.ExeName));
  if not (GetDriveType(PChar(drive)) in [DRIVE_REMOVABLE, DRIVE_FIXED, DRIVE_CDROM]) then
  begin
    MessageDlg(ResStr_StandaloneInstallFromLocalDriveOnly, mtInformation, [mbOk], 0);
    Exit;
  end;

  cursor := HourglassCursor;
  try
    // Now it is relevant to create the Settings object.
    FSettings := TSettings.Create(itStandalone);
    FSettings.ResourcesLib := LoadLibrary('SetupRes.dll');
    FSettings.Mode := moInstallR6;
    // Hide main menu panel AFTER the Settings object is there, as some stuff take a
    // little while to load.
    pnlIntroduction.Visible := False;
    KeyPreview              := False;
    btnCancel.Enabled       := True;
    ChangePage(TfraWelcome);
  finally
    DefaultCursor(cursor);
  end;
end;  // TfrmMain.lblInstallClick

{-------------------------------------------------------------------------------
}
procedure TfrmMain.lblRemoveR2KClick(Sender: TObject);
var
  lCursor: TCursor;
begin
  lCursor := HourglassCursor;
  try
    // Now it is relevant to create the Settings object.
    FSettings := TRemovalSettings.Create(itStandalone);
    FSettings.ResourcesLib := LoadLibrary('SetupRes.dll');
    FSettings.Mode := moUninstallR2K2;
    TRemovalSettings(FSettings).LoadRemovalSettings;
    // Hide main menu panel AFTER the Settings object is there, as some stuff take a
    // little while to load.
    pnlIntroduction.Visible := False;
    KeyPreview              := False;
    btnCancel.Enabled       := True;
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
  btnBack.Enabled         := False;
  btnNext.Enabled         := False;
  btnCancel.Enabled       := False;
  hvInfo.LoadFromFile('installSqlServer.html');
  pnlIntroduction.BringToFront;
end;  // TfrmMain.ShowIntroduction

{-------------------------------------------------------------------------------
}
procedure TfrmMain.SynchronizeNavigationButtons;
begin
  btnNext.Enabled := FCurrentPage.HasNext;
  btnBack.Enabled := FCurrentPage.HasPrevious;
  btnNext.Caption := FCurrentPage.NextCaption;  // Can be anything, "Next", "Install"...
  if FCurrentPage.IsFinal then begin
    btnNext.Enabled := True;
    btnNext.SetFocus;
  end;
end;  // TfrmMain.SynchronizeNavigationButtons

procedure TfrmMain.hvInfoHotSpotClick(Sender: TObject; const SRC: String;
  var Handled: Boolean);
begin
  ShellFile(src);
end;

end.
