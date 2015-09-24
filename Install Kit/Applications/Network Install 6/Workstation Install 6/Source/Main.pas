{===============================================================================
  Unit:        Main

  Defines:     TfrmMain

  Description:

  Model:       Workstation Install 6.mpb

  Created:     November 2004

  Last revision information:
    $Revision: 8 $
    $Date: 14/07/09 15:43 $
    $Author: Ericsalmon $

===============================================================================}

unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, SqlList, Settings, TextMessages, BasePage, HotLabel,
  XPMenu, VCLUnZip;

resourcestring
  ResStr_DiskSpaceNeeded =
      #13#13'The application cannot proceed until there is enough disk space available.';

type
  TfrmMain = class(TForm)
    btnBack: TButton;
    btnCancel: TButton;
    btnNext: TButton;
    imgBanner: TImage;
    imgPage: TImage;
    lblSetupInfo: TLabel;
    lblCopyData: THotLabel;
    lblExit: THotLabel;
    lblGettingStarted: THotLabel;
    lblInstallRecorder: THotLabel;
    lblRemoveR2K: THotLabel;
    lblWelcome: TLabel;
    pnlImage: TPanel;
    pnlIntroduction: TPanel;
    pnlPages: TPanel;
    pnlTop: TPanel;
    procedure btnBackClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure btnNextClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure lblCopyDataClick(Sender: TObject);
    procedure lblExitClick(Sender: TObject);
    procedure lblGettingStartedClick(Sender: TObject);
    procedure lblInstallRecorderClick(Sender: TObject);
    procedure lblRemoveR2KClick(Sender: TObject);
  private
    FCurrentPage: TBasePage;
    FSettings: TSettings;
    FXPMenu: TXPMenu;
    procedure CancelInstall;
    procedure ChangePage(const APageClass: TBasePageClass);
    procedure CheckRecorder6Installed;
    procedure FreePageAndSettings;
    procedure HandleException(Sender: TObject; E: Exception);
    procedure PageContentChanged(Sender: TObject);
    procedure PageForceNextPage(Sender: TObject; APageClass: TBasePageClass);
    procedure SynchronizeNavigationButtons;
  public
    constructor Create(AOwner: TComponent); override;
  end;
  
var
  frmMain: TfrmMain;

//==============================================================================
implementation

{$R *.dfm}

uses
  InstallFolderPage, MigrateWelcomePage, RemoveWelcomePage, APIUtils, GeneralFunctions,
  Registry, SetupConstants, Functions, MigrationSettings, RemovalSettings;

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
  FXPMenu := TXPMenu.Create(Self);
  FXPMenu.XPControls := FXPMenu.XPControls - [xcCombo];
  FXPMenu.Active := True;
end;  // TfrmMain.Create 

{-------------------------------------------------------------------------------
}
procedure TfrmMain.btnBackClick(Sender: TObject);
begin
  // Back button enabled and not previous page if first page in route.
  // So go back to main menu
  if FCurrentPage.Previous = nil then begin
    FreePageAndSettings;
    pnlIntroduction.Visible := True;
    imgPage.Visible         := False;
  end else
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
  CheckRecorder6Installed;
  if FCurrentPage.IsFinal then
  begin
    FSettings.Complete := True;

    case FSettings.Mode of
      moInstallR6:
        begin
          FSettings.SaveLogToDisk;
          FSettings.SaveToIniFile;
          FreePageAndSettings;
          pnlIntroduction.Visible := True;
          imgPage.Visible         := False;
        end;

      moUninstallR2K2, moMigrate:
        begin
          FreePageAndSettings;
          pnlIntroduction.Visible := True;
          imgPage.Visible         := False;
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
  FSettings.Complete := False;
  FSettings.Cancelled := True;
  // Tell current page to stop what it's doing.
  FCurrentPage.Cancel;
  Application.ProcessMessages;
  
  if FSettings.Mode = moInstallR6 then begin
    // Install stopped in its tracks. Save log to Temp dir and run Uninstaller
    FSettings.InstallFolder := GetWindowsTempDir;
    FSettings.SaveLogToDisk;
    WinExec32(Format('"%sWorkstation Setup\%s" ' + PARAM_SKIP + ' "%s"',
                     [FSettings.RootFolder, STR_UNINSTALLER_EX, FSettings.InstallFolder]),
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
  if APageClass = nil then
    raise Exception.Create('Next page class not specified.');
  
  if Assigned(FCurrentPage) then
    FreeAndNil(FCurrentPage);
  
  DefaultCursor(crDefault);
  
  FCurrentPage := APageClass.Create(Self, FSettings);
  with FCurrentPage do begin
    Parent := pnlPages;
    Align := alClient;
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
end;  // TfrmMain.ChangePage 

{-------------------------------------------------------------------------------
}
procedure TfrmMain.CheckRecorder6Installed;
begin
  with TRegistry.Create do
    try
      lblCopyData.Enabled  := OpenKeyReadOnly(REG_KEY_R6);
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
    CanClose := True;
    // If cancell on last page of R6 install, save logs
    if FSettings.Mode = moInstallR6 then begin
      FSettings.SaveLogToDisk;
      FSettings.SaveToIniFile;
    end;
  end else
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
}
procedure TfrmMain.FreePageAndSettings;
begin
  FreeAndNil(FCurrentPage);
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
      MessageDlg(SysErrorMessage(ERROR_DISK_FULL) + ResStr_DiskSpaceNeeded, mtError, [mbOk], 0)
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
procedure TfrmMain.lblCopyDataClick(Sender: TObject);
var
  lCursor: TCursor;
begin
  lCursor := HourglassCursor;
  try
    // Now it is relevant to create the Settings object.
    FSettings := TMigrationSettings.Create(itWorkstation);
    FSettings.Mode := moMigrate;
    // Load migration-specific values.
    TMigrationSettings(FSettings).LoadMigrationSettings;
    // Hide main menu panel AFTER the Settings object is there, as some stuff take a
    // little while to load.
    pnlIntroduction.Visible := False;
    imgPage.Visible := True;
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
  ShellFile(ExtractFilePath(Application.ExeName) + STR_GETTING_STARTED);
end;  // TfrmMain.lblGettingStartedClick 

{-------------------------------------------------------------------------------
}
procedure TfrmMain.lblInstallRecorderClick(Sender: TObject);
var
  lCursor: TCursor;
begin
  lCursor := HourglassCursor;
  try
    // Now it is relevant to create the Settings object.
    FSettings := TSettings.Create(itWorkstation);
    FSettings.Mode := moInstallR6;
  
    if FileExists(FSettings.RootFolder + STR_INSTALLSETTINGS) then
      with TStringList.Create do
        try
          LoadFromFile(FSettings.RootFolder + STR_INSTALLSETTINGS);
          FSettings.SiteID       := Values['SiteID'];
          FSettings.ServerName   := Values['Server Name'];
          FSettings.TrustedLogin := Values['Trusted Security'] = '1';
        finally
          Free;
        end;
  
    // Hide main menu panel AFTER the Settings object is there, as some stuff take a
    // little while to load.
    pnlIntroduction.Visible := False;
    imgPage.Visible := True;
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
    FSettings := TRemovalSettings.Create(itWorkstation);
    FSettings.Mode := moUninstallR2K2;
    TRemovalSettings(FSettings).LoadRemovalSettings;
    // Hide main menu panel AFTER the Settings object is there, as some stuff take a
    // little while to load.
    pnlIntroduction.Visible := False;
    imgPage.Visible := True;
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
procedure TfrmMain.SynchronizeNavigationButtons;
begin
  btnNext.Enabled   := FCurrentPage.HasNext;
  btnBack.Enabled   := FCurrentPage.HasPrevious;
  btnNext.Caption   := FCurrentPage.NextCaption;  // Can be anything, "Next", "Install"...
end;  // TfrmMain.SynchronizeNavigationButtons 

end.



