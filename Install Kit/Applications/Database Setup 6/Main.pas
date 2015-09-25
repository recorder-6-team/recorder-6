{-------------------------------------------------------------------------------
  Unit:         Main.pas

  Defines:      TfrmMain

  Description:  Database Setup.  Completes the bit of the upgrade that Installshield doesn't do.

  Created:

  Last revision information:
    $Revision: 13 $
    $Date: 3/07/09 15:23 $
    $Author: Ericsalmon $

  Copyright © Dorset Software Services Ltd, 2003

-------------------------------------------------------------------------------}

unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ExtCtrls, GeneralFunctions, AttachDB, Registry,
  ComCtrls, ApiUtils, ShlObj, ActiveX, DAOTools, PrepareMDB, ComObj, Logger,
  VersionInfo, ExceptionForm, XPMenu;

const
  WM_START = WM_APP + 1;
  ST_BATCHFILE_NAME = 'CreateAccessLinkedTables.bat';

resourcestring
  // Main form caption
  ResStr_SetupCaption         = 'Recorder Database Setup';
  ResStr_AccessDBSetupCaption = 'Recorder Access Database Setup';

  // Close form message
  ResStr_CloseQuery =
      'Closing the wizard before it is complete can leave your Recorder '
      + 'installation in an inconsistent state and can lead to unpredictable '
      + 'results.  Are you sure you want to close the wizard now?';

  ResStr_MDBFailed =
      'Setup failed to create the linked Access database, due to the following error: '#13#10
      + '%s'#13#10#13#10
      + 'This is a minor issue and will not affect core Recorder functionality.'#13#10#13#10
      + 'You can retry creating the Access Database now or at a later date by '
      + 'running the'#13#10' ''' + ST_BATCHFILE_NAME + ''' file located in ''%s''';

  ResStr_Finish                  = 'Finish';
  ResStr_StartingSQLExpress      = 'Starting SQL Express...';
  ResStr_CreatingDBFile          = 'Creating database file...';
  ResStr_ConfiguringDB           = 'Configuring database...';
  ResStr_InstallDirParamRequired = '%s parameter is required for new installations';
  ResStr_SelectMDBPath           = 'Select the path to the nbndata.mdb file that you are going to upgrade.';
  ResStr_MDBPathNotFound         = 'Path to nbndata.mdb not found.  Upgrade aborted.';
  ResStr_DatabasePassword        = 'Database Password';
  ResStr_AccessPassword          = 'Please enter the password for the Access database file (if required)';
  ResStr_CreateOnly =
      'Click All if you want to upgrade a database onto the server '
      + ' or Ok of you want to just attach the database file.';

type
  TInstallType = (itUnknown, itStandalone, itServer, itWorkstation);

  TInstallPhase = (ipAttachDB, ipMigrateOptions, ipLinkDB, ipComplete);

  EInstallError = class(TExceptionPath);

  TfrmMain = class(TForm)
    imgBackground: TImage;
    pnlAttachAuto: TPanel;
    lblAttachDB: TLabel;
    pnlCreateMDB: TPanel;
    lblMigratingData: TLabel;
    lblAttachProgress: TLabel;
    lblPleaseWait: TLabel;
    lblMigrationProcess: TLabel;
    pnlComplete: TPanel;
    lblUpgradeComplete: TLabel;
    lblCompletionInfo: TLabel;
    pnlLogin: TPanel;
    lblLoginOptions: TLabel;
    lblLoginInfo: TLabel;
    gbLogin: TGroupBox;
    rbTrustedLogin: TRadioButton;
    rbSQLLogin: TRadioButton;
    eUsername: TEdit;
    ePassword: TEdit;
    lblUsername: TLabel;
    lblPassword: TLabel;
    pnlProgress: TPanel;
    lblProgress: TLabel;
    pbMDBProgress: TProgressBar;
    lblTotalRecords: TLabel;
    lblLinking: TLabel;
    lblErrors: TLabel;
    lblTableName: TLabel;
    pbAttachProgress: TProgressBar;
    btnNext: TButton;
    lblRunOnServer: TLabel;
    lblContinue: TLabel;
    btnCancel: TButton;
    lblWarningUpgrade: TLabel;
    lblWarningTransfer: TLabel;
    lblWarningOverwrite: TLabel;
    lblDeletingOldData: TLabel;
    lblSubNode: TLabel;
    chkLaunchGuide: TCheckBox;
    chkLaunchRecorder: TCheckBox;
    procedure btnNextClick(Sender: TObject);
    procedure rbSQLLoginClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure btnCancelClick(Sender: TObject);
  private
    FAborted : Boolean;
    FInstallType : TInstallType;  // read from params
    FInstallDir  : String;        // read from params
    FDatabaseDir : String;        // read from params
    FRemotesrv   : Boolean;       // read from params
    FServerName  : String;        // read from params
    FUserName: string;
    FPassword: string;
    FTrustedLogin: boolean;
    FAddinPath: String;
    FDBPath: String;
    FDBPassword: String;
    FInstallPhase: TInstallPhase;
    FCreateOnly: Boolean;
    FLoginRequired: Boolean;
    FRecreateMDB: Boolean;
    FXPMenu: TXPMenu;
    procedure AttachDatabase;
    procedure AttachProgress(AValue: Integer);
    procedure CompletionPage;
    procedure DeleteOldData;
    procedure HidePanels;
    procedure MigrationOptions;
    procedure PrepareLinkedMDB;
    procedure PrepareRerunProcess(const installDir, cmdLine: String);
    procedure ReadParams;
    procedure ReadSettings;
    function SelectApplicationPath(const istFileName, istFilter: String): String;
    procedure SetInstallPhase(const Value: TInstallPhase);
    procedure WMStart(var Message : TMessage); message WM_START;

    property InstallPhase: TInstallPhase read FInstallPhase write SetInstallPhase;
    function GetRegistryInstanceName: string;
  public
    constructor Create(AOwner: TComponent); override;
  end;

var
  frmMain: TfrmMain;

//==============================================================================
implementation

{$R *.dfm}

uses
  SetupConstants, DBMigrator;

//==============================================================================
{ TfrmMain }
{===============================================================================
 Description : Constructor displays just the intro panel
 Created : 16/1/2003 }
constructor TfrmMain.Create(AOwner: TComponent);
begin
  inherited;
  FAborted       := False;
  FLoginRequired := True; // we can use this to override hiding of the connection options
                          // should default to true for SQL Express
  HidePanels;
  ReadParams;
  ReadSettings;
  PostMessage(Handle, WM_START, 0, 0);

  FXPMenu := TXPMenu.Create(Self);
  FXPMenu.XPControls := FXPMenu.XPControls - [xcCombo, xcGroupBox];
  FXPMenu.Active := True;
  // allow for CDs with no getting started guide
  if (not FileExists(ExtractFilePath(Application.ExeName) + '..\' + STR_GETTING_STARTED)) then begin
    chkLaunchGuide.Visible := false;
    chkLaunchGuide.Checked := false;
  end;
end;

{===============================================================================
 Description : Hide all the stage panels
 Created : 16/1/2003 }
procedure TfrmMain.HidePanels;
begin
  pnlAttachAuto.Visible := False;
  pnlCreateMDB.Visible  := False;
  pnlLogin.Visible      := False;
  pnlComplete.Visible   := False;
end;

{===============================================================================
 Description : Read the command line parameters.  Hide appropriate labels on
             intro page if settings are turned off.
 Created : 16/1/2003 }
procedure TfrmMain.ReadParams;
var
  liParamIndex : integer;
begin
  FRemotesrv   := False;
  FInstallType := itUnknown;
  FServerName  := '(local)';
  FDatabaseDir := '';
  FUserName    := '';
  FPassword    := '';
  FTrustedLogin:= true;

  for liParamIndex := 1 to ParamCount do begin
    if SameText(ParamStr(liParamIndex), '/remotesrv') then begin
      FRemotesrv := True;
      Log('remotesrv parameter detected', False);
    end else
    if SameText(ParamStr(liParamIndex), '/standalone') then begin
      FInstallType := itStandalone;
      Caption := ResStr_SetupCaption;
      Log('Standalone parameter detected', False);
    end else
    if SameText(ParamStr(liParamIndex), '/server') then begin
      FInstallType := itServer;
      Caption := ResStr_SetupCaption;
      Log('Server parameter detected', False);
    end else
    if SameText(ParamStr(liParamIndex), '/workstation') then begin
      FInstallType := itWorkstation;
      Log('Workstation parameter detected', False);
      Log('Do not call ' + Application.Title + ' with the /workstation parameter', False);
      Abort; // don't bother creating the form
    end else
    if SameText(Copy(ParamStr(liParamIndex), 1, 11), '/installdir') then begin
      FInstallDir := IncludeTrailingPathDelimiter(
          Copy(ParamStr(liParamIndex), 13, Length(ParamStr(liParamIndex))));
      Log('Installdir parameter read - ' + FInstallDir, False);
    end else
    if SameText(Copy(ParamStr(liParamIndex), 1, 12), '/databasedir') then begin
      FDatabaseDir := IncludeTrailingPathDelimiter(
          Copy(ParamStr(liParamIndex), 14, Length(ParamStr(liParamIndex))));
      Log('Databasedir parameter read - ' + FDatabaseDir, False);
    end else
    if SameText(ParamStr(liParamIndex), '/mdb') then begin
      FLoginRequired := True;
      FRecreateMDB            := True;
      Caption                 := ResStr_AccessDBSetupCaption;
      Log('mdb parameter detected', False);
    end else
    // note we use the command line to pass the username and password, but it is never
    // stored in the batch file. Mustn't leave this sort of info lying around in a text file.
    if SameText(Copy(ParamStr(liParamIndex), 1, 9), '/username') then begin
      FUserName := Copy(ParamStr(liParamIndex), 11, Length(ParamStr(liParamIndex)));
      eUsername.Text := FUserName;
    end else
    if SameText(Copy(ParamStr(liParamIndex), 1, 9), '/password') then begin
      FPassword := Copy(ParamStr(liParamIndex), 11, Length(ParamStr(liParamIndex)));
      ePassword.Text := FPassword;
    end;
  end;

  // Get the server name, as specified in first part of install process
  if FInstallType in [itStandalone, itServer] then
    with TStringList.Create do
      try
        LoadFromFile(FInstallDir + STR_INSTALLSETTINGS);
        FServerName := Values['Server Name'];
        rbTrustedLogin.Checked := Values['Trusted Security'] = '1';
        FTrustedLogin := Values['Trusted Security'] = '1';
      finally
        Free;
      end;
  FLoginRequired := (FTrustedLogin=false) and ((FUserName='') or (FPassword=''));
end;

{===============================================================================
 Description : Kick of the update when the proceed button is clicked.
  Created : 16/1/2003 }
procedure TfrmMain.btnNextClick(Sender: TObject);
var
  lCursor : TCursor;
begin
  // If clicked on complete page, close app, 'coz all's finished
  if InstallPhase = ipComplete then
  begin
    if chkLaunchGuide.Checked then
      ShellFile(ExtractFilePath(Application.ExeName) + '..\' + STR_GETTING_STARTED);
    if chkLaunchRecorder.Checked then
      ShellFile(FInstallDir + STR_RECORDER_SPLASH_EXE);
    Close;
  end else begin
    lCursor := HourglassCursor;
    btnNext.Enabled := False; // no double clicks please!
    try
      case InstallPhase of
        ipMigrateOptions: begin
          FUserName := eUsername.Text;
          FPassword := ePassword.Text;
          FTrustedLogin := rbTrustedLogin.Checked;
          InstallPhase := ipAttachDB;          
        end;
        ipAttachDB      : InstallPhase := ipLinkDB;
        ipLinkDB        : InstallPhase := ipComplete;
      end;
    finally
      DefaultCursor(lCursor);
    end;
  end;
end;

{===============================================================================
 Description : Displays the 'Installation Complete' page.
 Created : }
procedure TfrmMain.CompletionPage;
begin
  Log('Installation Completed', False);
  // No need to display anything if all went well, just close the app
  with btnNext do begin
    Caption := ResStr_Finish;
    Enabled := True;
    Default := True;
    SetFocus;
  end;
  HidePanels;
  pnlComplete.Visible := True;
  btnCancel.Enabled   := False;
end;

{===============================================================================
 Description : Attach the installation database
 Created : 16/1/2003 }
procedure TfrmMain.AttachDatabase;
var lAttachDB: TAttachDB;
    lCursor  : TCursor;
begin
  HidePanels;
  pnlAttachAuto.Visible := True;
  Application.ProcessMessages;
  Log('Attaching database', False);
  // Save trusted security setting to registry, so that Recorder knows about it.
  with TRegistry.Create do
    try
      RootKey := HKEY_LOCAL_MACHINE;
      Access  := KEY_ALL_ACCESS;
      OpenKey(REG_KEY_R6, False);
      WriteBool('Trusted Security', rbTrustedLogin.Checked);
      CloseKey;
    finally
      Free;
    end;
  lAttachDB := TAttachDB.Create(FServerName, GetRegistryInstanceName, AttachProgress, FUserName,
                                FPassword, FTrustedLogin);
  lCursor := HourglassCursor;
  try
    try
      lblAttachProgress.Caption := ResStr_StartingSQLExpress;
      if lAttachDB.ConnectAndCheckDb then
      begin
        lblAttachProgress.Caption := ResStr_CreatingDBFile;
        lAttachDB.CopyMDF(
            ExtractFilePath(Application.ExeName) + 'Database\' + MDF_FILENAME + '.zip',
            FDatabaseDir);
        lblAttachProgress.Caption := ResStr_ConfiguringDB;
        lAttachDB.InitDB(true);
      end else begin
        MessageDlg(lAttachDB.ErrorMessage, mtWarning, [mbOk], 0);
        FLoginRequired := True;
        InstallPhase := ipMigrateOptions;
        Exit;
      end;
    finally
      lAttachDB.Free;
      DefaultCursor(lCursor);
    end;
  except
    on EAttachAborted do begin
      Log('Attaching the database file was aborted as an NBNData database already exists', False);
      FAborted := True;
      Close;
      Abort;
    end;
  end; // try
  // SQL Express setup ok, carry on.
  btnNextClick(nil)
end;

{-------------------------------------------------------------------------------
  Description : Prepares an nbndata.mdb file in the old database path that links
      the new SQL Server tables in.
  Created : 14/02/2003 }
procedure TfrmMain.PrepareLinkedMDB;
var lAttributes: Word;
    lDBFileName: String;
begin
  // If MigrateDatabase was skipped, the panel is still hidden!!!
  HidePanels;
  pnlCreateMDB.Visible := True;
  Application.ProcessMessages;

  //Create batch file to redo Access Links
  if not FRecreateMDB then PrepareRerunProcess(FInstallDir, CmdLine);

  // Filename of Access database file in [InstallFolder]
  lDBFileName := ExtractFilePath(FDBPath) + 'nbndata.mdb';

  Log('Preparing linked mdb database file', False);
  DeleteOldData;
  // Some labels are moved during DeleteOldData, so put them back, and change the captions too.
  lblTableName.Caption := '';
  Application.ProcessMessages;

  // Ensure folder exists before trying to copy file across.
  ForceDirectories(ExtractFilePath(FDBPath));
  // Now copy new empty MDB
  CopyFile(PChar(ExtractFilePath(Application.ExeName) + 'Database\nbndata.mdb'),
           PChar(lDBFileName), False);
  // Remove readonly flag from copied file
  lAttributes := FileGetAttr(lDBFileName);
  lAttributes := lAttributes and not SysUtils.faReadOnly;
  FileSetAttr(lDBFileName, lAttributes);
  // Create the links
  lblLinking.Visible   := True;  // Now it can be shown
  lblTableName.Visible := True;
  with TPrepareMDB.Create(FServerName, lDBFileName, rbTrustedLogin.Checked,
                          eUsername.Text, ePassword.Text) do
    try
      Execute(pbMDBProgress, lblTableName);
    finally
      Free;
    end;
end;

{-------------------------------------------------------------------------------
  Description : Removes the now upgraded Access data files
  Created : 17/02/2003 }
procedure TfrmMain.DeleteOldData;
var
  lSearchRec: TSearchRec;
begin
  lblDeletingOldData.Visible := True;
  if FindFirst(ExtractFilePath(FDBPath) + '*.mdb', 0, lSearchRec) = 0 then
    repeat
      // don't delete upgrade errors file
      if CompareText(lSearchRec.Name, UPGRADE_ERROR_FILE) <> 0 then
        DeleteFile(ExtractFilePath(FDBPath) + lSearchRec.Name);
    until FindNext(lSearchRec) <> 0;
  FindClose(lSearchRec);
  lblDeletingOldData.Visible := False;
end;

{===============================================================================
 Description : Displays the migration options panel.  This is only required when
             noappfiles is set and migration is enabled, because we need to find
             out the server login details.
 Created : 28/1/2003 }
procedure TfrmMain.MigrationOptions;
begin
  if FLoginRequired then
  begin
    HidePanels;
    if FInstallType = itStandalone then
    begin
      lblRunOnServer.Visible := False;
      gbLogin.Top := lblRunOnServer.Top;
    end;
    pnlLogin.Visible := True;
    btnNext.Enabled  := True;
  end;
end;

{===============================================================================
 Description : Read any required registry settings
 Created : 17/1/2003 }
procedure TfrmMain.ReadSettings;
var
  reg: TRegistry;
begin
  FDBPath     := '';
  FCreateOnly := False;
  reg         := TRegistry.Create;
  try
    if reg.OpenKeyReadOnly(REG_KEY_R6_SETTINGS) then begin
      FDBPath     := reg.ReadString('Database Path');
      FDBPassword := reg.ReadString('Database Password');
      FAddinPath  := reg.ReadString('Addin Path');
    end;
    if FDBPath = '' then
    begin
      // request database path from user or use installdir param
      if FInstallType in [itStandalone, itServer] then
      begin
        if FInstallDir = '' then
          raise EInstallError.Create(Format(ResStr_InstallDirParamRequired, ['/installdir']));
        FDBPath := IncludeTrailingPathDelimiter(FInstallDir) + 'Database\nbndata.mdb';
      end else begin
        if FInstallType = itUnknown then
          FCreateOnly := MessageDlg(ResStr_CreateOnly, mtConfirmation, [mbAll, mbOk], 0) = mrOK;
        if (FInstallType <> itUnknown) or (not FCreateOnly) then begin
          FDBPath := SelectApplicationPath('nbndata.mdb', 'Access 97 files (*.mdb)|*.mdb');

          if not InputQuery(ResStr_DatabasePassword, ResStr_AccessPassword, FDBPassword) then
            Abort;
        end;
      end;
    end;
    reg.CloseKey;
  finally
    reg.Free;
  end;
end;

{===============================================================================
 Description : Returns the path to the application, by asking the user with
             a file open dialog.
 Created : 16/1/2003 }
function TfrmMain.SelectApplicationPath(const istFileName, istFilter: String): String;
var
  lOpenDlg: TOpenDialog;
begin
  lOpenDlg := TOpenDialog.Create(nil);
  try
    lOpenDlg.Title    := ResStr_SelectMDBPath;
    lOpenDlg.FileName := istFileName;
    lOpenDlg.Filter   := istFilter;
    if lOpenDlg.Execute then
      Result := lOpenDlg.FileName
    else begin
      MessageDlg(ResStr_MDBPathNotFound, mtError, [mbOk], 0);
      Close;
    end;
  finally
    lOpenDlg.Free;
  end; // try
end;

{===============================================================================
 Description : Ensure username and password boxes disabled when not required
 Created : 28/1/2003 }
procedure TfrmMain.rbSQLLoginClick(Sender: TObject);
begin
  ePassword.Enabled := rbSQLLogin.Checked;
  eUsername.Enabled := rbSQLLogin.Checked;
end;

{-------------------------------------------------------------------------------
  Description : Callback to set the progress during the attach
  Created : 17/03/2003 }
procedure TfrmMain.AttachProgress(AValue: Integer);
begin
  pbAttachProgress.Position := AValue;
  Application.ProcessMessages;
end;

{-------------------------------------------------------------------------------
  Description : Sets the current install phase, including skipping phases
              depending on the type of install
  Created : 14/02/2003 }
procedure TfrmMain.SetInstallPhase(const Value: TInstallPhase);
begin
  try
    FInstallPhase := Value;
    case FInstallPhase of
      ipMigrateOptions :
        if FLoginRequired then
          MigrationOptions
        else
          btnNextClick(nil);
      ipAttachDB :
        if not (FRemotesrv or FRecreateMDB) then AttachDatabase else btnNextClick(nil);
      ipLinkDB :
        if (not FCreateOnly) or FRecreateMDB then begin
          try
            PrepareLinkedMDB;
            btnNextClick(nil);
          except
            on E:Exception do begin
              if MessageDlg(
                  Format(ResStr_MDBFailed, [E.Message, FInstallDir]),
                  mtInformation, [mbRetry, mbIgnore], 0) = mrRetry then
                SetInstallPhase(ipLinkDB)
              else
                btnNextClick(nil);
            end;
          end;
        end else
          btnNextClick(nil);
      ipComplete :
        CompletionPage;
    end;
  except
    on E:Exception do begin
      Log('Error: ' + E.Classname + ' in phase ' + IntToStr(Ord(FInstallPhase)), False);
      Log(E.Message, False);
      raise;
    end;
  end; // try
end;

{-------------------------------------------------------------------------------
  Description : Start the whole thing off, once properly created
  Created : 19/03/2003 }
procedure TfrmMain.WMStart(var Message: TMessage);
begin
  InstallPhase := ipMigrateOptions;
  BringWindowToTop(Self.Handle);
end;

{-------------------------------------------------------------------------------
  If not completed, then prompt the user not to close the form as this could
  leave the install in an unpredictable state.
}
procedure TfrmMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if (InstallPhase <> ipComplete) and (not FAborted) then
    if MessageDlg(ResStr_CloseQuery, mtInformation, [mbOk, mbCancel], 0) <> mrOk then
      CanClose := False;
end;

{-------------------------------------------------------------------------------
  Prepare batch file to rerun the Access db linking.
}
procedure TfrmMain.PrepareRerunProcess(const installDir, cmdLine: String);
var
  idx: Integer;
begin
  with TStringList.Create do
    try
      if Pos('/mdb', cmdLine) = 0 then
        Add(cmdLine + ' /mdb')
      else
        Add(cmdLine);
      SaveToFile(installDir + ST_BATCHFILE_NAME);
      if FileExists(installDir + STR_INSTALLLOG) then
      begin
        LoadFromFile(installDir + STR_INSTALLLOG);
        idx := IndexOf(STR_FILES_SECTION);
        if idx > -1 then
        begin
          Insert(idx + 1, installDir + ST_BATCHFILE_NAME);
          SaveToFile(installDir + STR_INSTALLLOG);
        end;
      end;
    finally
      Free;
    end;
end;

//------------------------------------------------------------------------------
procedure TfrmMain.btnCancelClick(Sender: TObject);
begin
  Close;
end;

{-------------------------------------------------------------------------------
  Retrieve the registry version of a user entered instance name.  In SQL 2005
    these are not the same!
}              
function TfrmMain.GetRegistryInstanceName: string;
var
  instanceName: string;
var
  lComputerName: Array[0..MAX_COMPUTERNAME_LENGTH + 1] of Char;
  lSize: Cardinal;
begin
  lSize := MAX_COMPUTERNAME_LENGTH + 1;
  GetComputerName(lComputerName, lSize);
  instanceName := Copy(FServerName, Length(lComputerName) + 2, 255);
  Result := '';
  with TRegistry.Create do
    try
      RootKey := HKEY_LOCAL_MACHINE;
      Access  := KEY_READ;
      if KeyExists(REG_KEY_SQL2005_INSTANCES) then begin
        OpenKey(REG_KEY_SQL2005_INSTANCES, FALSE);
        if ValueExists(instanceName) then begin
          Result := ReadString(Result);
        end;
      end;
      // in vista or higher, also check 64 bit registry
      if (Result='') and (GetOSVersion>=wvVista) then begin
        Access := KEY_READ OR KEY_WOW64_64KEY;
        if KeyExists(REG_KEY_SQL2005_INSTANCES) then begin
          OpenKey(REG_KEY_SQL2005_INSTANCES, FALSE);
          if ValueExists(instanceName) then begin
            Result := ReadString(instanceName);
          end;
        end;
      end;
    finally
      Free;
    end;
end;

end.
