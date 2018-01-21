{===============================================================================
  Program:     Setup

  Description: Setup for Recorder 6 Server version.

  Created:     March 2004

  Last revision information:
    $Revision: $
    $Date: $
    $Author: $

===============================================================================}

program SetupApp;

{$R 'setup.manifest.res' 'setup.manifest.rc'}

uses
  SHFolder,
  madExcept,
  madListHardware,
  madListProcesses,
  madListModules,
  Forms,
  LocOnFly,
  Dialogs,
  Sysutils,
  Recorder2000_TLB         in '..\..\..\..\RecorderApp\Recorder2000_TLB.pas',
  ADOX_TLB                 in '..\..\..\..\Third Party\Dorset Software Services\DssVcl32\ADOX_TLB.pas',
  ADODB_TLB                in '..\..\..\..\Third Party\Dorset Software Services\DssVcl32\ADODB_TLB.pas',
  ApiUtils                 in '..\..\..\..\Third Party\Dorset Software Services\DssVcl32\ApiUtils.pas',
  BaseADODataModule        in '..\..\..\..\Third Party\Dorset Software Services\DssVcl32\BaseADODataModule.pas' {dmBaseADO: TDataModule},
  CustomOleCtrls7          in '..\..\..\..\Third Party\Dorset Software Services\DssVcl32\CustomOleCtrls7.pas',
  DisplayFuncs             in '..\..\..\..\Third Party\Dorset Software Services\DssVcl32\DisplayFuncs.pas',
  EasyShell                in '..\..\..\..\Third Party\Dorset Software Services\DssVcl32\EasyShell.pas',
  ExceptionForm            in '..\..\..\..\Third Party\Dorset Software Services\DssVcl32\ExceptionForm.pas',
  FolderBrowser            in '..\..\..\..\Third Party\Dorset Software Services\DssVcl32\FolderBrowser.pas',
  GeneralFunctions         in '..\..\..\..\Third Party\Dorset Software Services\DssVcl32\GeneralFunctions.pas',
  HotLabel                 in '..\..\..\..\Third Party\Dorset Software Services\DssVcl32\HotLabel.pas',
  ListDlls                 in '..\..\..\..\Third Party\Dorset Software Services\DssVcl32\ListDlls.pas',
  OleTools                 in '..\..\..\..\Third Party\Dorset Software Services\DssVcl32\OleTools.pas',
  Relationships_ADO        in '..\..\..\..\Third Party\Dorset Software Services\DssVcl32\Relationships_ADO.pas',
  RestrictedEdits          in '..\..\..\..\Third Party\Dorset Software Services\DssVcl32\RestrictedEdits.pas',
  ServiceHandler           in '..\..\..\..\Third Party\Dorset Software Services\DssVcl32\ServiceHandler.pas',
  SQLList                  in '..\..\..\..\Third Party\Dorset Software Services\DssVcl32\SQLList.pas',
  TablePriorityList_ADO    in '..\..\..\..\Third Party\Dorset Software Services\DssVcl32\TablePriorityList_ADO.pas',
  VersionInfo              in '..\..\..\..\Third Party\Dorset Software Services\DssVcl32\VersionInfo.pas',

  MS5                      in '..\..\..\..\Third Party\Graticule\MS5.pas',

  Constants                in '..\..\..\..\RecorderApp\Source\Constants.pas',
  DatabaseAccessADO        in '..\..\..\..\RecorderApp\Source\DatabaseAccessADO.pas' {dmDatabase: TDataModule},

  AttachDB                 in '..\..\Common 6\AttachDB.pas',
  BasePage                 in '..\..\Common 6\BasePage.pas' {BasePage: TFrame},
  BaseSQLExpressSettingsPage     in '..\..\Common 6\BaseSQLExpressSettingsPage.pas' {fraBaseMSDESettings: TFrame},
  DBFileLocationSelectFrame      in '..\..\Common 6\DBFileLocationSelectFrame.pas' {fraDBFileLocationSelectFrame: TFrame},
  DBMigrator               in '..\..\Common 6\DBMigrator.pas',
  Functions                in '..\..\Common 6\Functions.pas',
  InstallAddins            in '..\..\Common 6\InstallAddins.pas',
  InstallFiles             in '..\..\Common 6\InstallFiles.pas',
  MigrateComplete          in '..\..\Common 6\MigrateComplete.pas' {fraMigrateComplete: TFrame},
  MigrateSettings1Page     in '..\..\Common 6\MigrateSettings1Page.pas' {fraMigrateSettings1: TFrame},
  MigrateSettings2Page     in '..\..\Common 6\MigrateSettings2Page.pas' {fraMigrateSettings2: TFrame},
  MigratePage              in '..\..\Common 6\MigratePage.pas' {fraMigrate: TFrame},
  MigrateWelcomePage       in '..\..\Common 6\MigrateWelcomePage.pas' {fraMigrateWelcome: TFrame},
  MigrationSettings        in '..\..\Common 6\MigrationSettings.pas',
  PrepareMDB               in '..\..\Common 6\PrepareMDB.pas',
  Settings                 in '..\..\Common 6\Settings.pas',
  SetupConstants           in '..\..\Common 6\SetupConstants.pas',
  Shortcuts                in '..\..\Common 6\Shortcuts.pas',
  TextMessages             in '..\..\Common 6\TextMessages.pas',
  TransferQuery            in '..\..\Common 6\TransferQuery.pas',
  RemovalSettings          in '..\..\Common 6\RemovalSettings.pas',
  RemoveCompletePage       in '..\..\Common 6\RemoveCompletePage.pas' {fraRemoveComplete: TFrame},
  RemovePage               in '..\..\Common 6\RemovePage.pas' {fraRemove: TFrame},
  RemoveWelcomePage        in '..\..\Common 6\RemoveWelcomePage.pas' {fraRemoveWelcome: TFrame},
  WbemScripting_TLB        in '..\..\Common 6\WbemScripting_TLB.pas',

  Logger                   in '..\..\Database Setup 6\Logger.pas',

  DatabaseServerSelectPage in 'Source\DatabaseServerSelectPage.pas' {fraDatabaseServerSelect: TFrame},
  CompletionPage           in 'Source\CompletionPage.pas' {fraCompletion: TFrame},
  InstallationPage         in 'Source\InstallationPage.pas' {fraInstallation: TFrame},
  InstallFolderPage        in 'Source\InstallFolderPage.pas' {fraInstallFolder: TFrame},
  LoginPage                in 'Source\LoginPage.pas' {fraLogin: TFrame},
  Main                     in 'Source\Main.pas' {frmMain},
  ServerSelectPage         in 'Source\ServerSelectPage.pas' {fraServerSelect: TFrame},
  SiteInfoPage             in 'Source\SiteInfoPage.pas' {fraSiteInfo: TFrame},

  DatabaseUtilities        in '..\..\..\..\RecorderApp\Components\DatabaseUtilities.pas',
  DataClasses              in '..\..\..\..\RecorderApp\Components\DataClasses.pas',
  DAOTools                 in '..\..\..\..\RecorderApp\Components\DAOTools.pas',
  DAO_TLB                  in '..\..\..\..\RecorderApp\Components\DAO_TLB.pas',
  GenFuncs                 in '..\..\..\..\RecorderApp\Components\Genfuncs.pas',
  JNCCDatasets             in '..\..\..\..\RecorderApp\Components\JNCCDatasets.pas',
  JNCCRelationships        in '..\..\..\..\RecorderApp\Components\JNCCRelationships.pas',
  SpatialRefFuncs          in '..\..\..\..\RecorderApp\Components\SpatialRefFuncs.pas',
  SQLConverter             in '..\..\..\..\RecorderApp\Components\SQLConverter.pas',
  VagueDate                in '..\..\..\..\RecorderApp\Components\VagueDate.pas';

{$R SetupApp.KLR}

{$R *.res}
{$R ServerSetup.res}

begin
  LocalizerOnFly.Init;
  // try to find exact match on language & sub language e.g. French (Luxembourg)
  if LocalizerOnFly.LangList.IndexByLocale(Sysutils.SysLocale.DefaultLCID) >- 1 then
    LocalizerOnFly.SwitchTo(Sysutils.SysLocale.DefaultLCID)
  // else try to find a match on generic language e.g. French
  else if LocalizerOnFly.LangList.IndexByLocale(Sysutils.SysLocale.PriLangID) >- 1 then
    LocalizerOnFly.SwitchTo(Sysutils.SysLocale.PriLangID)
  else
    LocalizerOnFly.SwitchTo(LocalizerOnFly.NativeLocale);
  RegisterExceptionHandler(MadExceptionHandler, stTrySyncCallOnSuccess);
  madExceptErrorPath := GetFolder(CSIDL_COMMON_DOCUMENTS)+'Recorder 6\Errors\';
  Application.Initialize;
  Application.Title := 'Recorder 6 Server Setup';
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
