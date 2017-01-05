{===============================================================================
  Program:     Setup

  Description: Setup for Recorder 6 standalone version.

  Created:     March 2004

  Last revision information:
    $Revision: $
    $Date: $
    $Author: $

===============================================================================}

program Setup;



uses
  SHFolder,
  madExcept,
  madListHardware,
  madListProcesses,
  madListModules,
  Forms,
  LocOnFly,
  Sysutils,
  Recorder2000_TLB in '..\..\..\..\RecorderApp\Recorder2000_TLB.pas',
  BaseADODataModule in '..\..\..\..\Third Party\Dorset Software Services\DssVcl32\BaseADODataModule.pas' {dmBaseADO: TDataModule},
  DisplayFuncs in '..\..\..\..\Third Party\Dorset Software Services\DssVcl32\DisplayFuncs.pas',
  ExceptionForm in '..\..\..\..\Third Party\Dorset Software Services\DssVcl32\ExceptionForm.pas' {frmException},
  ADODB_TLB in '..\..\..\..\Third Party\Dorset Software Services\DssVcl32\ADODB_TLB.pas',
  ADOX_TLB in '..\..\..\..\Third Party\Dorset Software Services\DssVcl32\ADOX_TLB.pas',
  CustomOleCtrls7 in '..\..\..\..\Third Party\Dorset Software Services\DssVcl32\CustomOleCtrls7.pas',
  FolderBrowser in '..\..\..\..\Third Party\Dorset Software Services\DssVcl32\FolderBrowser.pas',
  HotLabel in '..\..\..\..\Third Party\Dorset Software Services\DssVcl32\HotLabel.pas',
  OleTools in '..\..\..\..\Third Party\Dorset Software Services\DssVcl32\OleTools.pas',
  Relationships_ADO in '..\..\..\..\Third Party\Dorset Software Services\DssVcl32\Relationships_ADO.pas',
  RestrictedEdits in '..\..\..\..\Third Party\Dorset Software Services\DssVcl32\RestrictedEdits.pas',
  ServiceHandler in '..\..\..\..\Third Party\Dorset Software Services\DssVcl32\ServiceHandler.pas',
  SQLList in '..\..\..\..\Third Party\Dorset Software Services\DssVcl32\SQLList.pas',
  TablePriorityList_ADO in '..\..\..\..\Third Party\Dorset Software Services\DssVcl32\TablePriorityList_ADO.pas',
  XPMenu in '..\..\..\..\Third Party\Khaled Shagrouni\XP Menu\XPMenu.pas',
  DatabaseAccessADO in '..\..\..\..\RecorderApp\Source\DatabaseAccessADO.pas' {dmDatabase: TDataModule},
  Constants in '..\..\..\..\RecorderApp\Source\Constants.pas',
  Logger in '..\..\Database Setup 6\Logger.pas',
  AttachDB in '..\..\Common 6\AttachDB.pas',
  BasePage in '..\..\Common 6\BasePage.pas' {BasePage: TFrame},
  BaseSQLExpressSettingsPage in '..\..\Common 6\BaseSQLExpressSettingsPage.pas' {BaseSQLExpressSettingsPage: TFrame},
  DBFileLocationSelectFrame in '..\..\Common 6\DBFileLocationSelectFrame.pas' {fraDBFileLocationSelect: TFrame},
  DBMigrator in '..\..\Common 6\DBMigrator.pas',
  Functions in '..\..\Common 6\Functions.pas',
  InstallAddins in '..\..\Common 6\InstallAddins.pas',
  InstallFiles in '..\..\Common 6\InstallFiles.pas',
  MigrateComplete in '..\..\Common 6\MigrateComplete.pas' {fraMigrateComplete: TFrame},
  MigratePage in '..\..\Common 6\MigratePage.pas' {fraMigrate: TFrame},
  MigrateSettings1Page in '..\..\Common 6\MigrateSettings1Page.pas' {fraMigrateSettings1: TFrame},
  MigrateSettings2Page in '..\..\Common 6\MigrateSettings2Page.pas' {fraMigrateSettings2: TFrame},
  MigrateWelcomePage in '..\..\Common 6\MigrateWelcomePage.pas' {fraMigrateWelcome: TFrame},
  MigrationSettings in '..\..\Common 6\MigrationSettings.pas',
  RebootFunctions in '..\..\Common 6\RebootFunctions.pas',
  RemovalSettings in '..\..\Common 6\RemovalSettings.pas',
  RemoveCompletePage in '..\..\Common 6\RemoveCompletePage.pas' {fraRemoveComplete: TFrame},
  RemovePage in '..\..\Common 6\RemovePage.pas' {fraRemove: TFrame},
  RemoveWelcomePage in '..\..\Common 6\RemoveWelcomePage.pas' {fraRemoveWelcome: TFrame},
  Settings in '..\..\Common 6\Settings.pas',
  SetupConstants in '..\..\Common 6\SetupConstants.pas',
  Shortcuts in '..\..\Common 6\Shortcuts.pas',
  TextMessages in '..\..\Common 6\TextMessages.pas',
  TransferQuery in '..\..\Common 6\TransferQuery.pas',
  WbemScripting_TLB in '..\..\Common 6\WbemScripting_TLB.pas',
  CompletionPage in 'Source\CompletionPage.pas' {fraCompletion: TFrame},
  DatabaseServerSelectPage in 'Source\DatabaseServerSelectPage.pas' {fraDatabaseServerSelect: TFrame},
  InstallationPage in 'Source\InstallationPage.pas' {fraInstallation: TFrame},
  InstallFolderPage in 'Source\InstallFolderPage.pas' {fraInstallFolder: TFrame},
  LoginPage in 'Source\LoginPage.pas' {fraLogin: TFrame},
  Main in 'Source\Main.pas' {frmMain},
  SelectExistingServerPage in 'Source\SelectExistingServerPage.pas' {fraSelectExistingServerPage: TFrame},
  SiteInfoPage in 'Source\SiteInfoPage.pas' {fraSiteInfo: TFrame},
  SpatialRefPage in 'Source\SpatialRefPage.pas' {fraSpatialRef: TFrame},
  WelcomePage in 'Source\WelcomePage.pas' {fraWelcome: TFrame},
  DAO_TLB in '..\..\..\..\RecorderApp\Components\DAO_TLB.pas',
  DAOTools in '..\..\..\..\RecorderApp\Components\DAOTools.pas',
  DatabaseUtilities in '..\..\..\..\RecorderApp\Components\DatabaseUtilities.pas',
  DataClasses in '..\..\..\..\RecorderApp\Components\DataClasses.pas',
  GenFuncs in '..\..\..\..\RecorderApp\Components\Genfuncs.pas',
  JNCCRelationships in '..\..\..\..\RecorderApp\Components\JNCCRelationships.pas',
  JNCCDatasets in '..\..\..\..\RecorderApp\Components\JNCCDatasets.pas',
  JRO_TLB in '..\..\..\..\RecorderApp\Components\JRO_TLB.pas',
  SpatialRefFuncs in '..\..\..\..\RecorderApp\Components\SpatialRefFuncs.pas',
  SQLConverter in '..\..\..\..\RecorderApp\Components\SQLConverter.pas',
  VagueDate in '..\..\..\..\RecorderApp\Components\VagueDate.pas',
  easyshell in '..\..\..\..\Third Party\Dorset Software Services\DssVcl32\EasyShell.pas',
  DatabaseFileLocationPage in 'Source\DatabaseFileLocationPage.pas' {fraDatabaseFileLocation: TFrame},
  VersionInfo in '..\..\..\..\Third Party\Dorset Software Services\DssVcl32\VersionInfo.pas',
  GeneralFunctions in '..\..\..\..\Third Party\Dorset Software Services\DssVcl32\GeneralFunctions.pas',
  ListDlls in '..\..\..\..\Third Party\Dorset Software Services\DssVcl32\ListDlls.pas',
  ApiUtils in '..\..\..\..\Third Party\Dorset Software Services\DssVcl32\ApiUtils.pas';

{$R Setup.KLR}

{$R *.res}
{$R StandaloneSetup.res}

begin
  LocalizerOnFly.Init;
  // try to find exact match on language & sub language e.g. French (Luxembourg)
  if LocalizerOnFly.LangList.IndexByLocale(Sysutils.SysLocale.DefaultLCID)>-1 then
    LocalizerOnFly.SwitchTo(Sysutils.SysLocale.DefaultLCID)
  // else try to find a match on generic language e.g. French
  else if LocalizerOnFly.LangList.IndexByLocale(Sysutils.SysLocale.PriLangID)>-1 then
    LocalizerOnFly.SwitchTo(Sysutils.SysLocale.PriLangID)
  else
    LocalizerOnFly.SwitchTo(LocalizerOnFly.NativeLocale);
  RegisterExceptionHandler(MadExceptionHandler, stTrySyncCallOnSuccess);
  madExceptErrorPath := GetFolder(CSIDL_COMMON_DOCUMENTS)+'Recorder 6\Errors\';
  Application.Initialize;
  Application.Title := 'Recorder 6 Setup';
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
