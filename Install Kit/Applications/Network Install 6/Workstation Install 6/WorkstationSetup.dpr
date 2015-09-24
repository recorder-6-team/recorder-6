{===============================================================================
  Program:     WorkstationSetup

  Description: Setup for Recorder 6 Workstation version.

  Created:     March 2004

  Last revision information:
    $Revision: $
    $Date: $
    $Author: $

===============================================================================}

program WorkstationSetup;

uses
  SHFolder,
  madExcept,
  madListHardware,
  madListProcesses,
  madListModules,
  Forms,
  LocOnFly,
  Sysutils,
  Recorder2000_TLB         in '..\..\..\..\..\RecorderApp\trunk\Recorder2000_TLB.pas',
  ADODB_TLB                in '..\..\..\..\..\Third Party\Dorset Software Services\DssVcl32\trunk\ADODB_TLB.pas',
  ADOX_TLB                 in '..\..\..\..\..\Third Party\Dorset Software Services\DssVcl32\trunk\ADOX_TLB.pas',
  ApiUtils                 in '..\..\..\..\..\Third Party\Dorset Software Services\DssVcl32\trunk\ApiUtils.pas',  
  BaseADODataModule        in '..\..\..\..\..\Third Party\Dorset Software Services\DssVcl32\trunk\BaseADODataModule.pas' {dmBaseADO: TDataModule},
  CustomOleCtrls7          in '..\..\..\..\..\Third Party\Dorset Software Services\DssVcl32\trunk\CustomOleCtrls7.pas',
  DisplayFuncs             in '..\..\..\..\..\Third Party\Dorset Software Services\DssVcl32\trunk\DisplayFuncs.pas',
  ExceptionForm            in '..\..\..\..\..\Third Party\Dorset Software Services\DssVcl32\trunk\ExceptionForm.pas',
  FolderBrowser            in '..\..\..\..\..\Third Party\Dorset Software Services\DssVcl32\trunk\FolderBrowser.pas',
  GeneralFunctions         in '..\..\..\..\..\Third Party\Dorset Software Services\DssVcl32\trunk\GeneralFunctions.pas',
  HotLabel                 in '..\..\..\..\..\Third Party\Dorset Software Services\DssVcl32\trunk\HotLabel.pas',
  ListDlls                 in '..\..\..\..\..\Third Party\Dorset Software Services\DssVcl32\trunk\ListDlls.pas',
  OLETools                 in '..\..\..\..\..\Third Party\Dorset Software Services\DssVcl32\trunk\OLETools.pas',
  Relationships_ADO        in '..\..\..\..\..\Third Party\Dorset Software Services\DssVcl32\trunk\Relationships_ADO.pas',
  SQLList                  in '..\..\..\..\..\Third Party\Dorset Software Services\DssVcl32\trunk\SQLList.pas',
  TablePriorityList_ADO    in '..\..\..\..\..\Third Party\Dorset Software Services\DssVcl32\trunk\TablePriorityList_ADO.pas',
  VersionInfo              in '..\..\..\..\..\Third Party\Dorset Software Services\DssVcl32\trunk\VersionInfo.pas',

  XPMenu                   in '..\..\..\..\..\Third Party\Khaled Shagrouni\XP Menu\trunk\XPMenu.pas',

  DatabaseAccessADO        in '..\..\..\..\..\RecorderApp\trunk\Source\DatabaseAccessADO.pas',
  Constants                in '..\..\..\..\..\RecorderApp\trunk\Source\Constants.pas',

  AttachDB                 in '..\..\Common 6\AttachDB.pas',
  BasePage                 in '..\..\Common 6\BasePage.pas' {BasePage: TFrame},
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
  Settings                 in '..\..\Common 6\Settings.pas',
  SetupConstants           in '..\..\Common 6\SetupConstants.pas',
  Shortcuts                in '..\..\Common 6\Shortcuts.pas',
  TextMessages             in '..\..\Common 6\TextMessages.pas',
  TransferQuery            in '..\..\Common 6\TransferQuery.pas',
  RemovalSettings          in '..\..\Common 6\RemovalSettings.pas',
  RemoveCompletePage       in '..\..\Common 6\RemoveCompletePage.pas' {fraRemoveComplete: TFrame},
  RemovePage               in '..\..\Common 6\RemovePage.pas' {fraRemove: TFrame},
  RemoveWelcomePage        in '..\..\Common 6\RemoveWelcomePage.pas' {fraRemoveWelcome: TFrame},

  Logger                   in '..\..\Database Setup 6\Logger.pas',

  CompletionPage           in 'Source\CompletionPage.pas' {fraCompletion: TFrame},
  DatabaseServerSelectPage in 'Source\DatabaseServerSelectPage.pas' {fraDatabaseServerSelect: TFrame},
  InstallationPage         in 'Source\InstallationPage.pas' {fraInstallation: TFrame},
  InstallFolderPage        in 'Source\InstallFolderPage.pas' {fraInstallFolder: TFrame},
  LoginPage                in 'Source\LoginPage.pas' {fraLogin: TFrame},
  Main                     in 'Source\Main.pas' {frmMain},
  SpatialRefPage           in 'Source\SpatialRefPage.pas' {fraSpatialRef: TFrame},

  DatabaseUtilities        in '..\..\..\..\..\RecorderApp\trunk\Components\DatabaseUtilities.pas',
  DataClasses              in '..\..\..\..\..\RecorderApp\trunk\Components\DataClasses.pas',
  DAOTools                 in '..\..\..\..\..\RecorderApp\trunk\Components\DAOTools.pas',
  DAO_TLB                  in '..\..\..\..\..\RecorderApp\trunk\Components\DAO_TLB.pas',
  GenFuncs                 in '..\..\..\..\..\RecorderApp\trunk\Components\Genfuncs.pas',
  JNCCDatasets             in '..\..\..\..\..\RecorderApp\trunk\Components\JNCCDatasets.pas',
  JNCCRelationships        in '..\..\..\..\..\RecorderApp\trunk\Components\JNCCRelationships.pas',
  SpatialRefFuncs          in '..\..\..\..\..\RecorderApp\trunk\Components\SpatialRefFuncs.pas',
  SQLConverter             in '..\..\..\..\..\RecorderApp\trunk\Components\SQLConverter.pas',
  VagueDate                in '..\..\..\..\..\RecorderApp\trunk\Components\VagueDate.pas';

{$R WorkstationSetup.KLR}

{$R *.res}
{$R WksSetup.res}

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
  Application.Title := 'Recorder 6 Workstation Setup';
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
