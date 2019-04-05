program RecorderUpgrade;

{$R 'AdditionalResources.res' 'AdditionalResources.rc'}

uses
  madExcept,
  madListHardware,
  madListProcesses,
  madListModules,
  Forms,
  LocOnFly,
  Windows,
  Dialogs,
  SysUtils,
  Messages,
  BaseADODataModule in '..\..\Third Party\Dorset Software Services\DssVcl32\BaseADODataModule.pas' {dmBaseADO: TDataModule},
  Recorder2000_TLB in '..\..\RecorderApp\Recorder2000_TLB.pas',
  ADODB_TLB in '..\..\Third Party\Dorset Software Services\DssVcl32\ADODB_TLB.pas',
  ADOX_TLB in '..\..\Third Party\Dorset Software Services\DssVcl32\ADOX_TLB.pas',
  ApiUtils in '..\..\Third Party\Dorset Software Services\DssVcl32\ApiUtils.pas',
  DisplayFuncs in '..\..\Third Party\Dorset Software Services\DssVcl32\DisplayFuncs.pas',
  ExceptionForm in '..\..\Third Party\Dorset Software Services\DssVcl32\ExceptionForm.pas',
  GeneralFunctions in '..\..\Third Party\Dorset Software Services\DssVcl32\GeneralFunctions.pas',
  HotLabel in '..\..\Third Party\Dorset Software Services\DssVcl32\HotLabel.pas',
  ListDlls in '..\..\Third Party\Dorset Software Services\DssVcl32\ListDlls.pas',
  Relationships_ADO in '..\..\Third Party\Dorset Software Services\DssVcl32\Relationships_ADO.pas',
  TablePriorityList_ADO in '..\..\Third Party\Dorset Software Services\DssVcl32\TablePriorityList_ADO.pas',
  TaskProgress in '..\..\Third Party\Dorset Software Services\DssVcl32\TaskProgress.pas',
  VersionInfo in '..\..\Third Party\Dorset Software Services\DssVcl32\VersionInfo.pas',
  VersionInfo2 in '..\..\Third Party\Dorset Software Services\DssVcl32\VersionInfo2.pas',
  XPMenu in '..\..\Third Party\Khaled Shagrouni\XP Menu\XPMenu.pas',
  BaseFrameUnit in '..\Common\BaseFrameUnit.pas' {BaseFrame: TFrame},
  DBUpgrader in '..\Common\DBUpgrader.pas',
  FileUpgrader in '..\Common\FileUpgrader.pas',
  LoginFrame in '..\Common\LoginFrame.pas' {fraLogin: TFrame},
  Main in '..\Common\Main.pas' {frmMain},
  MissingDBScripts in '..\Common\MissingDBScripts.pas' {dlgMissingScripts},
  Settings in '..\Common\Settings.pas',
  UpdateDescriptions in '..\Common\UpdateDescriptions.pas' {frmUpdateDescriptions},
  UpgradeFrame in '..\Common\UpgradeFrame.pas' {fraUpgrade: TFrame},
  RecorderLock in '..\Common\RecorderLock.pas',
  Functions in '..\..\Install Kit\Applications\Common 6\Functions.pas',
  PrepareMDB in '..\..\Install Kit\Applications\Common 6\PrepareMDB.pas',
  SetupConstants in '..\..\Install Kit\Applications\Common 6\SetupConstants.pas',
  TextMessages in '..\..\Install Kit\Applications\Common 6\TextMessages.pas',
  DatabaseAccessADO in '..\..\RecorderApp\Source\DatabaseAccessADO.pas',
  DBMerger in '..\..\RecorderApp\Source\DBMerger.pas',
  Constants in '..\..\RecorderApp\Source\Constants.pas',
  SQLConstants in '..\..\RecorderApp\Source\SQLConstants.pas',
  DefaultPaths in '..\..\RecorderApp\Source\DefaultPaths.pas',
  DataClasses in '..\..\RecorderApp\Components\DataClasses.pas',
  JNCCDatasets in '..\..\RecorderApp\Components\JNCCDatasets.pas',
  VagueDate in '..\..\RecorderApp\Components\VagueDate.pas',
  GenFuncs in '..\..\RecorderApp\Components\Genfuncs.pas',
  SpatialRefFuncs in '..\..\RecorderApp\Components\SpatialRefFuncs.pas',
  SQLConverter in '..\..\RecorderApp\Components\SQLConverter.pas',
  DatabaseUtilities in '..\..\RecorderApp\Components\DatabaseUtilities.pas',
  JNCCRelationships in '..\..\RecorderApp\Components\JNCCRelationships.pas',
  KeyCheckFrame in '..\..\Install Kit\Applications\Standalone 6\Install 6\Source\KeyCheckFrame.pas' {fraKeyCheck: TFrame};

{$R RecorderUpgrade.KLR}

{$R *.res}

resourcestring
  ResStr_IntroMessage = 'The Recorder Application Upgrade will copy any new files required '+
      'into your Recorder installation folders. In addition it will connect to the database and make any changes in order to bring your database up to the latest version.';

begin
  LocalizerOnFly.Init;
  // try to find exact match on language & sub language e.g. French (Luxembourg)
  if LocalizerOnFly.LangList.IndexByLocale(Sysutils.SysLocale.DefaultLCID) > -1 then
    LocalizerOnFly.SwitchTo(Sysutils.SysLocale.DefaultLCID)
  else
  // else try to find a match on generic language e.g. French
  if LocalizerOnFly.LangList.IndexByLocale(Sysutils.SysLocale.PriLangID) > -1 then
    LocalizerOnFly.SwitchTo(Sysutils.SysLocale.PriLangID)
  else
    LocalizerOnFly.SwitchTo(LocalizerOnFly.NativeLocale);
  // Check for previous instance of Recorder 2000
  if not AcquireRecorderLock then
  begin
    MessageDlg(ResStr_AlreadyRunningOnThisMachine, mtWarning, [mbOk], 0);
    if ParamCount >= 1 then
      PostMessage(StrToInt64(Copy(ParamStr(1), 2, 255)), WM_CLOSE, 0, 0);
    Exit;
  end;
  Application.Initialize;
  // This line is used by Delphi itself to set the title in the Project's Options.
  // Can't use resource string for it in here. It's done in SetApplicationTitle further down.
  Application.Title := 'Recorder Application Upgrade';
  Application.CreateForm(TfrmMain, frmMain);
  frmMain.Settings.SequenceSettingName := 'DB Seq';
  frmMain.Settings.ScriptsOnly         := False;
  frmMain.Settings.DatabaseDescription := 'database structure';
  frmMain.Settings.IntroMessage := ResStr_IntroMessage;
  frmMain.SetApplicationTitle(ResStr_RecorderUpgrade);
  frmMain.EmbedCurrentFrame;
  Application.Run;
end.
