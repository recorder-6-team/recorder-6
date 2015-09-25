program DictionaryUpgrade;

{$R 'AdditionalResources.res' 'AdditionalResources.rc'}

uses
  madExcept,
  madListHardware,
  madListProcesses,
  madListModules,
  Forms, LocOnFly,
  Windows,
  Dialogs,
  SysUtils,
  Messages,
  BaseADODataModule    in '..\..\..\Third Party\Dorset Software Services\DssVcl32\Trunk\BaseADODataModule.pas' {dmBaseADO: TDataModule},
  Recorder2000_TLB     in '..\..\..\RecorderApp\trunk\Recorder2000_TLB.pas',
  ADODB_TLB            in '..\..\..\Third Party\Dorset Software Services\DssVcl32\trunk\ADODB_TLB.pas',
  ADOX_TLB             in '..\..\..\Third Party\Dorset Software Services\DssVcl32\trunk\ADOX_TLB.pas',
  ApiUtils             in '..\..\..\Third Party\Dorset Software Services\DssVcl32\trunk\ApiUtils.pas',
  DisplayFuncs         in '..\..\..\Third Party\Dorset Software Services\DssVcl32\trunk\DisplayFuncs.pas',
  ExceptionForm        in '..\..\..\Third Party\Dorset Software Services\DssVcl32\Trunk\ExceptionForm.pas',
  GeneralFunctions     in '..\..\..\Third Party\Dorset Software Services\DssVcl32\Trunk\GeneralFunctions.pas',
  HotLabel             in '..\..\..\Third Party\Dorset Software Services\DssVcl32\trunk\HotLabel.pas',
  ListDlls             in '..\..\..\Third Party\Dorset Software Services\DssVcl32\trunk\ListDlls.pas',
  Relationships_ADO    in '..\..\..\Third Party\Dorset Software Services\DssVcl32\trunk\Relationships_ADO.pas',
  TablePriorityList_ADO in '..\..\..\Third Party\Dorset Software Services\DssVcl32\trunk\TablePriorityList_ADO.pas',
  TaskProgress         in '..\..\..\Third Party\Dorset Software Services\DssVcl32\trunk\TaskProgress.pas',
  VersionInfo         in '..\..\..\Third Party\Dorset Software Services\DssVcl32\trunk\VersionInfo.pas',
  VersionInfo2         in '..\..\..\Third Party\Dorset Software Services\DssVcl32\trunk\VersionInfo2.pas',

  XPMenu               in '..\..\..\Third Party\Khaled Shagrouni\XP Menu\trunk\XPMenu.pas',

  BaseFrameUnit        in '..\Common\BaseFrameUnit.pas' {BaseFrame: TFrame},
  DBUpgrader           in '..\Common\DBUpgrader.pas',
  FileUpgrader         in '..\Common\FileUpgrader.pas',
  LoginFrame           in '..\Common\LoginFrame.pas' {fraLogin: TFrame},
  Main                 in '..\Common\Main.pas' {frmMain},
  MissingDBScripts     in '..\Common\MissingDBScripts.pas' {dlgMissingScripts},
  Settings             in '..\Common\Settings.pas',
  UpdateDescriptions   in '..\Common\UpdateDescriptions.pas' {frmUpdateDescriptions},
  UpgradeFrame         in '..\Common\UpgradeFrame.pas' {fraUpgrade: TFrame},
  RecorderLock         in '..\Common\RecorderLock.pas',

  Functions            in '..\..\..\Install Kit\trunk\Applications\Common 6\Functions.pas',
  PrepareMDB           in '..\..\..\Install Kit\trunk\Applications\Common 6\PrepareMDB.pas',
  SetupConstants       in '..\..\..\Install Kit\trunk\Applications\Common 6\SetupConstants.pas',
  TextMessages         in '..\..\..\Install Kit\trunk\Applications\Common 6\TextMessages.pas',

  DatabaseAccessADO    in '..\..\..\RecorderApp\trunk\Source\DatabaseAccessADO.pas',
  DBMerger             in '..\..\..\RecorderApp\trunk\Source\DBMerger.pas',
  DefaultPaths         in '..\..\..\RecorderApp\trunk\Source\DefaultPaths.pas',
  Constants            in '..\..\..\RecorderApp\trunk\Source\Constants.pas',
  SQLConstants         in '..\..\..\RecorderApp\trunk\Source\SQLConstants.pas',

  DataClasses          in '..\..\..\RecorderApp\trunk\Components\DataClasses.pas',
  JNCCDatasets         in '..\..\..\RecorderApp\trunk\Components\JNCCDatasets.pas',
  VagueDate            in '..\..\..\RecorderApp\trunk\Components\VagueDate.pas',
  GenFuncs             in '..\..\..\RecorderApp\trunk\Components\Genfuncs.pas',
  SpatialRefFuncs      in '..\..\..\RecorderApp\trunk\Components\SpatialRefFuncs.pas',
  SQLConverter         in '..\..\..\RecorderApp\trunk\Components\SQLConverter.pas',
  DatabaseUtilities    in '..\..\..\RecorderApp\trunk\Components\DatabaseUtilities.pas',
  JNCCRelationships    in '..\..\..\RecorderApp\trunk\Components\JNCCRelationships.pas';

{$R DictionaryUpgrade.KLR}

{$R *.res}

resourcestring
  ResStr_IntroMessage = 'The Dictionary Upgrade Application will now upgrade your database dictionary '+
      'data to the latest version.';

var
  MutHandle: THandle = 0;
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
  MutHandle := OpenMutex(MUTEX_ALL_ACCESS, False, 'Recorder 2000');
  // If not running, all fine, else exit
  if MutHandle = 0 then
    MutHandle := CreateMutex(nil, True, 'Recorder 2000')
  else begin
    MessageDlg(ResStr_AlreadyRunningOnThisMachine, mtWarning, [mbOk], 0);
    if ParamCount >= 1 then
      PostMessage(StrToInt64(Copy(ParamStr(1), 2, 255)), WM_CLOSE, 0, 0);
    Exit;
  end;

  Application.Initialize;
  // This line is used by Delphi itself to set the title in the Project's Options.
  // Can't use resource string for it in here. It's done in SetApplicationTitle further down.
  Application.Title := 'Recorder Dictionary Upgrade';
  Application.CreateForm(TfrmMain, frmMain);
  frmMain.Settings.SequenceSettingName := 'Dict Seq';
  frmMain.Settings.ScriptsOnly         := True;
  frmMain.Settings.DatabaseDescription := 'dictionaries';
  frmMain.Settings.IntroMessage := ResStr_IntroMessage;
  frmMain.SetApplicationTitle(ResStr_DictionaryUpgrade);  
  frmMain.EmbedCurrentFrame;
  Application.Run;
end.
