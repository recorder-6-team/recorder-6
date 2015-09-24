program DatabaseSetup;

{$R 'database.manifest.res' 'database.manifest.rc'}

uses
  madExcept,
  madListHardware,
  madListProcesses,
  madListModules,
  Forms, LocOnFly, Sysutils,

  ADOX_TLB          in '..\..\..\..\Third Party\Dorset Software Services\DssVcl32\trunk\ADOX_TLB.pas',
  ADODB_TLB         in '..\..\..\..\Third Party\Dorset Software Services\DssVcl32\trunk\ADODB_TLB.pas',
  ApiUtils          in '..\..\..\..\Third Party\Dorset Software Services\DssVcl32\trunk\ApiUtils.pas',
  BaseADODataModule in '..\..\..\..\Third Party\Dorset Software Services\DssVcl32\trunk\BaseADODataModule.pas' {dmBaseADO: TDataModule},
  DisplayFuncs      in '..\..\..\..\Third Party\Dorset Software Services\DssVcl32\trunk\DisplayFuncs.pas',
  ExceptionForm     in '..\..\..\..\Third Party\Dorset Software Services\DssVcl32\trunk\ExceptionForm.pas',
  GeneralFunctions  in '..\..\..\..\Third Party\Dorset Software Services\DssVcl32\trunk\GeneralFunctions.pas',
  ListDlls          in '..\..\..\..\Third Party\Dorset Software Services\DssVcl32\trunk\ListDlls.pas',
  Relationships_ADO in '..\..\..\..\Third Party\Dorset Software Services\DssVcl32\trunk\Relationships_ADO.pas',
  TablePriorityList_ADO
                    in '..\..\..\..\Third Party\Dorset Software Services\DssVcl32\trunk\TablePriorityList_ADO.pas',
  VersionInfo       in '..\..\..\..\Third Party\Dorset Software Services\DssVcl32\trunk\VersionInfo.pas',
  XPMenu            in '..\..\..\..\Third Party\Khaled Shagrouni\XP Menu\trunk\XPMenu.pas',

  DAOTools          in '..\..\..\..\RecorderApp\trunk\Components\DAOTools.pas',
  Recorder2000_TLB  in '..\..\..\..\RecorderApp\trunk\Recorder2000_TLB.pas',
  DatabaseAccessADO in '..\..\..\..\RecorderApp\trunk\Source\DatabaseAccessADO.pas' {dmDatabase: TDataModule},
  Constants         in '..\..\..\..\RecorderApp\trunk\Source\Constants.pas' {dmDatabase: TDataModule},

  AttachDB          in '..\Common 6\AttachDB.pas',
  DBMigrator        in '..\Common 6\DBMigrator.pas',
  Functions         in '..\Common 6\Functions.pas',
  PrepareMDB        in '..\Common 6\PrepareMDB.pas',
  SetupConstants    in '..\Common 6\SetupConstants.pas',
  TextMessages      in '..\Common 6\TextMessages.pas',
  TransferQuery     in '..\Common 6\TransferQuery.pas',

  Main              in 'Main.pas' {frmMain},
  Logger            in 'Logger.pas',

  DatabaseUtilities in '..\..\..\..\RecorderApp\trunk\Components\DatabaseUtilities.pas',
  DataClasses       in '..\..\..\..\RecorderApp\trunk\Components\DataClasses.pas',
  DAO_TLB           in '..\..\..\..\RecorderApp\trunk\Components\DAO_TLB.pas',
  GenFuncs          in '..\..\..\..\RecorderApp\trunk\Components\Genfuncs.pas',
  JNCCDatasets      in '..\..\..\..\RecorderApp\trunk\Components\JNCCDatasets.pas',
  JNCCRelationships in '..\..\..\..\RecorderApp\trunk\Components\JNCCRelationships.pas',
  SpatialRefFuncs   in '..\..\..\..\RecorderApp\trunk\Components\SpatialRefFuncs.pas',
  SQLConverter      in '..\..\..\..\RecorderApp\trunk\Components\SQLConverter.pas',
  VagueDate         in '..\..\..\..\RecorderApp\trunk\Components\VagueDate.pas';

{$R DatabaseSetup.KLR}

{$R *.res}

begin
  LocalizerOnFly.Init;
  {$IFDEF madExcept}
  RegisterExceptionHandler(MadExceptionHandler, stTrySyncCallOnSuccess);
  {$ENDIF}  
  // try to find exact match on language & sub language e.g. French (Luxembourg)
  if LocalizerOnFly.LangList.IndexByLocale(Sysutils.SysLocale.DefaultLCID) > -1 then
    LocalizerOnFly.SwitchTo(Sysutils.SysLocale.DefaultLCID)
  // else try to find a match on generic language e.g. French
  else if LocalizerOnFly.LangList.IndexByLocale(Sysutils.SysLocale.PriLangID) > -1 then
    LocalizerOnFly.SwitchTo(Sysutils.SysLocale.PriLangID)
  else
    LocalizerOnFly.SwitchTo(LocalizerOnFly.NativeLocale);
  Application.Initialize;
  Application.Title := 'Recorder Database Migration';
  Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(TdmDatabase, dmDatabase);
  Application.Run;
end.
