program DatabaseSetup;

{$R 'database.manifest.res' 'database.manifest.rc'}

uses
  madExcept,
  madListHardware,
  madListProcesses,
  madListModules,
  Forms, LocOnFly, Sysutils,

  ADOX_TLB          in '..\..\..\Third Party\Dorset Software Services\DssVcl32\ADOX_TLB.pas',
  ADODB_TLB         in '..\..\..\Third Party\Dorset Software Services\DssVcl32\ADODB_TLB.pas',
  ApiUtils          in '..\..\..\Third Party\Dorset Software Services\DssVcl32\ApiUtils.pas',
  BaseADODataModule in '..\..\..\Third Party\Dorset Software Services\DssVcl32\BaseADODataModule.pas' {dmBaseADO: TDataModule},
  DisplayFuncs      in '..\..\..\Third Party\Dorset Software Services\DssVcl32\DisplayFuncs.pas',
  ExceptionForm     in '..\..\..\Third Party\Dorset Software Services\DssVcl32\ExceptionForm.pas',
  GeneralFunctions  in '..\..\..\Third Party\Dorset Software Services\DssVcl32\GeneralFunctions.pas',
  ListDlls          in '..\..\..\Third Party\Dorset Software Services\DssVcl32\ListDlls.pas',
  Relationships_ADO in '..\..\..\Third Party\Dorset Software Services\DssVcl32\Relationships_ADO.pas',
  TablePriorityList_ADO
                    in '..\..\..\Third Party\Dorset Software Services\DssVcl32\TablePriorityList_ADO.pas',
  VersionInfo       in '..\..\..\Third Party\Dorset Software Services\DssVcl32\VersionInfo.pas',
  XPMenu            in '..\..\..\Third Party\Khaled Shagrouni\XP Menu\XPMenu.pas',

  DAOTools          in '..\..\..\RecorderApp\Components\DAOTools.pas',
  Recorder2000_TLB  in '..\..\..\RecorderApp\Recorder2000_TLB.pas',
  DatabaseAccessADO in '..\..\..\RecorderApp\Source\DatabaseAccessADO.pas' {dmDatabase: TDataModule},
  Constants         in '..\..\..\RecorderApp\Source\Constants.pas' {dmDatabase: TDataModule},

  AttachDB          in '..\Common 6\AttachDB.pas',
  DBMigrator        in '..\Common 6\DBMigrator.pas',
  Functions         in '..\Common 6\Functions.pas',
  PrepareMDB        in '..\Common 6\PrepareMDB.pas',
  SetupConstants    in '..\Common 6\SetupConstants.pas',
  TextMessages      in '..\Common 6\TextMessages.pas',
  TransferQuery     in '..\Common 6\TransferQuery.pas',

  Main              in 'Main.pas' {frmMain},
  Logger            in 'Logger.pas',

  DatabaseUtilities in '..\..\..\RecorderApp\Components\DatabaseUtilities.pas',
  DataClasses       in '..\..\..\RecorderApp\Components\DataClasses.pas',
  DAO_TLB           in '..\..\..\RecorderApp\Components\DAO_TLB.pas',
  GenFuncs          in '..\..\..\RecorderApp\Components\Genfuncs.pas',
  JNCCDatasets      in '..\..\..\RecorderApp\Components\JNCCDatasets.pas',
  JNCCRelationships in '..\..\..\RecorderApp\Components\JNCCRelationships.pas',
  SpatialRefFuncs   in '..\..\..\RecorderApp\Components\SpatialRefFuncs.pas',
  SQLConverter      in '..\..\..\RecorderApp\Components\SQLConverter.pas',
  VagueDate         in '..\..\..\RecorderApp\Components\VagueDate.pas';

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
