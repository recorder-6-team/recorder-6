library Indicia2Recorder;

uses
  madExcept,
  LocOnFly,
  madLinkDisAsm,
  madListHardware,
  madListProcesses,
  madListModules,
  ComServ,
  Recorder2000_TLB in '..\..\Recorder2000_TLB.pas',
  Indicia2Recorder_TLB in 'Indicia2Recorder_TLB.pas',
  DownloadDialogImpl in 'DownloadDialogImpl.pas' {DownloadDialog: TActiveForm} {DownloadDialog: CoClass},
  ADODB_TLB in '..\..\ADODB_TLB.pas',
  uLkJSON in 'uLkJSON.pas',
  ExceptionForm in '..\..\..\Third Party\Dorset Software Services\DssVcl32\ExceptionForm.pas',
  VersionInfo in '..\..\..\Third Party\Dorset Software Services\DssVcl32\VersionInfo.pas',
  GeneralFunctions in '..\..\..\Third Party\Dorset Software Services\DssVcl32\GeneralFunctions.pas',
  ListDlls in '..\..\..\Third Party\Dorset Software Services\DssVcl32\ListDlls.pas',
  ApiUtils in '..\..\..\Third Party\Dorset Software Services\DssVcl32\ApiUtils.pas';

{$R Indicia2Recorder.KLR}

{$E ocx}

exports
  DllGetClassObject,
  DllCanUnloadNow,
  DllRegisterServer,
  DllUnregisterServer;

{$R *.tlb}

{$R *.RES}

begin
end.
