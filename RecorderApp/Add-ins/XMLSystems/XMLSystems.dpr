library XMLSystems;

uses
  ComServ,
  Recorder2000_TLB in '..\..\Recorder2000_TLB.pas',
  XMLSystems_TLB in 'XMLSystems_TLB.pas',
  GenFuncs in '..\..\Components\Genfuncs.pas',
  geodll32 in 'Source\geodll32.pas',
  XMLSystemsImpl in 'Source\XMLSystemsImpl.pas',
  SpatialRefFuncs in '..\..\Components\SpatialRefFuncs.pas',
  ADODB_TLB in '..\..\..\Third Party\Dorset Software Services\DssVcl32\ADODB_TLB.pas',
  ExceptionForm in '..\..\..\Third Party\Dorset Software Services\DssVcl32\\ExceptionForm.pas' {frmException},
  VersionInfo in '..\..\..\Third Party\Dorset Software Services\DssVcl32\\VersionInfo.pas',
  GeneralFunctions in '..\..\..\Third Party\Dorset Software Services\DssVcl32\\GeneralFunctions.pas',
  ListDlls in '..\..\..\Third Party\Dorset Software Services\DssVcl32\ListDlls.pas',
  ApiUtils in '..\..\..\Third Party\Dorset Software Services\DssVcl32\ApiUtils.pas',
  MSScriptControl_TLB in 'Components\MSScriptControl_TLB.pas';

exports
  DllGetClassObject,
  DllCanUnloadNow,
  DllRegisterServer,
  DllUnregisterServer;

{$R *.TLB}

{$R *.RES}

begin
end.
