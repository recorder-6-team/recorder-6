library BulkLoad;
// Version for Recorder 6.3 March 2004
uses
  ComServ,
  BulkLoad_TLB in 'BulkLoad_TLB.pas',
  BulkLoadImpl in 'BulkLoadImpl.pas' {formBulkLoadX: TActiveForm} {BulkLoadX: CoClass},
  Recorder2000_TLB in '..\..\RecorderApp\trunk\Recorder2000_TLB.pas',
  ADODB_TLB in '..\..\Third Party\Dorset Software Services\DssVcl32\trunk\ADODB_TLB.pas',
  VersionInfo in '..\..\Third Party\Dorset Software Services\DssVcl32\trunk\VersionInfo.pas',
  BrowseForFolderU in '..\Common\BrowseForFolderU.pas';

{$E ocx}

exports
  DllGetClassObject,
  DllCanUnloadNow,
  DllRegisterServer,
  DllUnregisterServer;

{$R *.TLB}

{$R *.RES}

begin
end.
