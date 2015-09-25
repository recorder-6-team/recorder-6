library VCchecker;

uses
  ComServ,
  VCchecker_TLB in 'VCchecker_TLB.pas',
  VC_Grid_CheckerImpl in 'VC_Grid_CheckerImpl.pas' {VC_Grid_CheckerX: TActiveForm} {VC_Grid_CheckerX: CoClass},
  ItemsList in 'ItemsList.pas',
  VC_check in 'VC_check.pas',
  gpc in 'gpc.pas',
  GridRefs in 'GridRefs.pas',
  SampleAdminArea in 'SampleAdminArea.pas';

{$E ocx}

exports
  DllGetClassObject,
  DllCanUnloadNow,
  DllRegisterServer,
  DllUnregisterServer;

{$R *.TLB}

{$R *.RES}

begin
{$WARN SYMBOL_PLATFORM OFF}
  ReportMemoryLeaksOnShutdown := DebugHook <> 0;
end.
