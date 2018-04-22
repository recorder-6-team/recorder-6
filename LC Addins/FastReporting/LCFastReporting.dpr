library LCFastReporting;

uses
  madExcept,
  madLinkDisAsm,
  madListHardware,
  madListProcesses,
  madListModules,
  ComServ,
  LCFastReporting_TLB in 'LCFastReporting_TLB.pas',
  FastReportingXImpl1 in 'FastReportingXImpl1.pas' {FastReportingX: TActiveForm} {FastReportingX: CoClass},
  Recorder2000_TLB in '..\..\Third Party\Imports\Recorder2000_TLB.pas',
  ADODB_TLB in '..\..\Third Party\Imports\ADODB_TLB.pas';


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
