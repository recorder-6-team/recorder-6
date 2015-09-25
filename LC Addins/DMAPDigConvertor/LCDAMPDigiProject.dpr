library LCDAMPDigiProject;

uses
  ComServ,
  LCDAMPDigiProject_TLB in 'LCDAMPDigiProject_TLB.pas',
  LCDMAPDigiForm1Impl1 in 'LCDMAPDigiForm1Impl1.pas' {LCDMAPDigiForm1: TActiveForm} {LCDMAPDigiForm1: CoClass},
  Recorder2000_TLB in '..\Imports\Recorder2000_TLB.pas';

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
