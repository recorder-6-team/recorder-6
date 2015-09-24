library ProjectRuck3;

uses
  ComServ,
  ProjectRuck3_TLB in 'ProjectRuck3_TLB.pas',
  Project3RuckForm1Impl1 in 'Project3RuckForm1Impl1.pas' {Project3RuckForm1: TActiveForm} {Project3RuckForm1: CoClass},
  ADODB_TLB in '..\Imports\ADODB_TLB.pas',
  Recorder2000_TLB in '..\..\Imports\Recorder2000_TLB.pas';

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
