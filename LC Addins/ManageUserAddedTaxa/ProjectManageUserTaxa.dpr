library ProjectManageUserTaxa;

uses
  ComServ,
  ProjectManageUserTaxa_TLB in 'ProjectManageUserTaxa_TLB.pas',
  ManageUDTImpl1 in 'ManageUDTImpl1.pas' {ManageUDT: TActiveForm} {ManageUDT: CoClass},
  Recorder2000_TLB in '..\Imports\Recorder2000_TLB.pas',
  ADODB_TLB in '..\Imports\ADODB_TLB.pas';

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
