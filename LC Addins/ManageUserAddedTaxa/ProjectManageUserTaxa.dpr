library ProjectManageUserTaxa;

uses
  ComServ,
  ProjectManageUserTaxa_TLB in 'ProjectManageUserTaxa_TLB.pas',
  ManageUDTImpl1 in 'ManageUDTImpl1.pas' {ManageUDT: TActiveForm} {ManageUDT: CoClass},
  ADODB_TLB in '..\..\Third Party\Imports\ADODB_TLB.pas',
  Recorder2000_TLB in '..\..\Third Party\Imports\Recorder2000_TLB.pas';

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
