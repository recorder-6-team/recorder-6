library ManageDesignationSets;

uses
  ComServ,
  ManageDesignationSets_TLB in 'ManageDesignationSets_TLB.pas',
  ManageDesignationSetsImpl1 in 'ManageDesignationSetsImpl1.pas' {ManageDesignationSetsX: TActiveForm} {ManageDesignationSetsX: CoClass},
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
