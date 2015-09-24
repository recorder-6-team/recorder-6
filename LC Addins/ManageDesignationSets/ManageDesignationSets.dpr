library ManageDesignationSets;

uses
  ComServ,
  ManageDesignationSets_TLB in 'ManageDesignationSets_TLB.pas',
  ManageDesignationSetsImpl1 in 'ManageDesignationSetsImpl1.pas' {ManageDesignationSetsX: TActiveForm} {ManageDesignationSetsX: CoClass},
  Recorder2000_TLB in '..\..\Imports\Recorder2000_TLB.pas',
  ADODB_TLB in '..\..\Imports\ADODB_TLB.pas';

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
