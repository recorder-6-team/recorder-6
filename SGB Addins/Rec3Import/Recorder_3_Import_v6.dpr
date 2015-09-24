library Recorder_3_Import_v6;

uses
  ComServ,
  Recorder_3_Import_v6_TLB in 'Recorder_3_Import_v6_TLB.pas',
  Rec3Importer_v6 in 'Rec3Importer_v6.pas' {Rec3Importer: CoClass},
  ImportProgressImpl_v6 in 'ImportProgressImpl_v6.pas' {ImportProgressX: TActiveForm} {ImportProgressX: CoClass};

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
