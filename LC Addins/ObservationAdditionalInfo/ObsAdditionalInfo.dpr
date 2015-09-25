library ObsAdditionalInfo;

uses
  ComServ,
  ObsAdditionalInfo_TLB in 'ObsAdditionalInfo_TLB.pas',
  ADODB_TLB in '..\..\Imports\ADODB_TLB.pas',
  ObsAddInfoButtonImpl1 in 'ObsAddInfoButtonImpl1.pas' {ObsAddInfoButton: CoClass},
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
