library ObsAdditionalInfo;

uses
  ComServ,
  ObsAdditionalInfo_TLB in 'ObsAdditionalInfo_TLB.pas',
  ObsAddInfoButtonImpl1 in 'ObsAddInfoButtonImpl1.pas' {ObsAddInfoButton: CoClass},
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
