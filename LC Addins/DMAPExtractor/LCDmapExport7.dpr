library LCDmapExport7;

uses
  ComServ,
  LCDmapExport7_TLB in 'LCDmapExport7_TLB.pas',
  LcDmap7Unit in 'LcDmap7Unit.pas' {TLcDmap7: CoClass},
  Recorder2000_TLB in '..\..\Third Party\Imports\Recorder2000_TLB.pas';

exports
  DllGetClassObject,
  DllCanUnloadNow,
  DllRegisterServer,
  DllUnregisterServer;

{$R *.TLB}

{$R *.RES}

begin
end.
