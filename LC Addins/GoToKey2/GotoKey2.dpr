library GotoKey2;

uses
  ComServ,
  Recorder2000_TLB in '..\..\Imports\Recorder2000_TLB.pas',
  ADODB_TLB in '..\..\Imports\ADODB_TLB.pas',
  GotoKey2_TLB in 'GotoKey2_TLB.pas',
  GotoKey2Impl1 in 'GotoKey2Impl1.pas' {GotoKey2X: TActiveForm} {GotoKey2X: CoClass};

exports
  DllGetClassObject,
  DllCanUnloadNow,
  DllRegisterServer,
  DllUnregisterServer;

{$R *.TLB}

{$R *.RES}

begin
end.
