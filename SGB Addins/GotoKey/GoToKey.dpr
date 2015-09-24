library GoToKey;

uses
  ComServ,
  GoToKey_TLB in 'GoToKey_TLB.pas',
  GoToKeyImpl in 'GoToKeyImpl.pas' {GoToKeyX: TActiveForm} {GoToKeyX: CoClass};

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
