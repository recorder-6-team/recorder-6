library ShowUnchecked;

uses
  ComServ,
  ShowUnchecked_TLB in 'ShowUnchecked_TLB.pas',
  DialogShowUncheckedImpl in 'DialogShowUncheckedImpl.pas' {DialogShowUnchecked: TActiveForm} {DialogShowUnchecked: CoClass};

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
