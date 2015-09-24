library NBNExchange;

uses
  ComServ,
  NBNExchange_TLB in 'NBNExchange_TLB.pas',
  NBN_Exchange_Impl in 'NBN_Exchange_Impl.pas' {NBNExchangeX: CoClass},
  NBNTransferOptions in 'NBNTransferOptions.pas' {formOptions},
  login in 'login.pas' {formLogin};

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
