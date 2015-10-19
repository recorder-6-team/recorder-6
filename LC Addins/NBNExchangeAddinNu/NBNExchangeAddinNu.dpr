library NBNExchangeAddinNu;


uses
  ComServ,
  NBNExchangeAddinNu_TLB in 'NBNExchangeAddinNu_TLB.pas',
  NBNExchangeAddinNuImp1 in 'NBNExchangeAddinNuImp1.pas' {NBNExchangeAddinNuImp: CoClass},
  NBNTransferOptions in 'NBNTransferOptions.pas' {formOptions},
  GenFuncs in 'GenFuncs.pas',
  VersionInfo in 'VersionInfo.pas',
  VagueDate in 'VagueDate.pas',
  login in 'login.pas' {formLogin},
  ADODB_TLB in '..\..\Third Party\Imports\ADODB_TLB.pas',
  Recorder2000_TLB in '..\..\Third Party\Imports\Recorder2000_TLB.pas',
  Abbrevia_TLB in '..\..\Third Party\Abbrevia\Abbrevia_TLB.pas';

exports
  DllGetClassObject,
  DllCanUnloadNow,
  DllRegisterServer,
  DllUnregisterServer;

{$R *.TLB}

{$R *.RES}

begin
end.
