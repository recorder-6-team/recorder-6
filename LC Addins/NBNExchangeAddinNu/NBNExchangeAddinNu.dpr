library NBNExchangeAddinNu;


uses
  ComServ,
  Recorder2000_TLB in '..\..\Imports\Recorder2000_TLB.pas',
  NBNExchangeAddinNu_TLB in 'NBNExchangeAddinNu_TLB.pas',
  NBNExchangeAddinNuImp1 in 'NBNExchangeAddinNuImp1.pas' {NBNExchangeAddinNuImp: CoClass},
  ADODB_TLB in '..\..\Imports\ADODB_TLB.pas',
  NBNTransferOptions in 'NBNTransferOptions.pas' {formOptions},
  GenFuncs in 'GenFuncs.pas',
  VersionInfo in 'VersionInfo.pas',
  VagueDate in 'VagueDate.pas',
  login in 'login.pas' {formLogin},
  Abbrevia_TLB in '..\..\Imports\Abbrevia_TLB.pas';

exports
  DllGetClassObject,
  DllCanUnloadNow,
  DllRegisterServer,
  DllUnregisterServer;

{$R *.TLB}

{$R *.RES}

begin
end.
