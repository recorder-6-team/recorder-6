library SurveyDelete;

uses
  ComServ,
  SurveyDelete_TLB in 'SurveyDelete_TLB.pas',
  SurveyDeleteImpl in 'SurveyDeleteImpl.pas' {SurveyDeleteX: TActiveForm} {SurveyDeleteX: CoClass};

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
