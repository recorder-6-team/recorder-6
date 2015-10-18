library ProjectTaxonMetaButton;

uses
  ComServ,
  ProjectTaxonMetaButton_TLB in 'ProjectTaxonMetaButton_TLB.pas',
  TaxonButtonImpl1 in 'TaxonButtonImpl1.pas' {TaxonButton: CoClass},
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
