library ProjectTaxonMetaButton;

uses
  ComServ,
  ProjectTaxonMetaButton_TLB in 'ProjectTaxonMetaButton_TLB.pas',
  TaxonButtonImpl1 in 'TaxonButtonImpl1.pas' {TaxonButton: CoClass},
  Recorder2000_TLB in '..\Imports\Recorder2000_TLB.pas',
  ADODB_TLB in '..\Imports\ADODB_TLB.pas';

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
