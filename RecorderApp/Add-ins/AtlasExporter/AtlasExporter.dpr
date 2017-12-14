library AtlasExporter;

uses
  ComServ,
  AtlasExporter_TLB in 'AtlasExporter_TLB.pas',
  ReportButtonImpl in 'Translatable\ReportButtonImpl.pas' {ReportButton: TActiveForm} {ReportButton: CoClass},
  Recorder2000_TLB in '..\..\..\..\..\Localsource\recorder-6\RecorderApp\Recorder2000_TLB.pas',
  ADODB_TLB in '..\..\..\..\..\Localsource\recorder-6\Third Party\Dorset Software Services\DssVcl32\ADODB_TLB.pas',
  ExportWizard in 'Translatable\ExportWizard.pas' {dlgExportWizard};

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
