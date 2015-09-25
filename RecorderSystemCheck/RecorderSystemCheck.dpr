program RecorderSystemCheck;

uses
  Forms,
  main in 'main.pas' {frmMain},
  Recorder2000_TLB in '..\..\RecorderApp\trunk\Recorder2000_TLB.pas',
  ADODB_TLB in '..\..\Third Party\Dorset Software Services\DssVcl32\trunk\ADODB_TLB.pas',
  ApiUtils in '..\..\Third Party\Dorset Software Services\DssVcl32\trunk\ApiUtils.pas';

{$R *.res}
{$R 'RecorderSystemCheck.manifest.res'}


begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
