program Setup;

{$R 'setup.manifest.res' 'setup.manifest.rc'}

uses
  Forms,
  EasyShell in '..\..\..\..\Third Party\Dorset Software Services\DssVcl32\EasyShell.pas';

{$E .ex_}

{$R *.res}

begin
  Application.Initialize;
  ShellFile('SetupApp.exe');
end.
