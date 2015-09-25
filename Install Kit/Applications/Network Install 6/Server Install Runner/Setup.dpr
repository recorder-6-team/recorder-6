program Setup;

{$R 'setup.manifest.res' 'setup.manifest.rc'}

uses
  Forms,
  EasyShell;

{$E .ex_}

{$R *.res}

begin
  Application.Initialize;
  ShellFile('SetupApp.exe');
end.
