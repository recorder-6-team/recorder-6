program LicenceKeyGeneratorProject;

uses
  Forms,
  LicenceKeyGenerator in 'LicenceKeyGenerator.pas' {LicenceKey};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TLicenceKey, LicenceKey);
  Application.Run;
end.
