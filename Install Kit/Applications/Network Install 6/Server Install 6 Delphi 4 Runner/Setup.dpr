program Setup;

{$R 'setup.manifest.res' 'setup.manifest.rc'}

uses
  Forms,
  Windows,
  Dialogs,
  Sysutils,
  GeneralFunctions;

{$R *.RES}

begin
  Application.Initialize;
  if GetOSName = 'Windows 95' then begin
    MessageDlg('The Windows 95 operating system is not supported. ' +
               'Recorder 6 requires a minimum of Windows 98 with Y2K Update Pack 2.',
               mtInformation, [mbOk], 0);
    Exit;
  end;
  WinExec(PChar(ExtractFilePath(Application.Exename) + 'Setup.ex_'), SW_SHOWDEFAULT);
end.
