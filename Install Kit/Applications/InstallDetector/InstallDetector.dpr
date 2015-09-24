{-------------------------------------------------------------------------------
  Program:     InstallDetector

  Description:

  Author:      John van Breda
  Created:

  Last revision information:
    $Revision: $
    $Date: $
    $Author: $

  Copyright © Dorset Software Services Ltd, 2003

-------------------------------------------------------------------------------}

program InstallDetector;

uses
  Windows,
  Forms,
  Dialogs,
  Controls,
  Sysutils,
  APIUtils,
  GeneralFunctions,
  Main in 'Main.pas' {frmMain},
  InstallationRequired in 'InstallationRequired.pas',
  EasyShell in '..\..\..\..\Third Party\Dorset Software Services\DssVcl32\trunk\EasyShell.pas',
  HotLabel in '..\..\..\..\Third Party\Dorset Software Services\DssVcl32\trunk\HotLabel.pas';

{$R *.RES}

var
  mtfAbort : boolean;

begin
  Application.Initialize;
  mtfAbort := False;
  // some requirements checks
  if GetOSName = 'Windows 95' then begin
    MessageDlg('Recorder cannot run on Windows 95.  Please upgrade your ' +
               'operating system.', mtError, [mbOk], 0);
    mtfAbort := True;
  end
  else if GetOSName = 'Windows 98' then begin
    if MessageDlg('Recorder requires the Year 2000 update for Windows 98 to '+
                  'install.  Please ensure this is installed before proceeding.',
                  mtWarning, [mbOk, mbAbort], 0) = mrAbort then
      mtfAbort := True;
  end;
  if (not mtfAbort) and (GetIEMajorVersion < 5) then begin
    MessageDlg('Recorder requires Internet Explorer 5 or higher to install. ' +
               'Please install this before proceeding.', mtError, [mbOk], 0);
    mtfAbort := True;
  end;

  if not mtfAbort then begin
    if GetInstallationRequired = irUpgrade then begin
      // we need an upgrade, as files are present.
      WinExec32('"' + ExtractFilePath(Application.ExeName) + 'System\InstallPlugin.exe" /upgrade',
                ExtractFilePath(Application.ExeName) + 'System\', SW_SHOWNORMAL);
    end else begin
      Application.Title := 'Recorder 6 Installation';
  Application.CreateForm(TfrmMain, frmMain);
    Application.Run;
    end;
  end;
end.
