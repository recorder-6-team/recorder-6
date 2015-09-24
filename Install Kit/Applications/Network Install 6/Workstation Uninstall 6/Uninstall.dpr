{-------------------------------------------------------------------------------
  Program:     Uninstaller.dpr

  Description: Recorder workstation uninstall wizard.

  Created:     November 2004

  Last revision information:
    $Revision: $
    $Date: $
    $Author: $

-------------------------------------------------------------------------------}

program Uninstall;

{$R 'AdditionalResources.res' 'AdditionalResources.rc'}

uses
  Forms, LocOnFly,
  InstallAddins        in '..\..\Common 6\InstallAddins.pas',
  SetupConstants       in '..\..\Common 6\SetupConstants.pas',
  Shortcuts            in '..\..\Common 6\Shortcuts.pas',
  TextMessages         in '..\..\Common 6\TextMessages.pas',
  FrameBase            in '..\..\Common 6\FrameBase.pas' {PageFrame: TFrame},
  Settings             in '..\..\Common 6\Settings.pas',

  FrameUninstall       in 'Source\FrameUninstall.pas' {fraUninstall: TFrame},
  FrameUninstallSelect in 'Source\FrameUninstallSelect.pas' {fraUninstallSelect: TFrame},
  FrameWelcome         in 'Source\FrameWelcome.pas' {fraWelcome: TFrame},
  Main                 in 'Source\Main.pas' {frmMain};

{$R Uninstall.KLR}

{$E ex_}

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Recorder 6 Workstation Uninstall';
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
