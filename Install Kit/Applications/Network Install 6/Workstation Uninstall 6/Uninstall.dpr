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
  Recorder2000_TLB     in '..\..\..\..\RecorderApp\Recorder2000_TLB.pas',
  Functions            in '..\..\Common 6\Functions.pas',
  InstallAddins        in '..\..\Common 6\InstallAddins.pas',
  SetupConstants       in '..\..\Common 6\SetupConstants.pas',
  Shortcuts            in '..\..\Common 6\Shortcuts.pas',
  TextMessages         in '..\..\Common 6\TextMessages.pas',
  FrameBase            in '..\..\Common 6\FrameBase.pas' {PageFrame: TFrame},
  Settings             in '..\..\Common 6\Settings.pas',
  ADODB_TLB            in '..\..\..\..\Third Party\Dorset Software Services\DssVcl32\ADODB_TLB.pas',
  ApiUtils             in '..\..\..\..\Third Party\Dorset Software Services\DssVcl32\ApiUtils.pas',
  ExceptionForm        in '..\..\..\..\Third Party\Dorset Software Services\DssVcl32\ExceptionForm.pas',
  CustomOleCtrls7      in '..\..\..\..\Third Party\Dorset Software Services\DssVcl32\CustomOleCtrls7.pas',
  GeneralFunctions     in '..\..\..\..\Third Party\Dorset Software Services\DssVcl32\GeneralFunctions.pas',
  ListDlls             in '..\..\..\..\Third Party\Dorset Software Services\DssVcl32\ListDlls.pas',
  OleTools             in '..\..\..\..\Third Party\Dorset Software Services\DssVcl32\OleTools.pas',
  SQLList              in '..\..\..\..\Third Party\Dorset Software Services\DssVcl32\SQLList.pas',
  VersionInfo          in '..\..\..\..\Third Party\Dorset Software Services\DssVcl32\VersionInfo.pas',
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
