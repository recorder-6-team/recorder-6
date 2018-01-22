{-------------------------------------------------------------------------------
  Program:     Uninstaller.dpr

  Description: Recorder uninstall wizard.

  Created:     April 2003

  Last revision information:
    $Revision: $
    $Date: $
    $Author: $

-------------------------------------------------------------------------------}

program Uninstall;

{$R 'AdditionalResources.res' 'AdditionalResources.rc' 'Uninstall.res'}

uses
  Forms, LocOnFly,
  Recorder2000_TLB     in '..\..\..\..\RecorderApp\Recorder2000_TLB.pas',
  Functions            in '..\..\Common 6\Functions.pas',
  InstallAddins        in '..\..\Common 6\InstallAddins.pas',
  Settings             in '..\..\Common 6\Settings.pas',
  SetupConstants       in '..\..\Common 6\SetupConstants.pas',
  Shortcuts            in '..\..\Common 6\Shortcuts.pas',
  TextMessages         in '..\..\Common 6\TextMessages.pas',

  FrameBase            in 'Source\FrameBase.pas' {PageFrame: TFrame},
  FrameUninstall       in 'Source\FrameUninstall.pas' {fraUninstall: TFrame},
  FrameUninstallSelect in 'Source\FrameUninstallSelect.pas' {fraUninstallSelect: TFrame},
  FrameWelcome         in 'Source\FrameWelcome.pas' {fraWelcome: TFrame},
  Main                 in 'Source\Main.pas' {frmMain},

  
  ADODB_TLB            in '..\..\..\..\Third Party\Dorset Software Services\DssVcl32\ADODB_TLB.pas',
  ApiUtils             in '..\..\..\..\Third Party\Dorset Software Services\DssVcl32\ApiUtils.pas',
  ExceptionForm        in '..\..\..\..\Third Party\Dorset Software Services\DssVcl32\ExceptionForm.pas',
  CustomOleCtrls7      in '..\..\..\..\Third Party\Dorset Software Services\DssVcl32\CustomOleCtrls7.pas',
  GeneralFunctions     in '..\..\..\..\Third Party\Dorset Software Services\DssVcl32\GeneralFunctions.pas',
  ListDlls             in '..\..\..\..\Third Party\Dorset Software Services\DssVcl32\ListDlls.pas',
  OLETools             in '..\..\..\..\Third Party\Dorset Software Services\DssVcl32\OLETools.pas',
  ServiceHandler       in '..\..\..\..\Third Party\Dorset Software Services\DssVcl32\ServiceHandler.pas',
  SQLList              in '..\..\..\..\Third Party\Dorset Software Services\DssVcl32\SQLList.pas',
  VersionInfo          in '..\..\..\..\Third Party\Dorset Software Services\DssVcl32\VersionInfo.pas';


{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Recorder 6 Uninstall';
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
