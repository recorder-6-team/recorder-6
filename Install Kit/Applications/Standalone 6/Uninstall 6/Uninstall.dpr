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
  Recorder2000_TLB     in '..\..\..\..\..\RecorderApp\trunk\Recorder2000_TLB.pas',
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

  ADODB_TLB            in '..\..\..\..\..\Third Party\Dorset Software Services\DssVcl32\trunk\ADODB_TLB.pas',
  ExceptionForm        in '..\..\..\..\..\Third Party\Dorset Software Services\DssVcl32\trunk\ExceptionForm.pas',
  CustomOleCtrls7      in '..\..\..\..\..\Third Party\Dorset Software Services\DssVcl32\trunk\CustomOleCtrls7.pas',
  OLETools             in '..\..\..\..\..\Third Party\Dorset Software Services\DssVcl32\trunk\OLETools.pas',
  ServiceHandler       in '..\..\..\..\..\Third Party\Dorset Software Services\DssVcl32\trunk\ServiceHandler.pas',
  SQLList              in '..\..\..\..\..\Third Party\Dorset Software Services\DssVcl32\trunk\SQLList.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Recorder 6 Uninstall';
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
