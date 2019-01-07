{-------------------------------------------------------------------------------
  Program:     Uninstaller.dpr

  Description: Recorder uninstall wizard.

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
  Functions            in '..\..\Common 6\Functions.pas',
  SetupConstants       in '..\..\Common 6\SetupConstants.pas',
  TextMessages         in '..\..\Common 6\TextMessages.pas',
  FrameBase            in '..\..\Common 6\FrameBase.pas' {PageFrame: TFrame},
  Settings             in '..\..\Common 6\Settings.pas',
  ApiUtils             in '..\..\..\..\Third Party\Dorset Software Services\DssVcl32\ApiUtils.pas',
  ExceptionForm        in '..\..\..\..\Third Party\Dorset Software Services\DssVcl32\ExceptionForm.pas',
  GeneralFunctions     in '..\..\..\..\Third Party\Dorset Software Services\DssVcl32\GeneralFunctions.pas',
  ListDlls             in '..\..\..\..\Third Party\Dorset Software Services\DssVcl32\ListDlls.pas',
  SQLList              in '..\..\..\..\Third Party\Dorset Software Services\DssVcl32\SQLList.pas',
  VersionInfo          in '..\..\..\..\Third Party\Dorset Software Services\DssVcl32\VersionInfo.pas',

  FrameUninstall       in 'Source\FrameUninstall.pas' {fraUninstall: TFrame},
  Main                 in 'Source\Main.pas' {frmMain};

{$R Uninstall.KLR}

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Recorder 6 Server Uninstall';
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
