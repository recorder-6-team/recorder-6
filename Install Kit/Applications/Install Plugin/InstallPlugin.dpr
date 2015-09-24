{-------------------------------------------------------------------------------
  Program:     InstallPlugin

  Description: Recorder installation wizard.

  Created:     February 2003

  Last revision information:
    $Revision: $
    $Date: $
    $Author: $

-------------------------------------------------------------------------------}

program InstallPlugin;

uses
  Forms,
  Constants               in '..\Common\Constants.pas',
  FrameBase               in '..\Common\FrameBase.pas' {PageFrame: TFrame},
  InstallAddins           in '..\Common\InstallAddins.pas',
  Settings                in '..\Common\Settings.pas',
  Shortcuts               in '..\Common\Shortcuts.pas',
  TextMessages            in '..\Common\TextMessages.pas',
  Main                    in 'Main.pas' {frmMain},
  CopyFiles               in 'CopyFiles.pas',
  FixShortcuts            in 'FixShortcuts.pas',
  FrameComplete           in 'FrameComplete.pas' {fraComplete: TFrame},
  FrameCutOffYear         in 'FrameCutOffYear.pas' {fraCutOffYear: TFrame},
  FrameInstallation       in 'FrameInstallation.pas' {fraInstallation: TFrame},
  FrameInstallFolder      in 'FrameInstallFolder.pas' {fraInstallFolder: TFrame},
  FrameNewOrExisting      in 'FrameNewOrExisting.pas' {fraNewOrExisting: TFrame},
  FrameRemoteConnectMode  in 'FrameRemoteConnectMode.pas' {fraRemoteConnectMode: TFrame},
  FrameRemoteInstructions in 'FrameRemoteInstructions.pas' {fraRemoteInstructions: TFrame},
  FrameServerFiles        in 'FrameServerFiles.pas' {fraServerFiles: TFrame},
  FrameServerSettings     in 'FrameServerSettings.pas' {fraServerSettings: TFrame},
  FrameSiteInfo           in 'FrameSiteInfo.pas' {fraSiteInfo: TFrame},
  FrameSpatialRef         in 'FrameSpatialRef.pas' {fraSpatialRef: TFrame},
  FrameWelcome            in 'FrameWelcome.pas' {fraWelcome: TFrame},
  InstallMSDE             in 'InstallMSDE.pas',
  ViewRemoteInstructions  in 'ViewRemoteInstructions.pas' {dlgViewRemoteInstructions};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.

