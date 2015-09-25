{===============================================================================
  Unit:        FrameUninstall.pas

  Defines:     TfraUninstall

  Description: Main uninstall processes are run from here.

  Model:       Server Uninstall 6.mpb

  Created:     November 2004

  Last revision information:
    $Revision: 4 $
    $Date: 19/02/09 16:55 $
    $Author: Ericsalmon $

===============================================================================}

unit FrameUninstall;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, FrameBase, Settings, ComCtrls, TextMessages, StdCtrls, ExtCtrls;

type
  TfraUninstall = class(TPageFrame, IExecutePage)
    Animation: TAnimate;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    lblFiles: TLabel;
    lblRegistry: TLabel;
    lblStepFiles: TLabel;
    lblStepRegistry: TLabel;
    pnlExecute: TPanel;
    pnlFinish: TPanel;
    tmrAnim: TTimer;
    procedure tmrAnimTimer(Sender: TObject);
  private
    procedure RemoveFiles;
    procedure RemoveRegistryEntries;
  protected
    procedure DoPreProcess; override;
    procedure Execute;
    function GetIsFinal: Boolean; override;
    function GetNextButtonCaption: String; override;
  end;
  
//==============================================================================
implementation

{$R *.dfm}

uses
  GeneralFunctions, SetupConstants, Registry;

{-==============================================================================
    TfraUninstall
===============================================================================}
procedure TfraUninstall.DoPreProcess;
begin
  if Settings.OSVersion = wvVista then begin
    Settings.SetAnimationFromResource(Animation, ResAvi_VistaFileDelete);
    tmrAnim.Enabled  := False;
    Animation.Active := True;
  end;
end;

{-------------------------------------------------------------------------------
}
procedure TfraUninstall.Execute;
var
  lCursor: TCursor;
begin
  Refresh;
  pnlExecute.Visible := True;
  pnlFinish.Visible  := False;
  NextButton.Enabled := False;
  lCursor := HourglassCursor;
  // Start animation, something to look at...
  Animation.Active := True;
  Application.ProcessMessages;
  try
    RemoveFiles;
    RemoveRegistryEntries;
    WaitFor(1);
  finally
    DefaultCursor(lCursor);
    // Enough of that now
    Animation.Active := False;
  end;
  
  // Switch panels around
  pnlExecute.Visible := False;
  pnlFinish.Visible  := True;
  NextButton.Enabled := True;
  Application.ProcessMessages;
end;  // TfraUninstall.Execute 

{-------------------------------------------------------------------------------
}
function TfraUninstall.GetIsFinal: Boolean;
begin
  Result := True;
end;  // TfraUninstall.GetIsFinal 

{-------------------------------------------------------------------------------
}
function TfraUninstall.GetNextButtonCaption: String;
begin
  Result := ResStr_FinishCaption;
end;  // TfraUninstall.GetNextButtonCaption 

{-------------------------------------------------------------------------------
}
procedure TfraUninstall.RemoveFiles;
begin
  lblStepFiles.Caption := STR_ARROW;
  lblFiles.Font.Style := [fsBold];
  WaitFor(1);
  
  // Remove files and folders.
  Settings.DeleteLoggedFiles;
  Settings.DeleteLoggedFolders;
  
  lblFiles.Font.Style := [];
  lblStepFiles.Caption := STR_TICK;
end;  // TfraUninstall.RemoveFiles 

{-------------------------------------------------------------------------------
}
procedure TfraUninstall.RemoveRegistryEntries;
begin
  lblStepRegistry.Caption := STR_ARROW;
  lblRegistry.Font.Style := [fsBold];
  WaitFor(1);
  
  with TRegistry.Create do
    try
      // Delete machine's settings.
      RootKey := HKEY_LOCAL_MACHINE;
      Access  := KEY_ALL_ACCESS;
      if KeyExists(REG_KEY_R6_SERVER) then DeleteKey(REG_KEY_R6_SERVER);
      CloseKey;
    finally
      Free;
    end;
  
  lblRegistry.Font.Style := [];
  lblStepRegistry.Caption := STR_TICK;
end;  // TfraUninstall.RemoveRegistryEntries 

{-------------------------------------------------------------------------------
}
procedure TfraUninstall.tmrAnimTimer(Sender: TObject);
begin
  inherited;
  with Animation do begin
    StartFrame := (StartFrame + 1) mod FrameCount;
    Play(StartFrame, StartFrame, 1);
  end;
end;  // TfraUninstall.tmrAnimTimer 

end.


