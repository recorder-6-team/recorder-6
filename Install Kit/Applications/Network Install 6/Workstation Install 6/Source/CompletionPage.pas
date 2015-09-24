{===============================================================================
  Unit:        CompletionPage

  Defines:     TfraCompletion

  Description:

  Model:       Workstation Install 6.mpb

  Created:     November 2004

  Last revision information:
    $Revision: 4 $
    $Date: 14/07/09 15:43 $
    $Author: Ericsalmon $

===============================================================================}

unit CompletionPage;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, BasePage, StdCtrls, Settings, ExtCtrls, GeneralFunctions;

type
  TfraCompletion = class(TBasePage)
    lblUpgrading: TLabel;
    lblUpgradingAdditional: TLabel;
    lblUpgradingWarning: TLabel;
    lblCompletionInfo: TLabel;
    chkLaunchGuide: TCheckBox;
    chkLaunchRecorder: TCheckBox;
  protected
    function GetConfirmCancel: Boolean; override;
    function GetHasNext: Boolean; override;
    function GetIsFinal: Boolean; override;
    function GetNextCaption: String; override;
    function GetResourceImage: String; override;
    procedure SaveContent; override;
  end;

//==============================================================================
implementation

{$R *.dfm}

uses
  SetupConstants, TextMessages;

{-==============================================================================
    TfraCompletion
===============================================================================}
{-------------------------------------------------------------------------------
}
function TfraCompletion.GetConfirmCancel: Boolean;
begin
  Result := False;  // Confirmation not necessary
end;  // TfraCompletion.GetConfirmCancel 

{-------------------------------------------------------------------------------
}
function TfraCompletion.GetHasNext: Boolean;
begin
  Result := True;
end;  // TfraCompletion.GetHasNext 

{-------------------------------------------------------------------------------
}
function TfraCompletion.GetIsFinal: Boolean;
begin
  Result := True;
end;  // TfraCompletion.GetIsFinal 

{-------------------------------------------------------------------------------
}
function TfraCompletion.GetNextCaption: String;
begin
  Result := ResStr_MainMenuCaption;
end;  // TfraCompletion.GetNextCaption

{-------------------------------------------------------------------------------
}
function TfraCompletion.GetResourceImage: String;
begin
  Result := ResImg_Completion;
end;  // TfraCompletion.GetResourceImage

{-------------------------------------------------------------------------------
}
procedure TfraCompletion.SaveContent;
begin
  inherited;
  if chkLaunchGuide.Checked then
    ShellFile(Settings.RootFolder + STR_GETTING_STARTED);
  if chkLaunchRecorder.Checked then
    ShellFile(Settings.RootFolder + STR_RECORDER_SPLASH_EXE);
end;

end.
