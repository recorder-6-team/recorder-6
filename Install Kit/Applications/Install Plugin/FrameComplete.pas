{-------------------------------------------------------------------------------
  Unit:        FrameComplete.pas

  Defines:     TfraComplete

  Description: Last page of the wizard.

  Created:     February 2003

  Last revision information:
    $Revision: 5 $
    $Date: 8/03/04 10:24 $
    $Author: Ericsalmon $

-------------------------------------------------------------------------------}

unit FrameComplete;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, FrameBase, StdCtrls, TextMessages, Settings;

type
  TfraComplete = class(TPageFrame)
    Label1: TLabel;
    lblProceed: TLabel;
    lblSystemCompInfo: TLabel;
    lblCDInfo: TLabel;
    lblRebootInfo: TLabel;
  protected
    function GetIsFinal: Boolean; override;
    function GetNextButtonCaption: string; override;
    procedure DoPreProcess; override;
  end;

//==============================================================================
implementation

{$R *.dfm}

//==============================================================================
{-------------------------------------------------------------------------------
  Description : Implements ICompletionPage.NextButtonCaption to return the
              Finish caption for the button.
  Created : 25/02/2003 }
function TfraComplete.GetNextButtonCaption: string;
begin
  if Settings.InstallMode = imServer then begin
    Result := ST_FINISH_CAPTION;
    lblSystemCompInfo.Visible := false;
    lblRebootInfo.Visible     := true;
    lblProceed.Caption        := 'Click Finish to continue';
  end else
    Result := ST_PROCEED_CAPTION;
end;

//------------------------------------------------------------------------------
procedure TfraComplete.DoPreProcess;
begin
  Settings.SaveToIniFile;
  if Assigned(NextButton) then NextButton.SetFocus;
end;

function TfraComplete.GetIsFinal: Boolean;
begin
  Result := True;
end;

end.
