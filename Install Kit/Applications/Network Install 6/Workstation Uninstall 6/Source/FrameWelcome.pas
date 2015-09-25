{===============================================================================
  Unit:        FrameWelcome.pas

  Defines:     TfraWelcome

  Description: First page of the uninstall wizard. Show introductory text. No
               additional functionality.

  Model:       Workstation Uninstall 6.mpb

  Created:     November 2004

  Last revision information:
    $Revision: 2 $
    $Date: 8/11/04 16:05 $
    $Author: Ericsalmon $

===============================================================================}

unit FrameWelcome;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, FrameBase;

type
  TfraWelcome = class(TPageFrame)
    Label2: TLabel;
    Label3: TLabel;
    lblWelcomeInstruct1: TLabel;
    lblWelcomeTitle: TLabel;
  protected
    function GetNextFrame: TPageFrameClass; override;
    procedure SetNextButton(const Value: TButton); override;
  end;
  
//==============================================================================
implementation

{$R *.dfm}

uses
  FrameUninstallSelect, FrameUninstall;

{-==============================================================================
    TfraWelcome
===============================================================================}
{-------------------------------------------------------------------------------
}
function TfraWelcome.GetNextFrame: TPageFrameClass;
begin
  if Settings.SkipToUninstall then
    Result := TfraUninstall
  else
    Result := TfraUninstallSelect;
end;  // TfraWelcome.GetNextFrame 

{-------------------------------------------------------------------------------
}
procedure TfraWelcome.SetNextButton(const Value: TButton);
begin
  inherited;
  if Assigned(NextButton) then NextButton.Enabled := true;
end;  // TfraWelcome.SetNextButton 

end.

