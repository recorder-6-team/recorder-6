{-------------------------------------------------------------------------------
  Unit:        FrameWelcome.pas

  Defines:     TfraWelcome

  Description: First page of the uninstall wizard. Show introductory text. No
               additional functionality.

  Created:     April 2003

  Last revision information:
    $Revision: 1 $
    $Date: 14/05/04 17:05 $
    $Author: Ericsalmon $

-------------------------------------------------------------------------------}

unit FrameWelcome;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, FrameBase;

type
  TfraWelcome = class(TPageFrame)
    lblWelcomeInstruct1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    lblWelcomeTitle: TLabel;
  protected
    procedure SetNextButton(const Value: TButton); override;
    function GetNextFrame : TPageFrameClass; override;
  end;

//==============================================================================
implementation

{$R *.dfm}

uses
  FrameUninstallSelect, FrameUninstall;

//==============================================================================
{ TfraWelcome }
{-------------------------------------------------------------------------------
  14/02/2003
  Returns the class type of the appropriate page to follow this.
}
function TfraWelcome.GetNextFrame: TPageFrameClass;
begin
  if Settings.SkipToUninstall then
    Result := TfraUninstall
  else
    Result := TfraUninstallSelect;
end;

//------------------------------------------------------------------------------
procedure TfraWelcome.SetNextButton(const Value: TButton);
begin
  inherited;
  if Assigned(NextButton) then NextButton.Enabled := true;
end;

//------------------------------------------------------------------------------
end.
