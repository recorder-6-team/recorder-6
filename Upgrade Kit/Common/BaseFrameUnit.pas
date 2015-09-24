{===============================================================================
  Unit:        UpgradeFrame

  Defines:     TfraUpgrade

  Description: Progress splash screen for Recorder database upgrades.

  Created:

  Last revision information:
    $Revision: 5 $
    $Date: 6/07/09 10:14 $
    $Author: Ericsalmon $

===============================================================================}
unit BaseFrameUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, Settings, ExtCtrls;

type
  {-----------------------------------------------------------------------------
    Base frame for each page in the upgrade wizard.
  }
  TBaseFrame = class (TFrame)
    pnlTitle: TPanel;
    lblTitle: TLabel;
  public
    procedure ApplySettings(ASettings: TSettings); virtual;
    procedure Validate(ASettings: TSettings; var ACanProceed: boolean); virtual;
    function CreateNextFrame(AOwner: TComponent): TBaseFrame; virtual;
    procedure Execute(ASettings: TSettings); virtual;
  end;
  
//==============================================================================

implementation

{$R *.dfm}

{-==============================================================================
    TBaseFrame
===============================================================================}
{-------------------------------------------------------------------------------
  Any empty stub for the ApplySettings is used rather than an abstract method because this 
      allows the derived frames to not implement this method if no action required. 
}
procedure TBaseFrame.ApplySettings(ASettings: TSettings);
begin
end;  // TBaseFrame.ApplySettings 

{-------------------------------------------------------------------------------
  Base method for returning the next frame in sequence. 
}
function TBaseFrame.CreateNextFrame(AOwner: TComponent): TBaseFrame;
begin
  Result := nil;
end;  // TBaseFrame.CreateNextFrame

{-------------------------------------------------------------------------------
  Virtual execute method - empty rather than abstract so that it does not have to be
      implemented.
  This is called as soon as a frame is shown, allowing it to perform any actions.
}
procedure TBaseFrame.Execute(ASettings: TSettings);
begin
  // do nothing
end;  // TBaseFrame.Execute

(**
 * Validate method - override in frames. Set ACanProceed to false to prevent going
 * to next frame. Frame is responsible for displaying any messages.
 *)
procedure TBaseFrame.Validate(ASettings: TSettings;
  var ACanProceed: boolean);
begin
end;

end.





