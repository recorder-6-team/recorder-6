{-------------------------------------------------------------------------------
  Unit:        FrameWelcome.pas

  Defines:     TfraWelcome

  Description: First page of the wizard. Show introductory text. No additional
               functionality.

  Created:     February 2003

  Last revision information:
    $Revision: 7 $
    $Date: 8/03/04 10:24 $
    $Author: Ericsalmon $

-------------------------------------------------------------------------------}

unit FrameWelcome;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, FrameBase, Settings, TextMessages;

type
  TfraWelcome = class(TPageFrame)
    lblWelcomeInstruct1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    lblWelcomeTitle: TLabel;
    Label5: TLabel;
  protected
    function GetNextFrame: TPageFrameClass; override;
    procedure SetNextButton(const Value: TButton); override;
    procedure SetSettings(const Value : TSettings); override;
  end;

//==============================================================================
implementation

{$R *.dfm}

uses
  FrameInstallFolder, FrameSpatialRef;

//==============================================================================
{ TfraWelcome }
{-------------------------------------------------------------------------------
  14/02/2003
  Returns the class type of the appropriate page to follow this.
}
function TfraWelcome.GetNextFrame: TPageFrameClass;
begin
  if Settings.InstallMode = imWorkstation then
    Result := TfraSpatialRef
  else
    Result := TfraInstallFolder
end;

//------------------------------------------------------------------------------
procedure TfraWelcome.SetNextButton(const Value: TButton);
begin
  inherited;
  if Assigned(NextButton) then NextButton.Enabled := true;
end;

{-------------------------------------------------------------------------------
  Description : Sets the labels to say 'Upgrade' if relevant'
  Created : 25/03/2003 }
procedure TfraWelcome.SetSettings(const Value: TSettings);
begin
  inherited;
  if Value.InstallMode = imUpgrade then begin
    lblWelcomeTitle.Caption := StringReplace(lblWelcomeTitle.Caption, 'Installation', 'Upgrade',
                            [rfReplaceAll]);
    lblWelcomeInstruct1.Caption := StringReplace(lblWelcomeInstruct1.Caption, 'installation', 'upgrade',
                            [rfReplaceAll]);
  end;
end;

end.
