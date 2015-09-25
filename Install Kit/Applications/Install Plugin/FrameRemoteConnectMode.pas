{-------------------------------------------------------------------------------
  Unit:        FrameRemoteConnectMode.pas

  Defines:     TfraRemoteConnectMode

  Description: Allow users to specify the mode of authentication to use to
               connect to selected server.

  Created:     March 2003

  Last revision information:
    $Revision: 6 $
    $Date: 8/03/04 10:24 $
    $Author: Ericsalmon $

-------------------------------------------------------------------------------}

unit FrameRemoteConnectMode;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, FrameBase, Settings, StdCtrls;

type
  TfraRemoteConnectMode = class(TPageFrame)
    Label1: TLabel;
    Label2: TLabel;
    cbNTAuthentication: TCheckBox;
    Label3: TLabel;
    procedure cbNTAuthenticationClick(Sender: TObject);
  protected
    function GetNextFrame: TPageFrameClass; override;
    procedure SetSettings(const Value: TSettings); override;
  end;

//==============================================================================
implementation

{$R *.dfm}

uses
  FrameRemoteInstructions, FrameComplete;

//==============================================================================
{ TfraRemoteConnectMode }
{-------------------------------------------------------------------------------
  Description : initialises component states
  Created : 23/03/2003 }
procedure TfraRemoteConnectMode.SetSettings(const Value: TSettings);
begin
  inherited;
  cbNtAuthentication.Checked := Settings.NTAuthentication;
end;

//------------------------------------------------------------------------------
function TfraRemoteConnectMode.GetNextFrame: TPageFrameClass;
begin
  if Settings.SelectedServerIsLocal then
    Result := TfraComplete
  else
    Result := TfraRemoteInstructions;
end;

//------------------------------------------------------------------------------
procedure TfraRemoteConnectMode.cbNTAuthenticationClick(Sender: TObject);
begin
  inherited;
  Settings.NTAuthentication := cbNtAuthentication.Checked;
end;

//------------------------------------------------------------------------------
end.
