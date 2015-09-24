{-------------------------------------------------------------------------------
  Unit:        FrameRemoteInstructions.pas

  Defines:     TfraRemoteInstructions

  Description: Tells the users what to do in case they link to a remote server.

  Created:     February 2003

  Last revision information:
    $Revision: 10 $
    $Date: 8/03/04 10:24 $
    $Author: Ericsalmon $

-------------------------------------------------------------------------------}

unit FrameRemoteInstructions;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, FrameBase, StdCtrls, Settings, TextMessages, ViewRemoteInstructions;

type
  TfraRemoteInstructions = class(TPageFrame)
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    btnView: TButton;
    procedure btnViewClick(Sender: TObject);
  protected
    function GetNextFrame: TPageFrameClass; override;
  end;

//==============================================================================
implementation

{$R *.dfm}

uses
  FrameComplete;

//==============================================================================
{ TfraRemoteInstructions }
//------------------------------------------------------------------------------
function TfraRemoteInstructions.GetNextFrame: TPageFrameClass;
begin
  Result := TfraComplete;
end;

//------------------------------------------------------------------------------
procedure TfraRemoteInstructions.btnViewClick(Sender: TObject);
begin
  inherited;
  with TdlgViewRemoteInstructions.Create(nil) do
    try
      SetSettings(Settings);
      ShowModal;
    finally
      Free;
    end;
end;



end.
