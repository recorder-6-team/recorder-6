{===============================================================================
  Unit:        CompletionPage

  Defines:     TfraCompletion

  Description:

  Model:       Server Install 6.mpb

  Created:     November 2004

  Last revision information:
    $Revision: 9 $
    $Date: 14/07/09 11:38 $
    $Author: Ericsalmon $

===============================================================================}

unit CompletionPage;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, BasePage, StdCtrls, Settings, ExtCtrls;

type
  TfraCompletion = class(TBasePage)
    pnlDatabase: TPanel;
    lblDatabaseInfo: TLabel;
    Label1: TLabel;
    pnlRecorder: TPanel;
    lblRecorderInfo: TLabel;
    Label2: TLabel;
  protected
    function GetConfirmCancel: Boolean; override;
    function GetHasNext: Boolean; override;
    function GetIsFinal: Boolean; override;
    function GetNextCaption: String; override;
    function GetResourceImage: String; override;
    procedure LoadContent; override;
  end;
  
//==============================================================================
implementation

{$R *.dfm}

uses
  SetupConstants, TextMessages, GeneralFunctions;

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
procedure TfraCompletion.LoadContent;
begin
  inherited;
  pnlRecorder.Visible := Settings.Mode = moInstallR6;
  pnlDatabase.Visible := Settings.Mode = moInstallDatabase;
end;  // TfraCompletion.LoadContent

end.
