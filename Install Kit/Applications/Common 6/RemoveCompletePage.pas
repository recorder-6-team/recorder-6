{===============================================================================
  Unit:        RemoveCompletePage

  Defines:     TfraRemoveComplete

  Description:

  Model:

  Created:

  Last revision information:
    $Revision: 4 $
    $Date: 11/02/09 15:26 $
    $Author: Ericsalmon $

===============================================================================}

unit RemoveCompletePage;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, BasePage, StdCtrls, ExtCtrls;

type
  TfraRemoveComplete = class(TBasePage)
    lblInformation: TLabel;
  protected
    function GetConfirmCancel: Boolean; override;
    function GetHasNext: Boolean; override;
    function GetIsFinal: Boolean; override;
    function GetNextCaption: String; override;
    function GetResourceImage: String; override;
  end;

//==============================================================================
implementation

{$R *.dfm}

uses
  SetupConstants, TextMessages;

{-==============================================================================
    TfraRemoveComplete
===============================================================================}
{-------------------------------------------------------------------------------
}
function TfraRemoveComplete.GetConfirmCancel: Boolean;
begin
  Result := False;
end;  // TfraRemoveComplete.GetConfirmCancel 

{-------------------------------------------------------------------------------
}
function TfraRemoveComplete.GetHasNext: Boolean;
begin
  Result := True;
end;  // TfraRemoveComplete.GetHasNext 

{-------------------------------------------------------------------------------
}
function TfraRemoveComplete.GetIsFinal: Boolean;
begin
  Result := True;
end;  // TfraRemoveComplete.GetIsFinal 

{-------------------------------------------------------------------------------
}
function TfraRemoveComplete.GetNextCaption: String;
begin
  Result := ResStr_MainMenuCaption;
end;  // TfraRemoveComplete.GetNextCaption 

{-------------------------------------------------------------------------------
}
function TfraRemoveComplete.GetResourceImage: String;
begin
  Result := ResImg_Completion;
end;  // TfraRemoveComplete.GetResourceImage 

end.
