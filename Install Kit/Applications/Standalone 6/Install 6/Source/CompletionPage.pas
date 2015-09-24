{===============================================================================
  Unit:        CompletionPage

  Defines:     TfraCompletion

  Description:

  Model:

  Created:     March 2004

  Last revision information:
    $Revision: 3 $
    $Date: 3/07/09 15:26 $
    $Author: Ericsalmon $

===============================================================================}

unit CompletionPage;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, BasePage, StdCtrls, ExtCtrls;

type
  TfraCompletion = class (TBasePage)
    lblInformation: TLabel;
  protected
    function GetHasNext: Boolean; override;
    function GetIsFinal: Boolean; override;
    function GetNextCaption: String; override;
    function GetResourceImage: String; override;
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
  Result := ResStr_ContinueCaption;
end;  // TfraCompletion.GetNextCaption

{-------------------------------------------------------------------------------
}
function TfraCompletion.GetResourceImage: String;
begin
  Result := ResImg_Completion;
end;  // TfraCompletion.GetResourceImage

end.
