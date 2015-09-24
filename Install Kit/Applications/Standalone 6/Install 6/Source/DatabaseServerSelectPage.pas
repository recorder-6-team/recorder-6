{===============================================================================
  Unit:        DatabaseServerSelectPage

  Defines:     TfraDatabaseServerSelect

  Description: Simplified version of the server install unit to skip straight to
               this project's own version of TfraLogin.

  Model:       Workstation Install 6.mpb

  Created:     January 2005

  Last revision information:
    $Revision: 2 $
    $Date: 11/02/09 15:39 $
    $Author: Ericsalmon $

===============================================================================}

unit DatabaseServerSelectPage;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, BasePage, StdCtrls, ExtCtrls;

type
  TfraDatabaseServerSelect = class(TBasePage)
  protected
    function GetConfirmCancel: Boolean; override;
    function GetHasNext: Boolean; override;
    function GetNext: TBasePageClass; override;
  public
    function Execute: Boolean; override;
  end;

//==============================================================================
implementation

{$R *.dfm}

uses
  MigrateSettings2Page, LoginPage;

{-==============================================================================
    TfraDatabaseServerSelect
===============================================================================}
{-------------------------------------------------------------------------------
}
function TfraDatabaseServerSelect.Execute: Boolean;
begin
  Result := True;
end;  // TfraDatabaseServerSelect.Execute

{-------------------------------------------------------------------------------
}
function TfraDatabaseServerSelect.GetConfirmCancel: Boolean;
begin
  Result := False;
end;  // TfraDatabaseServerSelect.GetConfirmCancel

{-------------------------------------------------------------------------------
}
function TfraDatabaseServerSelect.GetHasNext: Boolean;
begin
  Result := True;
end;  // TfraDatabaseServerSelect.GetHasNext

{-------------------------------------------------------------------------------
}
function TfraDatabaseServerSelect.GetNext: TBasePageClass;
begin
  Result := TfraLogin;
end;  // TfraDatabaseServerSelect.GetNext

end.

