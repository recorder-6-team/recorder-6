{===============================================================================
  Unit:        MigrateWelcomePage

  Defines:     TfraMigrateWelcome

  Description:

  Model:

  Created:     March 2004

  Last revision information:
    $Revision: 7 $
    $Date: 5/08/09 10:47 $
    $Author: Ericsalmon $

===============================================================================}

unit MigrateWelcomePage;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, BasePage, StdCtrls, Settings, ExtCtrls;

type
  TfraMigrateWelcome = class(TBasePage)
    lblWarning: TLabel;
    lblSiteIDInformation: TLabel;
    lblDiskSpaceInformation: TLabel;
    lblInformation: TLabel;
  protected
    function GetConfirmCancel: Boolean; override;
    function GetHasNext: Boolean; override;
    function GetHasPrevious: Boolean; override;
    function GetNext: TBasePageClass; override;
    function GetResourceImage: String; override;
  end;

//==============================================================================
implementation

{$R *.dfm}

uses
  SetupConstants, DatabaseServerSelectPage, LoginPage;

{-==============================================================================
    TfraMigrateWelcome
===============================================================================}
{-------------------------------------------------------------------------------
}
function TfraMigrateWelcome.GetConfirmCancel: Boolean;
begin
  Result := False;  // Confirmation not necessary
end;  // TfraMigrateWelcome.GetConfirmCancel 

{-------------------------------------------------------------------------------
}
function TfraMigrateWelcome.GetHasNext: Boolean;
begin
  Result := True;
end;  // TfraMigrateWelcome.GetHasNext

{-------------------------------------------------------------------------------
}
function TfraMigrateWelcome.GetHasPrevious: Boolean;
begin
  Result := True;
end;  // TfraMigrateWelcome.GetHasPrevious

{-------------------------------------------------------------------------------
}
function TfraMigrateWelcome.GetNext: TBasePageClass;
begin
  // Note: Network setup has it's own LoginPage that knows to go back
  // to the appropriate previous page.
  if Settings.InstallType = itServer then
    Result := TfraDatabaseServerSelect
  else
    Result := TfraLogin;
end;  // TfraMigrateWelcome.GetNext

{-------------------------------------------------------------------------------
}
function TfraMigrateWelcome.GetResourceImage: String;
begin
  Result := ResImg_Welcome;
end;  // TfraMigrateWelcome.GetResourceImage

end.
