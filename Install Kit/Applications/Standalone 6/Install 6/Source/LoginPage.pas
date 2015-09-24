{===============================================================================
  Unit:        LoginPage

  Defines:     TfraLogin

  Description:

  Model:

  Created:     March 2004

  Last revision information:
    $Revision: 6 $
    $Date: 5/08/09 10:47 $
    $Author: Ericsalmon $

===============================================================================}

unit LoginPage;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, BasePage, StdCtrls, ExtCtrls;

type
  TfraLogin = class (TBasePage)
    ePassword: TEdit;
    eUsername: TEdit;
    gbLogin: TGroupBox;
    lblInformation: TLabel;
    lblNote: TLabel;
    lblUsername: TLabel;
    lblPassword: TLabel;
    rbSQLLogin: TRadioButton;
    rbTrustedLogin: TRadioButton;
  protected
    function GetHasNext: Boolean; override;
    function GetHasPrevious: Boolean; override;
    function GetNext: TBasePageClass; override;
    function GetPrevious: TBasePageClass; override;
    function GetResourceImage: String; override;
    procedure LoadContent; override;
    procedure SaveContent; override;
  end;
  
//==============================================================================
implementation

{$R *.dfm}

uses
  MigrateSettings1Page, Settings, TextMessages, MigrateWelcomePage, SetupConstants;

{-==============================================================================
    TfraLogin
===============================================================================}
{-------------------------------------------------------------------------------
}
function TfraLogin.GetHasNext: Boolean;
begin
  Result := True;
end;  // TfraLogin.GetHasNext

{-------------------------------------------------------------------------------
}
function TfraLogin.GetHasPrevious: Boolean;
begin
  Result := Settings.InstallType <> itServer;
end;  // TfraMigrateSettings2.GetHasPrevious

{-------------------------------------------------------------------------------
}
function TfraLogin.GetNext: TBasePageClass;
begin
  Result := TfraMigrateSettings1;
end;  // TfraLogin.GetNext

{-------------------------------------------------------------------------------
}
function TfraLogin.GetPrevious: TBasePageClass;
begin
  Result := TfraMigrateWelcome;
end;  // TfraMigrateSettings2.GetPrevious

{-------------------------------------------------------------------------------
}
function TfraLogin.GetResourceImage: String;
begin
  Result := ResImg_MigrateLogin;
end;  // TfraLogin.GetResourceImage

{-------------------------------------------------------------------------------
}
procedure TfraLogin.LoadContent;
begin
  inherited;
  with Settings do begin
    lblNote.Visible        := Mode <> moInstallR6;
    rbSQLLogin.Checked     := not TrustedLogin;
    rbTrustedLogin.Checked := TrustedLogin;
    eUserName.Text         := UserName;
    ePassword.Text         := Password;
    if not lblNote.Visible then
      gbLogin.Top := lblNote.Top;
  end;
end;  // TfraLogin.LoadContent 

{-------------------------------------------------------------------------------
}
procedure TfraLogin.SaveContent;
begin
  inherited;
  with Settings do begin
    TrustedLogin := rbTrustedLogin.Checked;
    UserName     := eUsername.Text;
    Password     := ePassword.Text;
  end;
end;  // TfraLogin.SaveContent 

end.

