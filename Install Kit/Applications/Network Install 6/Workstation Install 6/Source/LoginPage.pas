{===============================================================================
  Unit:        LoginPage

  Defines:     TfraLogin

  Description:

  Model:       Workstation Install 6.mpb

  Created:     November 2004

  Last revision information:
    $Revision: 3 $
    $Date: 19/02/09 16:55 $
    $Author: Ericsalmon $

===============================================================================}

unit LoginPage;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, BasePage, StdCtrls, ExtCtrls;

type
  TfraLogin = class(TBasePage)
    ePassword: TEdit;
    eUsername: TEdit;
    gbLogin: TGroupBox;
    Label16: TLabel;
    Label17: TLabel;
    lblCommonInfo: TLabel;
    lblMigrateInfo1: TLabel;
    lblMigrateInfo2: TLabel;
    rbSQLLogin: TRadioButton;
    rbTrustedLogin: TRadioButton;
  protected
    function GetConfirmCancel: Boolean; override;
    function GetHasNext: Boolean; override;
    function GetHasPrevious: Boolean; override;
    function GetNext: TBasePageClass; override;
    function GetNextCaption: String; override;
    function GetPrevious: TBasePageClass; override;
    function GetResourceImage: String; override;
    procedure LoadContent; override;
    procedure SaveContent; override;
  end;
  
//==============================================================================
implementation

{$R *.dfm}

uses
  MigrateSettings2Page, MigratePage, Settings, SetupConstants, TextMessages;

{-==============================================================================
    TfraLogin
===============================================================================}
{-------------------------------------------------------------------------------
}
function TfraLogin.GetConfirmCancel: Boolean;
begin
  Result := (Settings.Mode <> moInstallR6) or (Settings.FoldersCreated.Count <> 0);
end;  // TfraLogin.GetConfirmCancel 

{-------------------------------------------------------------------------------
}
function TfraLogin.GetHasNext: Boolean;
begin
  Result := GetNext <> nil;
end;  // TfraLogin.GetHasNext 

{-------------------------------------------------------------------------------
}
function TfraLogin.GetHasPrevious: Boolean;
begin
  Result := GetPrevious <> nil;
end;  // TfraLogin.GetHasPrevious 

{-------------------------------------------------------------------------------
}
function TfraLogin.GetNext: TBasePageClass;
begin
  Result := TfraMigrate;
end;  // TfraLogin.GetNext 

{-------------------------------------------------------------------------------
}
function TfraLogin.GetNextCaption: String;
begin
  Result := ResStr_TransferCaption
end;  // TfraLogin.GetNextCaption 

{-------------------------------------------------------------------------------
}
function TfraLogin.GetPrevious: TBasePageClass;
begin
  Result := TfraMigrateSettings2;
end;  // TfraLogin.GetPrevious 

{-------------------------------------------------------------------------------
}
function TfraLogin.GetResourceImage: String;
begin
  Result := ResImg_DatabaseLogin;
end;  // TfraLogin.GetResourceImage 

{-------------------------------------------------------------------------------
}
procedure TfraLogin.LoadContent;
begin
  inherited;
  with Settings do begin
    rbSQLLogin.Checked := not TrustedLogin;
    rbTrustedLogin.Checked := TrustedLogin;
    eUserName.Text := UserName;
    ePassword.Text := Password;
  end;
  gbLogin.Caption := ' ' + Format(ResStr_ServerLogin, [Settings.ServerName]) + ' ';
end;  // TfraLogin.LoadContent 

{-------------------------------------------------------------------------------
}
procedure TfraLogin.SaveContent;
begin
  inherited;
  with Settings do begin
    TrustedLogin := rbTrustedLogin.Checked;
    UserName := eUsername.Text;
    Password := ePassword.Text;
  end;
end;  // TfraLogin.SaveContent 

end.


