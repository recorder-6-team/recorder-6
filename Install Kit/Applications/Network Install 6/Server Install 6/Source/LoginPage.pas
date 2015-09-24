{===============================================================================
  Unit:        LoginPage

  Defines:     TfraLogin

  Description:

  Model:       Server Install 6.mpb

  Created:     November 2004

  Last revision information:
    $Revision: 10 $
    $Date: 14/07/09 11:37 $
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
    lblUsername: TLabel;
    lblPassword: TLabel;
    lblAccessInfo: TLabel;
    lblCommonInfo: TLabel;
    lblDatabaseInfo: TLabel;
    lblMigrateInfo1: TLabel;
    lblMigrateInfo2: TLabel;
    rbSQLLogin: TRadioButton;
    rbTrustedLogin: TRadioButton;
    procedure rbSQLLoginEnter(Sender: TObject);
    procedure rbSQLLoginClick(Sender: TObject);
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
  MigrateSettings1Page, InstallationPage, ServerSelectPage, DatabaseServerSelectPage,
  Settings, SetupConstants, TextMessages;

{-==============================================================================
    TfraLogin
===============================================================================}
{-------------------------------------------------------------------------------
}
function TfraLogin.GetConfirmCancel: Boolean;
begin
  // Confirm only if there were actual files/folders created that may need removing.
  Result := (Settings.FilesInstalled.Count > 0) or (Settings.FoldersCreated.Count > 0);
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
  Result := nil;
  if Settings.Mode = moMigrate then
    Result := TfraMigrateSettings1
  else
  if Settings.Mode in [moInstallDatabase, moInstallR6] then
    Result := TfraInstallation;
end;  // TfraLogin.GetNext 

{-------------------------------------------------------------------------------
}
function TfraLogin.GetNextCaption: String;
begin
  if Settings.Mode = moMigrate then
    Result := inherited GetNextCaption
  else
  if Settings.Mode in [moInstallDatabase, moInstallR6] then
    Result := ResStr_InstallCaption;
end;  // TfraLogin.GetNextCaption

{-------------------------------------------------------------------------------
}
function TfraLogin.GetPrevious: TBasePageClass;
begin
  Result := nil;
  if Settings.Mode = moInstallDatabase then
    Result := TfraServerSelect
  else
  if Settings.Mode in [moInstallR6, moMigrate] then
    Result := TfraDatabaseServerSelect;
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
var
  lShowDatabaseInfo: Boolean;
  lShowMigrateInfo: Boolean;
  lShowAccessInfo: Boolean;
begin
  inherited;
  with Settings do begin
    if TrustedLogin then begin
      rbSQLLogin.Enabled := false;
      eUserName.Enabled := false;
      ePassword.Enabled := false;
      lblUsername.Enabled := false;
      lblPassword.Enabled := false;
      lblCommonInfo.Visible := false;      
    end else begin
      eUserName.Text := UserName;
      ePassword.Text := Password;
    end;
  end;

  lShowDatabaseInfo := Settings.Mode = moInstallDatabase;
  lShowMigrateInfo  := Settings.Mode = moMigrate;
  lShowAccessInfo   := Settings.Mode = moInstallR6;

  lblDatabaseInfo.Visible := lShowDatabaseInfo;

  lblMigrateInfo1.Visible := lShowMigrateInfo;
  lblMigrateInfo2.Visible := lShowMigrateInfo;

  lblAccessInfo.Visible := lShowAccessInfo;
  if lShowDatabaseInfo or lShowAccessInfo then
    gbLogin.Top := lblCommonInfo.Top + lblCommonInfo.Height + 32;

  gbLogin.Caption := ' ' + Format(ResStr_ServerLogin, [Settings.ServerName]) + ' ';
end;  // TfraLogin.LoadContent 

{-------------------------------------------------------------------------------
}
procedure TfraLogin.rbSQLLoginClick(Sender: TObject);
begin
  inherited;
  if rbSQLLogin.checked and eUserName.CanFocus then
    eUserName.SetFocus;
end;  // TfraLogin.rbSQLLoginClick

{-------------------------------------------------------------------------------
}
procedure TfraLogin.rbSQLLoginEnter(Sender: TObject);
begin
  inherited;
  // Move to user name if tabbing AND radiobutton checked.
  if rbSQLLogin.Checked then eUserName.SetFocus;
end;  // TfraLogin.rbSQLLoginEnter

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


