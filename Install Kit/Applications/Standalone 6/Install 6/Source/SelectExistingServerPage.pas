{===============================================================================
  Unit:        SelectExistingServerPage

  Defines:     TfraSelectExistingServer

  Description: Page for selecting an existing SQL Server instance.

  Created:     March 2009

  Last revision information:
    $Revision: 3 $
    $Date: 3/07/09 15:26 $
    $Author: Ericsalmon $

===============================================================================}

unit SelectExistingServerPage;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, BaseSQLExpressSettingsPage, StdCtrls, ExtCtrls, BasePage, ADODB,
  ExceptionForm;

type

  ESelectExistingServer = class (TExceptionPath);

  TfraSelectExistingServer = class(TfraBaseSQLExpressSettings)
    lblInstanceName: TLabel;
    cmbInstances: TComboBox;
    lblWarning: TLabel;
    lblDropDownInfo: TLabel;
    lblExplanation: TLabel;
    gbLogin: TGroupBox;
    rbTrustedLogin: TRadioButton;
    Label1: TLabel;
    rbSQLLogin: TRadioButton;
    lblUsername: TLabel;
    lblPassword: TLabel;
    eUsername: TEdit;
    ePassword: TEdit;
    procedure cmbInstancesChange(Sender: TObject);
  protected
    function GetConfirmCancel: Boolean; override;
    function GetHasNext: Boolean; override;
    function GetHasPrevious: Boolean; override;
    function GetNext: TBasePageClass; override;
    function GetPrevious: TBasePageClass; override;
    function GetResourceImage: String; override;
    procedure LoadContent; override;
    procedure SaveContent; override;
  public
    procedure ValidateContent; override;
  end;

{==============================================================================}
implementation

uses
  DatabaseFileLocationPage, TextMessages, SpatialRefPage, SetupConstants,
  Settings;

{$R *.dfm}

{-------------------------------------------------------------------------------
  Gets whether or not the previous button should be enabled.
}
function TfraSelectExistingServer.GetHasPrevious: Boolean;
begin
  Result := True;
end;

{-------------------------------------------------------------------------------
  Gets the next page in the sequence.
}
function TfraSelectExistingServer.GetNext: TBasePageClass;
begin
  Result := TfraDatabaseFileLocation;

  if lblWarning.Visible then
    MessageDlg(ResStr_WindowsAuthenticationWarning, mtWarning, [mbOk], 0);
end;

{-------------------------------------------------------------------------------
  Gets the previous page in the sequence.
}
function TfraSelectExistingServer.GetPrevious: TBasePageClass;
begin
  Result := TfraSpatialRef;
end;

{-------------------------------------------------------------------------------
  Gets the file name of the image to display on this page.
}
function TfraSelectExistingServer.GetResourceImage: String;
begin
  Result := ResImg_SQLExpress;
end;  // TfraSelectExistingServer.GetResourceImage

{-------------------------------------------------------------------------------
  Sets up the page.
}
procedure TfraSelectExistingServer.LoadContent;
begin
  inherited;

  with Settings do begin
    // Only want local instances, no network stuff.
    cmbInstances.Items.Assign(LocalInstanceNames);

    cmbInstances.ItemIndex := cmbInstances.Items.IndexOf(ServerName);
    if cmbInstances.ItemIndex = -1 then
      cmbInstances.ItemIndex := 0;
    CheckTrustedConnectionMode(cmbInstances.Text, lblWarning);
  end;
end;

{-------------------------------------------------------------------------------
  Performs validation on the controls in the form.
}
procedure TfraSelectExistingServer.ValidateContent;
begin
  // Check entered name is valid
  if not ValidInstanceName(cmbInstances.Text) then begin
    MessageDlg(ResStr_InvalidInstanceName, mtWarning, [mbOk], 0);
    cmbInstances.SetFocus;
    Abort;
  end;

  // Force check for SQL 2005
  GetRegistryInstanceName(cmbInstances.Text);

  Settings.TrustedLogin := rbTrustedLogin.Checked;
  Settings.UserName := eUsername.Text;
  Settings.Password := ePassword.Text;
  Settings.ServerName := cmbInstances.Text;
  with TADOConnection.Create(nil) do try
    ConnectionString := Settings.GetConnectionString;
    try
      Open;
    except
      on E:Exception do begin
        MessageDlg(ResStr_InvalidLogin, mtWarning, [mbOk], 0);
        gbLogin.SetFocus;
        Abort;
      end;
    end;
  finally
    Free;
  end;
end;

{-------------------------------------------------------------------------------
  Saves the entered data to the settings file.
}
procedure TfraSelectExistingServer.SaveContent;
begin
  inherited;
  with Settings do begin
    ServerName             := cmbInstances.Text;
    SpecifyOwnInstanceName := False;
    SpecifyOwnInstancePath := False;
    TrustedLogin := rbTrustedLogin.Checked;
    UserName := eUsername.Text;
    Password := ePassword.Text;
    ServerName := cmbInstances.Text;
  end;
end;  // TfraSelectExistingServer.SaveContent

{-------------------------------------------------------------------------------
  When the selected server changes, update the warning label visibility.
}
procedure TfraSelectExistingServer.cmbInstancesChange(Sender: TObject);
begin
  inherited;
  CheckTrustedConnectionMode(cmbInstances.Text, lblWarning);
  // disable SQL login options if only trusted supported.
  rbSqlLogin.Enabled := not lblWarning.Visible;
  eUsername.Enabled := not lblWarning.Visible;
  ePassword.Enabled := not lblWarning.Visible;
  lblUsername.Enabled := not lblWarning.Visible;
  lblPassword.Enabled := not lblWarning.Visible;
  if lblWarning.Visible then
    rbTrustedLogin.Checked := true;
  ChangedContent;
end;

{-------------------------------------------------------------------------------
  Whether or not the user needs to confirm on cancelling the installation.
}
function TfraSelectExistingServer.GetConfirmCancel: Boolean;
begin
  Result := Settings.FoldersCreated.Count <> 0;
end;  // TfraSelectExistingServer.GetConfirmCancel

(**
 * Allow proceed to next page if server has been selected.
 *)
function TfraSelectExistingServer.GetHasNext: Boolean;
begin
  result := cmbInstances.Text <> '';
end;

end.
