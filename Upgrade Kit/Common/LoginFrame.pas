{===============================================================================
  Unit:        LoginFrame

  Defines:     TfraLogin

  Description: Login details for the upgrade process

  Created:

  Last revision information:
    $Revision: 6 $
    $Date: 6/07/09 10:14 $
    $Author: Ericsalmon $

===============================================================================}
unit LoginFrame;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, UpgradeFrame, BaseFrameUnit, Settings,keycheckframe;

resourcestring
  ResStr_CannotConnect = 'The connection settings you have provided could not connect to the '+
      'database server. The error message reported was:'#13#10' %s';
  ResStr_InsufficientRights = 'The connection settings you have provided do not have sufficient rights to the '+
      'Recorder database. Please provide a login which has database owner rights to the database.';

type
  {-----------------------------------------------------------------------------
    Frame containing user controls to obtain the user's database login.
  }
  TfraLogin = class (TBaseFrame)
    lblClickNext: TLabel;
    pnlLogin: TPanel;
    gbLogin: TGroupBox;
    lblUsername: TLabel;
    lblPassword: TLabel;
    rbSQL: TRadioButton;
    eUsername: TEdit;
    ePassword: TEdit;
    rbTrusted: TRadioButton;
    lblPleaseSelect: TLabel;
    pnlExplain: TPanel;
    lblExplain: TLabel;
    procedure rbSQLClick(Sender: TObject);
  private
    procedure InternalValidate(ASettings: TSettings;
      var ACanProceed: boolean; AShowMessage: boolean);
  public
    procedure ApplySettings(ASettings: TSettings); override;
    procedure Validate(ASettings: TSettings; var ACanProceed: boolean); override;
    function CreateNextFrame(AOwner: TComponent): TBaseFrame; override;
    procedure Execute(ASettings: TSettings); override;    
  end;
  
//==============================================================================

implementation

{$R *.dfm}

uses ADODB;

{-==============================================================================
    TfraLogin
===============================================================================}
{-------------------------------------------------------------------------------
  Applies the user's login information to the Settings object.
}
procedure TfraLogin.ApplySettings(ASettings: TSettings);
begin
  ASettings.TrustedSecurity := rbTrusted.Checked;
  if not rbTrusted.Checked then begin
    ASettings.Username := eUsername.Text;
    ASettings.Password := ePassword.Text;
  end;
  rbSQLClick(nil);
end;  // TfraLogin.ApplySettings

(**
 * Validate the login info provided before proceeding.
 *)
procedure TfraLogin.Validate(ASettings: TSettings; var ACanProceed: boolean);
begin
  InternalValidate(ASettings, ACanProceed, true);
end;

(**
 * Validates that the login info provided works. If so, then sets ACanProceed to
 * true. If not, then shows a warning message if AShowMessage is true.
 *)
procedure TfraLogin.InternalValidate(ASettings: TSettings; var ACanProceed: boolean; AShowMessage: boolean);
var
  testconnect: TADOConnection;
  rs: _Recordset;
begin
  ApplySettings(ASettings);
  testconnect := TADOConnection.Create(nil);
  try
    try
      testconnect.ConnectionString := ASettings.GetConnectionString;
      // connect or raise an exception
      testconnect.Connected := True;
    except
      on E: Exception do begin
        if AShowMessage then
          MessageDlg(Format(ResStr_CannotConnect, [E.Message]), mtError, [mbOk], 0);
        ACanProceed := false;
        exit;
      end;
    end;
    rs := testconnect.Execute('SELECT IS_MEMBER(''dbo'')', cmdText);
    if rs.Fields[0].Value=0 then begin
      if AShowMessage then
        MessageDlg(ResStr_InsufficientRights, mtError, [mbOk], 0);
      ACanProceed := false;
    end;
  finally
    testconnect.free;
  end;
end;

{-------------------------------------------------------------------------------
}
function TfraLogin.CreateNextFrame(AOwner: TComponent): TBaseFrame;
begin
  Result := TfraKeyCheck.Create(AOwner);
end;  // TfraLogin.CreateNextFrame

{-------------------------------------------------------------------------------
}
procedure TfraLogin.rbSQLClick(Sender: TObject);
begin
  eUsername.Enabled := rbSQL.Checked;
  ePassword.Enabled := rbSQL.Checked;
end;  // TfraLogin.rbSQLClick

(**
 * If using trusted login according to InstallSettings, then test that this login
 * works. If so no need to ask for a login.
 *)
procedure TfraLogin.Execute(ASettings: TSettings);
var
  settingsFile: TStringList;
  canProceed: Boolean;
begin
  if FileExists(ASettings.InstallationPath + 'InstallSettings.ini') then begin
    settingsFile := TStringList.Create;
    try
      settingsFile.LoadFromFile(ASettings.InstallationPath + 'InstallSettings.ini');
      if settingsFile.Values['Trusted Security']='1' then begin
        rbTrusted.Checked := True;
        canProceed := true;
        InternalValidate(ASettings, canProceed, false);
        if canProceed then begin
          // The installation was done with trusted authentication and this login method
          // works, so no need to ask for further login details.
          pnlLogin.Visible := false;
          pnlExplain.Visible := true;
          lblExplain.Caption := ASettings.IntroMessage;
          lblExplain.Width := pnlExplain.Width - lblExplain.Left * 2;
          lblTitle.Caption := Application.Title;
        end;
      end;
    finally
      settingsFile.Free;
    end;
  end;
end;


end.
