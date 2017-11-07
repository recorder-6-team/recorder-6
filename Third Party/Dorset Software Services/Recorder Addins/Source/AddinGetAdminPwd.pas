{===============================================================================
  Unit:        AddinGetAdminPwd

  Defines:     TfrmGetAdminPwd

  Description: Login box to retrieve the sa password required to install the
               addin.  Set the Connection then call ShowModal to obtain
               sa privileges on the connection

  Created:     May 2004

  Last revision information:
    $Revision: 1 $
    $Date: 25/05/04 15:25 $
    $Author: Johnvanbreda $

===============================================================================}
unit AddinGetAdminPwd;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Mask, ImageListButton, ExtCtrls, ADODb, ExceptionForm;

type
  EGetAdminPwd = class(TExceptionPath)
  end;
  
  {-----------------------------------------------------------------------------
    Dialog that automates obtaining the sa login details and setting a connection to use the
    details.
  }
  TdlgGetAdminPwd = class(TForm)
    Bevel1: TBevel;
    btnCancel: TImageListButton;
    btnOK: TImageListButton;
    chkTrustedLogin: TCheckBox;
    ePassword: TMaskEdit;
    eUsername: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    procedure btnCancelClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
  private
    FConnection: TADOConnection;
    function GetConnectionString(const AUserName, APassword: string; ATrusted: boolean):
        String;
    function GetPassword: String;
    function GetTrustedLogin: Boolean;
    function GetUsername: String;
    procedure SetConnection(const Value: TADOConnection);
    function TestConnectionIsDBO: Boolean;
  public
    property Connection: TADOConnection read FConnection write SetConnection;
    property Password: String read GetPassword;
    property TrustedLogin: Boolean read GetTrustedLogin;
    property Username: String read GetUsername;
  end;
  
implementation

{$R *.dfm}

uses
  ComObj, AddinResourceStrings, GeneralFunctions, Registry, AddinConstants;

{-==============================================================================
    TdlgGetAdminPwd
===============================================================================}
{-------------------------------------------------------------------------------
}
procedure TdlgGetAdminPwd.btnCancelClick(Sender: TObject);
begin
  raise EGetAdminPwd.Create(ResStr_InstallationCancelled);
end;  // TdlgGetAdminPwd.btnCancelClick 

{-------------------------------------------------------------------------------
}
procedure TdlgGetAdminPwd.btnOKClick(Sender: TObject);
begin
  FConnection.Close;
  FConnection.LoginPrompt := False;
  FConnection.ConnectionString := GetConnectionString(UserName, Password, TrustedLogin);
  try
    FConnection.Open;
    if not TestConnectionIsDBO then
      ModalResult := mrNone;
  except
    on E:EOleException do begin
      ShowInformation(ResStr_InvalidLogin);
      ModalResult := mrNone;
    end;
  end;
end;  // TdlgGetAdminPwd.btnOKClick 

{-------------------------------------------------------------------------------
}
function TdlgGetAdminPwd.GetConnectionString(const AUserName, APassword: string; ATrusted:
    boolean): String;
var
  lServerName, lDbName, lSecurity: String;
begin
  with TRegistry.Create do
    try
      RootKey := HKEY_LOCAL_MACHINE;
      if OpenKeyReadOnly(REG_KEY_RECORDER) then begin
        if ValueExists('Server Name') then begin
          lServerName := ReadString('Server Name');
          lDbName     := ReadString('Database Name');
          if ATrusted then
            lSecurity := 'Integrated Security=SSPI;'
          else
            lSecurity := Format('User ID=%s;password=%s;', [AUserName, APassword]);
  
          Result := Format('Provider=SQLOLEDB.1;%sPersist Security Info=False;' +
                    'Data Source=%s;Initial Catalog=%s;OLE DB Services=-2',
                    [lSecurity, lServerName, lDbName]);
        end;
        CloseKey;
      end;
    finally
      Free;
    end;
end;  // TdlgGetAdminPwd.GetConnectionString 

{-------------------------------------------------------------------------------
}
function TdlgGetAdminPwd.GetPassword: String;
begin
  Result := ePassword.Text;
end;  // TdlgGetAdminPwd.GetPassword 

{-------------------------------------------------------------------------------
}
function TdlgGetAdminPwd.GetTrustedLogin: Boolean;
begin
  Result := chkTrustedLogin.Checked;
end;  // TdlgGetAdminPwd.GetTrustedLogin 

{-------------------------------------------------------------------------------
}
function TdlgGetAdminPwd.GetUsername: String;
begin
  Result := eUserName.Text;
end;  // TdlgGetAdminPwd.GetUsername 

{-------------------------------------------------------------------------------
}
procedure TdlgGetAdminPwd.SetConnection(const Value: TADOConnection);
begin
  FConnection := Value;
end;  // TdlgGetAdminPwd.SetConnection 

{-------------------------------------------------------------------------------
}
function TdlgGetAdminPwd.TestConnectionIsDBO: Boolean;
begin
  // Test the connection is dbo
  with TADOCommand.Create(nil) do
    try
      Connection := FConnection;
      CommandType := cmdUnknown;
      CommandTimeout := 0;
      CommandText := 'SELECT User_Name() AS User_Name';
      if Execute.Fields['User_Name'].Value='dbo' then
        Result := True
      else begin
        Result := False;
        ShowInformation(ResStr_LoginNotDBO);
      end;
    finally
      Free;
    end;
end;  // TdlgGetAdminPwd.TestConnectionIsDBO 



end.
