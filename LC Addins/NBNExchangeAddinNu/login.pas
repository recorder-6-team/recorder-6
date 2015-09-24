unit login;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, Registry;

resourcestring
  ResStr_BadInstall = 'Recorder is not correclty installed on this machine. Installation of this addin cannot proceed.';

type
  TformLogin = class(TForm)
    StaticText: TStaticText;
    rbTrusted: TRadioButton;
    rbSQL: TRadioButton;
    edUsername: TEdit;
    edPassword: TEdit;
    labelUserName: TLabel;
    labelPassword: TLabel;
    Bevel1: TBevel;
    bLogin: TButton;
    bCancel: TButton;
    procedure rbTrustedClick(Sender: TObject);
    procedure rbSQLClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure bLoginClick(Sender: TObject);
  private
    { Private declarations }
    FConnectionString: ansistring;
    FServerName: ansistring;
    FDatabaseName: ansistring;
    procedure ReadRegistry;
    function GetConnectionString: ansistring;
  public
    { Public declarations }
    property ConnectionString: ansistring read FConnectionString;
  end;

var
  formLogin: TformLogin;

implementation

{$R *.dfm}
const
  // Connection String for main SQL Server DB
  CONNECTION_STRING = 'Provider=SQLOLEDB.1;%sPersist Security Info=False;' +
                      'Data Source=%s;Initial Catalog=%s;OLE DB Services=-2';
  //OLE DB Services=-2 turns off connection pooling which can cause confusing errors
  //when sp_SetAppRole is used.
  INTEGRATED_SECURITY = 'Integrated Security=SSPI;';
  SQL_SECURITY = 'User ID=%s;password=%s;';
  REG_PATH = 'SOFTWARE\Dorset Software\Recorder 6';

procedure TformLogin.bLoginClick(Sender: TObject);
begin
  ReadRegistry;
  FConnectionString := GetConnectionString;
end;

procedure TformLogin.FormCreate(Sender: TObject);
begin
  FConnectionString := '';
  FServerName := '';
  FDatabaseName := '';
  edPassword.Text := '';
  edUsername.Text := 'sa';
  rbTrusted.Checked := True;
  rbSQL.Checked := False;
  edUsername.Enabled := False;
  edPassword.Enabled := False;
end;

procedure TformLogin.rbSQLClick(Sender: TObject);
begin
  rbTrusted.Checked := False;
  edUsername.Enabled := True;
  edPassword.Enabled := True;
end;

procedure TformLogin.rbTrustedClick(Sender: TObject);
begin
  rbSQL.Checked := False;
  edUsername.Enabled := False;
  edPassword.Enabled := False;
end;

procedure TformLogin.ReadRegistry;
var
  lReg: TRegistry;
begin
  lReg := TRegistry.Create;
  try
    // Read off Lcoal Machine key.
    lReg.Rootkey := HKEY_LOCAL_MACHINE;
    if not lReg.OpenKeyReadOnly(REG_PATH) then begin
      MessageDlg(ResStr_BadInstall, mtError, [mbOK],0);
      Application.Terminate;
    end;
    FServerName := lReg.ReadString('Server Name');
    FDatabaseName := lReg.ReadString('Database Name');
  finally
    lReg.Free;
  end;
end;

function TformLogin.GetConnectionString: ansistring;
var
  lstSecurityInfo: ansistring;
begin
  if rbTrusted.Checked then
    lstSecurityInfo := INTEGRATED_SECURITY
  else
    lstSecurityInfo := Format(SQL_SECURITY, [edUserName.Text, edPassword.Text]);
  Result := Format(CONNECTION_STRING, [lstSecurityInfo, FServerName, FDatabaseName]);
end;

end.
