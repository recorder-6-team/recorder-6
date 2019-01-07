//==============================================================================
//  Unit:        Login
//
//  Implements:  TdlgLogin
//
//  Description: Implements the login dialog and validates the username/password
//
//  Author:      John van Breda
//  Created:     8 April 1999
//
//  Changes:     Eric Salmon - 09/04/2002
//               Validate user login using query instead of table, so that the
//               SQL statement can be intercepted and validated for
//               SQL Server/Oracle.
//
//  Last Revision Details:
//    $Revision: 42 $
//    $Date: 30/07/09 16:00 $
//    $Author: Ericsalmon $
//
//  Copyright © Dorset Software Services Ltd, 2001
//
//==============================================================================

unit Login;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, Variants,
  StdCtrls, Buttons, BaseFormUnit, ExtCtrls, DBListCombo, Constants, apiutils, DataClasses,
  StrUtils;

const
  WM_BringToFront = WM_APP + 121;

type
  TdlgLogin = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    ePassword: TEdit;
    bbOk: TBitBtn;
    bbCancel: TBitBtn;
    Bevel1: TBevel;
    dblcUsers: TDBListCombo;
    lblPasswordInstruct: TLabel;
    procedure bbOkClick(Sender: TObject);
    procedure dblcUsersChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FBypassLogin: Boolean;
    FCancelledFirstRun: Boolean;
    procedure CheckForFirstRunAndBypass;
    function GetLastUserID: String;
    procedure SetPreviouslyUsed(const Value: Boolean);
    procedure WMBringToFront(var Message: TMessage); message WM_BringToFront;
    property PreviouslyUsed: Boolean write SetPreviouslyUsed;
  public
    constructor Create(AOwner: TComponent); override;
    property BypassLogin: Boolean read FBypassLogin;
    property CancelledFirstRun: Boolean read FCancelledFirstRun;
  end;

var
  dlgLogin: TdlgLogin;

//==============================================================================
implementation

{$R *.DFM}
uses
  ApplicationSettings, Registry, DatabaseAccessADO, ADOInt, GeneralData, FirstRun;

resourcestring
  ResStr_LocateUserFail =
      'Unable to find the selected user in the database.  '#13
      + 'The user may have been deleted.';
  ResStr_NoUsers        = 'There are no users in the database.  The application cannot start.';
  ResStr_WrongPassword  = 'The password you entered is not correct.';
  ResStr_IndexNeedsRebuilding = 'The database index tables need rebuilding otherwise the application ' +
    'will not function correctly. Please select the Rebuild ... Index options from the Tools > Database Tools ' +
    'menu to rebuild them';
  ResStr_PasswordInstruct =
      'Hint : This is the first time you have logged into Recorder. '
      + 'If you set up your password during installation then use the password you gave. '
      + 'Otherwise enter "password" as your password or you can leave the password blank and click OK to login. '
      + 'To change your password you must first login. You can then select the '
      + 'Change Password option from the Tools menu to update your password.';
{-------------------------------------------------------------------------------
}
constructor TdlgLogin.Create;
var lIndex: _Recordset;
begin
  inherited;
  Caption := Format(Caption, [Application.Title]);
  dblcUsers.Active := True;
  // Check to see if the index tables are poulated and if not warns
  lIndex := dmDatabase.GetRecordset('usp_Check_Index',[]);
  if lIndex.eof then
    MessageDlg(ResStr_IndexNeedsRebuilding, mtWarning, [mbOk], 0);
  lIndex.Close;
  // End of check index
  CheckForFirstRunAndBypass;
  if not (BypassLogin or CancelledFirstRun) then
  begin
    //Set the selected user to the last logged-in user
    dblcUsers.KeyValue := GetLastUserID;
    //Show the password
    dblcUsersChange(nil);
    bbOK.Enabled := (dblcUsers.Text <> '');
  end;

  {$IFDEF DEBUG}
  Color := clBlue;
  {$ENDIF} // warning colour for compiler directive
end;  // Create

{-------------------------------------------------------------------------------
  Returns the NAME_KEY of the last logged on user on this login, or '' if none.
}
function TdlgLogin.GetLastUserID: String;
begin
  Result := ''; // default
  with TRegistry.Create do
    try
      RootKey := HKEY_CURRENT_USER;
      if OpenKeyReadOnly(REG_KEY_SETTINGS) then
      begin
        if ValueExists('Last User') then
          Result := ReadString('Last User');
        CloseKey;
      end;
    finally
      Free;
    end;

  // Nothing in registry, try to get Default User from database
  if Result = '' then
    with dmDatabase.GetRecordset('usp_DefaultUser_Get_Key',[]) do
      if not Eof then
        Result := Fields['Name_Key'].Value;
end;

{-------------------------------------------------------------------------------
}
procedure TdlgLogin.bbOkClick(Sender: TObject);
var
  lRS: _Recordset;
begin
  lRS := dmDatabase.GetRecordset('usp_User_Select', ['@NameKey', dblcUsers.KeyValue]);
  // In case User was deleted when nobody was looking...
  if lRS.Eof then
    MessageDlg(ResStr_LocateUserFail, mtWarning, [mbOk], 0)
  else begin
    //Check password
    lRS := dmDatabase.GetRecordset(
        'usp_User_Select_ForLogin',
        ['@NameKey', dblcUsers.KeyValue,
         '@Password', IfThen(ePassword.Text = '', DEFAULT_USER_PASSWORD, ePassword.Text)]);
    if lRS.Eof then
    begin
      MessageDlg(ResStr_WrongPassword, mtWarning, [mbOk], 0);
      ePassword.SetFocus;
      ePassword.SelectAll;
    end else begin
      //Set application wide variables
      // Read FullEditOwnData value first, or we get an error. Why? Beats me!
      AppSettings.RestrictFullEdit     := lRS.Fields['Full_Edit_Own_Data'].Value;
      AppSettings.LoginUserAccessLevel := TUserAccessLevel(lRS.Fields['Security_Level'].Value - 1);
      AppSettings.UserID               := dblcUsers.KeyValue;
      { TODO : This should be handled by upgrader to SQL Server. }

      dmGeneralData.CheckMapTables;
      dmDatabase.RunStoredProc('usp_User_Update_FirstLogin', ['@NameKey', dblcUsers.KeyValue]);
      ModalResult := mrOK;
    end;
  end;
end;  // bbOkClick

{-------------------------------------------------------------------------------
}
procedure TdlgLogin.dblcUsersChange(Sender: TObject);
begin
  {Show lblPasswordInstruct if a user is shown who hasn't logged in before.}
  with dmDatabase.GetRecordset('usp_User_Select', ['@NameKey', dblcUsers.KeyValue]) do
    if not EOF then
      PreviouslyUsed := Fields['First_Login'].Value = 0
    else
      PreviouslyUsed := True;
  bbOK.Enabled:= (dblcUsers.Text <> '');
end;  // dblcUsersChange

{-------------------------------------------------------------------------------
}
procedure TdlgLogin.WMBringToFront(var Message: TMessage);
begin
  SetForegroundWindow(Handle);
end;  // WMBringToFront

{-------------------------------------------------------------------------------
  When the form is showed, it will not necessarily be the foreground window
  due to the splash screen.  Therefore, we force it to the front - but we
  must wait till after the show message to do this
}
procedure TdlgLogin.FormShow(Sender: TObject);
begin
  PostMessage(Handle, WM_BRINGTOFRONT, 0, 0);
end;  // FormShow

{-------------------------------------------------------------------------------
  This hides or shows lblPasswordInstruct
}
procedure TdlgLogin.SetPreviouslyUsed(const Value: Boolean);
begin
 if Value then
   ClientHeight := bbOk.Top + bbOk.Height + 8
 else begin
   lblPasswordInstruct.Caption := Format(ResStr_PasswordInstruct, [Application.Title]);
   ClientHeight := lblPasswordInstruct.Top + lblPasswordInstruct.Height + 8;
 end;
end;  // SetPreviouslyUsed

{-------------------------------------------------------------------------------
  Checks the number of users and the passwords to determine whether to bypass
  the login screen or display the "Run First Time" screen instead.
}
procedure TdlgLogin.CheckForFirstRunAndBypass;
var
  rs: _Recordset;
  allDefaultPwd, isFirstRun: Boolean;
  defaultUser, otherUser: TKeyString;
  userCount, adminUserCount: integer;
begin
  defaultUser := '';
  isFirstRun  := False;

  // Try to get the Default User key.
  rs := dmDatabase.GetRecordset('usp_DefaultUser_Get_Key',[]);
  if not rs.Eof then
    defaultUser := rs.Fields['Name_Key'].Value;
  // Get the users who are not default users and have a security level of 5
  rs := dmDatabase.GetRecordset('usp_Users_Select_Admin', ['@default_User_key', DefaultUser ]);
  adminUserCount := rs.RecordCount;
  // Check how many users there are.
  rs := dmDatabase.GetRecordset('usp_Users_Select', []);
  userCount := rs.RecordCount;
  // No records, or Default User is the only record found. Show First Run screen.
  if ((defaultUser <> '') and (userCount = 1)) or (userCount = 0) then
  begin
    with TdlgFirstRun.Create(nil) do
      try
        if ShowModal = mrOk then
        begin
          otherUser    := NewUserKey;
          FBypassLogin := True;
          isFirstRun   := True;
        end else
          FCancelledFirstRun := True;
      finally
        Free;
      end;
  end else
  // Only 2 records, with Default User being one of them, check both for default password.
  if (defaultUser <> '') and (userCount = 2) then
  begin
    allDefaultPwd := True;
    while not rs.Eof do
    begin
      allDefaultPwd := allDefaultPwd and (rs.Fields['Password'].Value = DEFAULT_USER_PASSWORD);
      // Grab other user's key to set application wide variables.
      if rs.Fields['Name_Key'].Value <> defaultUser then
        otherUser := rs.Fields['Name_Key'].Value;
      rs.MoveNext;
    end;
    FBypassLogin := allDefaultPwd;
  end;
  // If only two records and one is the default, but there are no other admin users then
  // We need to show the login screen
  if (defaultUser <> '') and (userCount = 2) and (adminUserCount = 0) then
    FBypassLogin := false;

  if FBypassLogin then
  begin
    rs := dmDatabase.GetRecordset('usp_User_Select', ['@NameKey', otherUser]);
    AppSettings.RestrictFullEdit     := rs.Fields['Full_Edit_Own_Data'].Value;
    AppSettings.LoginUserAccessLevel := TUserAccessLevel(rs.Fields['Security_Level'].Value - 1);
    AppSettings.UserID               := otherUser;
    dmGeneralData.CheckMapTables;
    if not isFirstRun then
      dmDatabase.RunStoredProc('usp_User_Update_FirstLogin', ['@NameKey', otherUser]);
  end;
end;

//==============================================================================
end.
