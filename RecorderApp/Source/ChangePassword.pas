//==============================================================================
//  Unit:        ChangePassword
//
//  Implements:  TdlgChangePassword
//
//  Description: Allows a user to change his/her password.
//
//  Created:
//
//  Last Revision Details:
//    $Revision: 17 $
//    $Date: 16/07/09 11:39 $
//    $Author: Ericsalmon $
//
//  Copyright © Dorset Software Services Ltd, 2002
//
//==============================================================================

unit ChangePassword;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ExtCtrls, ExceptionForm, OnlineHelp, ImageListButton;

type
  EChangePasswordError = class(TExceptionPath);

  TdlgChangePassword = class(TForm)
    Bevel1: TBevel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    lblUserName: TLabel;
    eOldPassword: TEdit;
    eNewPassword: TEdit;
    eConfirmPassword: TEdit;
    bbOK: TImageListButton;
    bbCancel: TImageListButton;
    procedure FormCreate(Sender: TObject);
    procedure bbOKClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    hlpPassword: TOnlineHelp;
  public
  end;

//==================================================================================================
implementation

{$R *.DFM}

uses
  GeneralData, ApplicationSettings, FormActions, StrUtils, DatabaseAccessADO, Constants;
  
resourcestring
  ResStr_NewMissing         = 'Please enter a new password';
  ResStr_ConfirmMissing     = 'Please confirm your password';
  ResStr_NewPassFail        = 'Your new password must be different from your old password';
  ResStr_ConfirmFail        = 'New password and confirmation do not match';
  ResStr_ChangePasswordFail = 'Unable to change the password';
  ResStr_LocateUserFail     = 'Unable to locate the current user in the database';
  ResStr_WrongPassword      = 'The current password is incorrect';
  ResStr_PassLength         = 'The new password must be between 3 and 20 characters';
  ResStr_NoSpace            = 'The password must not end in a space.';
  ResStr_SuccessfulChange   = 'Your password has been changed successfully';

//==================================================================================================
procedure TdlgChangePassword.FormCreate(Sender: TObject);
begin
  lblUserName.Caption:= dmGeneralData.GetIndividualName(AppSettings.UserID);

  //Help Setup
  hlpPassword := TOnlineHelp.Create(Self.Handle);
  Onhelp      := hlpPassword.OnHelpReplacement;
  HelpContext := IDH_PASSWORD;
end; 

//==================================================================================================
procedure TdlgChangePassword.bbOKClick(Sender: TObject);
var
  oldPwd: String;
begin
  inherited;

  oldPwd := IfThen(eOldPassword.Text = '', DEFAULT_USER_PASSWORD, eOldPassword.Text);

  // Don't close the dialog if something's not right
  ModalResult := mrNone;
  with dmGeneralData.qryAllPurpose do
  begin
    // Force connection as this could be cause of IR 7552
    Connection := dmDatabase.dbLocal;
    //Find user in users table
    SQL.Text := 'SELECT * FROM [User] ' +
                'WHERE Name_Key = ''' + AppSettings.UserID + ''' ';
    try
      ParseSQL := False;
      try
        Open;
        if Eof then
          Raise EChangePasswordError.Create(ResStr_LocateUserFail);

        //Validate the current password
        if oldPwd <> FieldByName('PASSWORD').AsString then
          Raise EChangePasswordError.CreateValidation(ResStr_WrongPassword, eOldPassword);

        if eNewPassword.Text = '' then
          Raise EChangePasswordError.CreateValidation(ResStr_NewMissing, eNewPassword);

        if (Length(eNewPassword.Text) < 3) or (Length(eNewPassword.Text) > 20) then
          Raise EChangePasswordError.CreateValidation(ResStr_PassLength, eNewPassword);

        if eOldPassword.Text = eNewPassword.Text then
          Raise EChangePasswordError.CreateValidation(ResStr_NewPassFail, eNewPassword);

        //May not end in a space because this confuses things in the database.
        if RightStr(eNewPassword.Text, 1) = ' ' then
          Raise EChangePasswordError.CreateValidation(ResStr_NoSpace, eNewPassword);

        if eConfirmPassword.Text = '' then
          Raise EChangePasswordError.CreateValidation(ResStr_ConfirmMissing, eConfirmPassword);

        if eNewPassword.Text <> eConfirmPassword.Text then
          Raise EChangePasswordError.CreateValidation(ResStr_ConfirmFail, eConfirmPassword);

        //Change password
        try
          Edit;
          FieldByName('PASSWORD').AsString:= eNewPassword.Text;
          Post;
          Close;
        except on E:Exception do
          Raise EChangePasswordError.Create(ResStr_ChangePasswordFail, E);
        end;
      finally
        Close;
      end;
    finally
      ParseSQL := true;
    end;
	end;

  //If we have got this far without an exception, we can close the dialog
  MessageDlg(  ResStr_SuccessfulChange, mtInformation, [mbOK], 0);
  ModalResult:= mrOK;
end;

//==================================================================================================
procedure TdlgChangePassword.FormDestroy(Sender: TObject);
begin
  hlpPassword.Free;
end;

end.
