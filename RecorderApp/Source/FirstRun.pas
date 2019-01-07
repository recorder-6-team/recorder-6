//==============================================================================
//  Unit:        FirstRun
//
//  Implements:  TdlgFirstRun
//
//  Description: Implements the FirstRun dialog.
//
//  Created:     July 2009
//
//  Last Revision Details:
//    $Revision: 3 $
//    $Date: 30/07/09 15:01 $
//    $Author: Ericsalmon $
//
//  Copyright © Dorset Software Services Ltd, 2001
//
//==============================================================================

unit FirstRun;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls,
  Buttons, ExtCtrls, DataClasses, StrUtils, ExceptionForm;

resourcestring
  ResStr_SurnameMissing =
      'Please enter your Surname.';

  ResStr_NoSpace =
      'The password must not end in a space.';
  ResStr_PasswordLength =
      'The password must be between 3 and 20 characters long. Leave it blank if you wish '
      + 'to use the default ''%s''.';
  ResStr_PasswordsMismatch =
      'The password and confirmation do not match. Leave them both blank if you wish '
      + 'to use the default ''%s''.';
  ResStr_SurveyNameMissing =
      'Please enter a Survey Name or uncheck the tick box.';

  ResStr_ErrorsInSurveyTemplate =
      'Errors have been detected in the provided ''Survey Template.ini'' file: %s'
      + #10#10'The survey cannot be created.';

  ResStr_InvalidSurveyDate =
      '- The date specified for the AllowRecordsFrom value is not a valid vague date.';
  ResStr_InvalidRunByNameKey =
      '- The key specified for the RunByKey value cannot be matched to any '
      + 'existing record in the database.';
  ResStr_InvalidSurveyMediaKey =
      '- The key specified for the SurveyMediaKey value cannot be matched to any '
      + 'existing record in the database.';
  ResStr_InvalidSurveyTypeKey =
      '- The key specified for the SurveyTypeKey value cannot be matched to any '
      + 'existing record in the database.';

type
  ESurveyTemplateException = class(TExceptionPath)
  end;

  TdlgFirstRun = class(TForm)
    bvlFrame: TBevel;
    lblUsernameDesc: TLabel;
    lblFirstname: TLabel;
    eFirstname: TEdit;
    lblSurname: TLabel;
    eSurname: TEdit;
    lblPassword: TLabel;
    ePassword: TEdit;
    lblConfirmPassword: TLabel;
    eConfirmPassword: TEdit;
    chkCreateSurvey: TCheckBox;
    lblSurveyName: TLabel;
    eSurveyName: TEdit;
    chkAcceptTerms: TCheckBox;
    lblTerms: TLabel;
    mmTerms: TMemo;
    btnOk: TBitBtn;
    btnCancel: TBitBtn;
    procedure chkCreateSurveyClick(Sender: TObject);
    procedure chkAcceptTermsClick(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
  private
    FNewUserKey: TKeyString;
    procedure CreateSurveyFromTemplate;
    function Validate: Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    property NewUserKey: TKeyString read FNewUserKey;
  end;

//==============================================================================
implementation

{$R *.DFM}

uses
  GeneralFunctions, DatabaseAccessADO, Constants, VagueDate, BaseADODataModule,
  ApplicationSettings;

{-------------------------------------------------------------------------------
}
constructor TdlgFirstRun.Create(AOwner: TComponent);
begin
  inherited;

  if not FileExists(ExtractFilePath(Application.ExeName) + 'Help\TermsAndConditions.txt') then
  begin
    lblTerms.Visible       := False;
    mmTerms.Visible        := False;
    chkAcceptTerms.Visible := False;
    bvlFrame.Height        := lblTerms.Top - 8;
    btnOk.Top              := lblTerms.Top + 4;
    btnCancel.Top          := lblTerms.Top + 4;
    ClientHeight           := btnOk.Top + btnOk.Height + 8;
    btnOk.Enabled          := True;
  end else
    mmTerms.Lines.LoadFromFile(ExtractFilePath(Application.ExeName) + 'Help\TermsAndConditions.txt');
end;  // Create

{-------------------------------------------------------------------------------
}
procedure TdlgFirstRun.chkCreateSurveyClick(Sender: TObject);
begin
  lblSurveyName.Enabled := chkCreateSurvey.Checked;
  eSurveyName.Enabled   := chkCreateSurvey.Checked;
end;  // chkCreateSurveyClick

{-------------------------------------------------------------------------------
}
procedure TdlgFirstRun.chkAcceptTermsClick(Sender: TObject);
begin
  btnOk.Enabled := chkAcceptTerms.Checked;
end;  // chkAcceptTermsClick

{-------------------------------------------------------------------------------
}
procedure TdlgFirstRun.btnOkClick(Sender: TObject);
begin
  if Validate then
  begin
    // Need to enable the correct application role before anythign else.
    AppSettings.LoginUserAccessLevel := ualAdmin;

    // Create Individual and Name records.
    FNewUserKey := dmDatabase.RunInsertStoredProc(
        TN_INDIVIDUAL,
        'usp_Individual_Insert',
        ['@Forename',  eFirstname.Text,
         '@Surname',   eSurname.Text,
         '@EnteredBy', ''],
        '@Key');

    // Setup user record. FirstLogin set to true by default.
    dmDatabase.RunStoredProc(
        'usp_User_Insert',
        ['@NameKey',         FNewUserKey,
         '@Password',        DEFAULT_USER_PASSWORD,
         '@SecurityLevel',   Integer(ualAdmin) + 1,
         '@FullEditOwnData', 0]);

    // Update password to user's own if needed.
    if ePassword.Text <> '' then
      dmDatabase.RunUpdateStoredProc(
          'usp_User_Update_Password',
          ['@NameKey',  FNewUserKey,
           '@Password', ePassword.Text]);

    if chkCreateSurvey.Checked then
      CreateSurveyFromTemplate;

    AppSettings.RestrictFullEdit := False;
    AppSettings.UserID           := FNewUserKey;

    ModalResult := mrOk;
  end;
end;  // btnOkClick

{-------------------------------------------------------------------------------
}
function TdlgFirstRun.Validate: Boolean;
begin
  Result := False;
  if Trim(eSurname.Text) = '' then
  begin
    ShowInformation(ResStr_SurnameMissing);
    eSurname.SetFocus;
  end else
  if RightStr(ePassword.Text, 1) = ' ' then
  begin
    ShowInformation(ResStr_NoSpace);
    ePassword.SetFocus;
  end else
  if (ePassword.Text <> '') and not (Length(ePassword.Text) in [3..20]) then
  begin
    ShowInformation(Format(ResStr_PasswordLength, [DEFAULT_USER_PASSWORD]));
    ePassword.SetFocus;
  end else
  if ePassword.Text <> eConfirmPassword.Text then
  begin
    ShowInformation(Format(ResStr_PasswordsMismatch, [DEFAULT_USER_PASSWORD]));
    ePassword.SetFocus;
  end else
  if chkCreateSurvey.Checked and (eSurveyName.Text = '') then
  begin
    ShowInformation(ResStr_SurveyNameMissing);
    eSurveyName.SetFocus;
  end else
    Result := True;
end;  // Validate

{-------------------------------------------------------------------------------
  Attempts to create a survey based on the values read from a template file.
  If any of the values read are invalid, a message warns the user the survey
  could not be created. It doesn't stop Recorder starting though.
}
procedure TdlgFirstRun.CreateSurveyFromTemplate;
var
  template: TStringList;
  dateFrom: TVagueDate;
  errors: String;
  description, surveyMediaKey, surveyTypeKey,surveyLicenceKey: String;
  allowedFrom, geoCoverage, runByKey: String;

begin
  errors := '';

  // Setup default values for first survey.
  description    := '';
  surveyMediaKey := NONE_RECORD_KEY;
  surveyTypeKey  := NONE_RECORD_KEY;
  surveyLicenceKey :=  NONE_RECORD_KEY;
  allowedFrom    := '01/01/2000';
  geoCoverage    := 'United Kingdom';
  runByKey       := '<current user>';

  // If "Survey Template.ini" exists, use it to override default values.
  if FileExists(ExtractFilePath(Application.ExeName) + 'Help\Survey Template.ini') then
  begin
    template := TStringList.Create;
    try
      template.CaseSensitive := False;
      template.LoadFromFile(ExtractFilePath(Application.ExeName) + 'Help\Survey Template.ini');

      description := template.Values['Description'];
      if template.Values['SurveyMediaKey'] <> '' then
        surveyMediaKey := template.Values['SurveyMediaKey'];
      if template.Values['SurveyTypeKey'] <> '' then
        surveyTypeKey := template.Values['SurveyTypeKey'];
      if template.Values['AllowRecordsFrom'] <> '' then
        allowedFrom := template.Values['AllowRecordsFrom'];
      if template.Values['GeographicCoverage'] <> '' then
        geoCoverage := template.Values['GeographicCoverage'];
      if template.Values['RunByKey'] <> '' then
        runByKey := template.Values['RunByKey'];
    finally
      template.Free;
    end
  end;

  // Check all values, in case there are some invalid ones.
  // Check the date validity.
  if SameText(allowedFrom, '<current date>') then
    dateFrom := DateToVagueDate(Date)
  else
  if IsVagueDate(allowedFrom) then
    dateFrom := StringToVagueDate(allowedFrom)
  else
    errors := errors + #10 + ResStr_InvalidSurveyDate;

  // Check the RunByKey is valid.
  if SameText(runByKey, '<current user>') then
    runByKey := FNewUserKey
  else
  if dmDatabase.GetStoredProcOutputParam('usp_Name_Get', ['@Key', runByKey], '@Caption') = '' then
    errors := errors + #10 + ResStr_InvalidRunByNameKey;

  // Check the SurveyMediaKey is valid.
  if dmDatabase.GetRecordset('usp_SurveyMedia_Select', ['@Key', surveyMediaKey]).EOF then
    errors := errors + #10 + ResStr_InvalidSurveyMediaKey;

  // Check the SurveyTypeKey is valid.
  if dmDatabase.GetRecordset('usp_SurveyType_Select', ['@Key', surveyTypeKey]).EOF then
    errors := errors + #10 + ResStr_InvalidSurveyTypeKey;

  // All good, proceed.
  if errors = '' then
    dmDatabase.RunInsertStoredProc(
        TN_SURVEY,
        'usp_Survey_Insert',
        ['@ItemName',           eSurveyName.Text,
         '@Description',        description,
         '@RunByKey',           runByKey,
         '@SurveyMediaKey',     surveyMediaKey,
         '@SurveyTypeKey',      surveyTypeKey,
         '@LicenceKey',         surveyLicenceKey,
         '@FromVagueDateStart', dateFrom.StartDate,
         '@FromVagueDateEnd',   dateFrom.EndDate,
         '@FromVagueDateType',  dateFrom.DateTypeString,
         '@GeographicCoverage', geoCoverage,
         '@EnteredBy',          FNewUserKey],
        '@Key')
  else
    MessageDlg(Format(ResStr_ErrorsInSurveyTemplate, [errors]), mtWarning, [mbOk], 0);
end;  // CreateSurveyFromTemplate

end.
