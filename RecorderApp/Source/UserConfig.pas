//==================================================================================================
//  Unit:        UserConfig
//
//  Implements:  TdlgUserConfig
//
//  Description: Screen to manage users, add/edit/delete users, and set access rigths
//
//  Author:      John van Breda
//  Created:     8 April 1999
//
//  Last Revision Details:
//    $Revision: 43 $
//    $Date: 16/07/09 11:41 $
//    $Author: Ericsalmon $
//
//  Copyright © Dorset Software Services Ltd, 2002
//
//==================================================================================================

unit UserConfig;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, 
  Grids, StdCtrls, Buttons, ExtCtrls, ComCtrls, BaseFormUnit, DataClasses, 
  DropStruct, DropTarget, ApplicationSettings, ExceptionForm, BaseChildUnit, Db, 
  OnlineHelp, Constants, ImageListButton, Menus, ComObj, CheckLst;

type
  EUserConfigError = class(TExceptionPath);

  TfrmUserConfig = class(TBaseChild)
    pnlControls: TPanel;
    bbUserAdd: TImageListButton;
    bbUserEdit: TImageListButton;
    bbUserDel: TImageListButton;
    pnlConfig: TPanel;
    spltUserOptions: TSplitter;
    pnlSettings: TPanel;
    Label4: TLabel;
    Label1: TLabel;
    Shape2: TShape;
    bbUserFind: TImageListButton;
    bbEditSave: TImageListButton;
    bbEditCancel: TImageListButton;
    eUserName: TEdit;
    cmbAccessLevel: TComboBox;
    lblSurvey: TLabel;
    bbSelectAll: TButton;
    bbUnselectAll: TButton;
    pnlUsers: TPanel;
    lvUsers: TListView;
    clbSurveys: TCheckListBox;
    procedure eUserNameKeyPress(Sender: TObject; var Key: Char);
    procedure lvUsersClick(Sender: TObject);
    procedure bbUserAddClick(Sender: TObject);
    procedure bbUserEditClick(Sender: TObject);
    procedure bbEditSaveClick(Sender: TObject);
    procedure bbEditCancelClick(Sender: TObject);
    procedure bbUserFindClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure bbUserDelClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure lvUsersChanging(Sender: TObject; Item: TListItem;
      Change: TItemChange; var AllowChange: Boolean);
    procedure clbSurveysOnClickCheck(Sender: TObject);
    procedure bbSelectAllClick(Sender: TObject);
    procedure bbUnselectAllClick(Sender: TObject);
    procedure lvUsersSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure clbSurveysMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
  private
    FNameKey    : TKeyString;
    FOldNameKey : TKeyString;
    FOriginalSurveys : Array of Boolean;
    procedure ClearUserList;
    procedure ClearSurveyList;
    procedure DropUserName(const Sender: TObject; const iFormat: integer;
      const iSourceData: TKeyList; const iTextStrings: TstringList; var ioHandled: boolean);
    procedure PopulateUserList;
    procedure PopulateSurveyList;
    procedure SetEditMode(const Value: TEditMode);
    procedure UserNameUpdate(KeyList: TKeyList);
    procedure WMTransferDone(var Msg:TMessage); message WM_TRANSFER_DONE;
    procedure SetCheckedAll(value: Boolean);
  protected
    procedure SetupDestinationControls; override;
    property EditMode: TEditMode read FEditMode write SetEditMode;
  public
    procedure ApplySecurity; override;
  end;

function UALToString(UAL: TUserAccessLevel; RestrictedFullEdit: Boolean = False): string;

//==============================================================================
implementation

{$R *.DFM}

uses
  FormActions, Find, Maintbar, IndOrg, GeneralData, DatabaseAccessADO;

resourcestring
  ResStr_LocateUserFail    =
      'Unable to find the selected user in the database.  The user may have been deleted.';
  ResStr_UserExists        =
      'The specified individual already has a user account.  To change the access rights '
      + 'for this user, please edit the existing account.';
  ResStr_CurrentUserEdit   = 'You cannot edit your own user account.';
  ResStr_CurrentUserDelete = 'You cannot delete your own user account.';
  ResStr_RemoveUser        = 'Are you sure you want to remove %s as a user?';
  ResStr_EnterValidName    = 'Please enter a valid name.';
  ResStr_InvalidName       =  'The name is invalid. You must enter a valid name.';

const
  // Indexes in Access Level combo box.
  ACC_READ_ONLY = 0;
  ACC_RECORDER  = 1;
  ACC_ADD_ONLY  = 2;
  ACC_FULL_OWN  = 3;
  ACC_FULL_EDIT = 4;
  ACC_ADMIN     = 5;

//==============================================================================
function UALToString(UAL: TUserAccessLevel; RestrictedFullEdit: Boolean = False): string;
begin
  case UAL of
    ualAdmin   : Result := ResStr_SystemManager;
    ualFullUser: if RestrictedFullEdit then Result := ResStr_FullEditOwn
                                        else Result := ResStr_FullEdit;
    ualAddOnly : Result := ResStr_AddOnly;
    ualRecorder: Result := ResStr_RecordCardsOnly;
    ualReadOnly: Result := ResStr_ReadOnly;
  else
    Result:= ResStr_Unknown;
  end;
end;  // UALToString

//==============================================================================
procedure TfrmUserConfig.FormCreate(Sender: TObject);
begin
  inherited;
  FNameKey    := '';
  FOldNameKey := '';
  EditMode    := emView;
  PopulateUserList;
  PopulateSurveyList;

  //Help Setup
  HelpContext := IDH_USERCONFIG;
end;  // FormCreate

//==============================================================================
procedure TfrmUserConfig.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  inherited;
  Action := caFree;
end;  // FormClose

//==============================================================================
procedure TfrmUserConfig.FormDestroy(Sender: TObject);
begin
  inherited;
  //Delete all TKeyData objects
  ClearUserList;
  ClearSurveyList;
end;  // FormDestroy

//==============================================================================
procedure TfrmUserConfig.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  inherited;
  CanClose:=false;
  if EditMode <> emView then
  begin
    Beep;
    case ConfirmSaveAndClose of
      mrYes : begin
        bbEditSaveClick(nil);
        CanClose:=true;
      end;
      mrNo  : begin
        bbEditCancelClick(nil);
        CanClose:=true;
      end;
    end;
  end
  else
    CanClose:=true;
end;  // FormCloseQuery

//==============================================================================
procedure TfrmUserConfig.ClearUserList;
var
  lIdx : integer;
begin
  with lvUsers do begin
    for lIdx := 0 to Items.Count - 1 do
      if Assigned(Items[lIdx].Data) then
        TKeyData(Items[lIdx].Data).Free;
    Items.Clear;
  end;
end;  // ClearUserList;

//==============================================================================
procedure TfrmUserConfig.PopulateUserList;
var
  NewItem  : TListItem;
  ItemData : TKeyData;
  lAccess  : String;
begin
  inherited;
  //Delete all TKeyData objects
  ClearUserList;
  //Add users to the list
  with dmGeneralData.qryUsers do
  begin
    Open;
    while not Eof do
    begin
      lAccess := UALToString(TUserAccessLevel(FieldByName('SECURITY_LEVEL').AsInteger - 1),
                             FieldByName('Full_Edit_Own_Data').AsBoolean);

      //Create listview item
      NewItem := lvUsers.Items.Add;
      NewItem.Caption := FieldByName('DISPLAY_NAME').AsString;
      NewItem.SubItems.Add(lAccess);

      //Create itemData
      ItemData := TKeyData.Create;
      ItemData.ItemKey        := FieldByName('NAME_KEY').AsString;
      ItemData.ItemAdditional := IntToStr(cmbAccessLevel.Items.IndexOf(lAccess));
      NewItem.Data := ItemData;

      //Select item if required
      if ItemData.ItemKey = FNameKey then
        lvUsers.Selected := NewItem;
      Next;
    end;
    Close;
  end;
  //Select the first user if one is not already selected
  if not Assigned(lvUsers.Selected) then
  begin
    lvUsers.Selected := lvUsers.Items[0];
    lvUsersClick(Self);
  end;
end;  // PopulateUserList 

{-------------------------------------------------------------------------------
  Completely empties the survey list.
}
procedure TfrmUserConfig.ClearSurveyList;
var
  i : integer;
begin
  with clbSurveys.Items do begin
    for i := 0 to Count - 1 do
      if Assigned(Objects[i]) then
        Objects[i].Free;
    Clear;
  end;
end;  // ClearSurveyList

{-------------------------------------------------------------------------------
  Populates the survey list with all the surveys in the database.
}
procedure TfrmUserConfig.PopulateSurveyList;
var
  itemData : TKeyData;
  lastSelected : String;
  currentIndex : integer;
  i : integer;
begin
  if clbSurveys.ItemIndex > -1 then
    lastSelected := TKeyData(clbSurveys.Items.Objects[clbSurveys.ItemIndex]).ItemKey
  else
    lastSelected := '';

  //Delete all TKeyData objects
  ClearSurveyList;

  if Assigned(lvUsers.Selected) then
  begin
    //Add surveys to the list
    with dmDatabase.GetRecordset('usp_Surveys_Select_ForUserAccess',
        ['@User_Name_Key', TKeyData(lvUsers.Selected.Data).ItemKey]) do
    begin
     if not Eof then
      begin
        MoveFirst;
        while not Eof do
        begin
          //Create itemData
          itemData := TKeyData.Create;
          itemData.ItemKey := Fields['Survey_Key'].Value;

          //Add the item.
          currentIndex :=
              clbSurveys.Items.AddObject(Fields['Display_Name'].Value, itemData);

          clbSurveys.Checked[currentIndex] := Fields['Allow_View'].Value;

          //If this was the last selected item, re-select it.
          if(lastSelected = itemData.ItemKey) then
            clbSurveys.Selected[currentIndex] := True;

          MoveNext;
        end;
      end;
      Close;
    end;

    SetLength(FOriginalSurveys, clbSurveys.Items.Count);
    for i := 0 to clbSurveys.Items.Count - 1 do
      FOriginalSurveys[i] := clbSurveys.Checked[i];
  end;
end; //PopulateSurveyList

//==============================================================================
procedure TfrmUserConfig.SetupDestinationControls;
begin
  RegisterDropComponent(eUserName, DropUserName, ['INDIVIDUAL'], [CF_JNCCDATA]);
end;  // SetupDestinationControls

//==============================================================================
procedure TfrmUserConfig.DropUserName(const Sender: TObject;
  const iFormat: integer; const iSourceData: TKeyList;
  const iTextStrings: TstringList; var ioHandled: boolean);
begin
  if Assigned(iSourceData) and (iSourceData.Header.ItemCount > 0) then
  begin
    //Set user name and FNameKey
    FNameKey:= iSourceData.Items[0].KeyField1;
    eUserName.Text:= dmGeneralData.GetIndividualName(FNameKey);
  end;
  ioHandled:= True;
end;  // DropComm

//==============================================================================
procedure TfrmUserConfig.UsernameUpdate(KeyList: TKeyList);
begin
  try
    if Assigned(KeyList) and (KeyList.Header.ItemCount > 0) then
    begin
      //Set user name and FNameKey
      FNameKey := KeyList.Items[0].KeyField1;
      eUserName.Text := dmGeneralData.GetIndividualName(FNameKey);
    end;
  finally
    KeyList.Free;
  end;
end;  // RecorderUpdate

//==============================================================================
procedure TfrmUserConfig.WMTransferDone(var Msg: TMessage);
begin
  Show;
end;  // WMTransferDone

//==============================================================================
procedure TfrmUserConfig.eUserNameKeyPress(Sender: TObject; var Key: Char);
begin
  inherited;
  // Get the find dialog if the enter key is pressed
  if Key = #13 then
  begin
    if not dmGeneralData.CheckIndividual(eUserName, FNameKey) then
      MessageDlg(ResStr_NoIndividualItems, mtInformation, [mbOK], 0);
    Key := #0;
  end;
end;

//==============================================================================
procedure TfrmUserConfig.bbUserFindClick(Sender: TObject);
begin
  inherited;
  dmFormActions.actNames.Execute;
  SetupLink(TBaseForm(frmMain.ActiveMDIChild), Self, UsernameUpdate);
end;  // bbUserFindClick

//==============================================================================
procedure TfrmUserConfig.lvUsersClick(Sender: TObject);
var
  i: Integer;
begin
  if EditMode = emView then
  begin
    //If no user is selected then attempt to select the previous user
    with lvUsers do begin
      if not Assigned(Selected) then
        for i:= 0 to Items.Count - 1 do
          if TKeyData(Items[i].Data).ItemKey = FNameKey then
            Selected:= Items[i];

      //If still no user is selected then select the first one
      if not Assigned(Selected) then
        Selected := Items[0];
    end;
  end;
end;  // lvUsersClick

//==============================================================================
procedure TfrmUserConfig.bbUserAddClick(Sender: TObject);
begin
  EditMode                 := emAdd;
  eUserName.Text           := '';
  FNameKey                 := '';
  cmbAccessLevel.ItemIndex := 0;
end;  // bbUserAddClick

//==============================================================================
procedure TfrmUserConfig.bbUserEditClick(Sender: TObject);
begin
  FOldNameKey := FNameKey;
  EditMode    := emEdit;
end;  // bbUserEditClick

//==============================================================================
procedure TfrmUserConfig.bbUserDelClick(Sender: TObject);
begin
  inherited;
  if FNameKey = AppSettings.UserID then
    raise EUserConfigError.CreateNonCritical(ResStr_CurrentUserDelete)
  else
  //Confirm the deletion
  if MessageDlg(Format(ResStr_RemoveUser, [dmGeneralData.GetIndividualName(FNameKey)]),
                mtConfirmation, [mbYes, mbNo], 0) = idYes then
    begin
      // Now remove the user. Will raise exception if failed due to referential integrity.
      dmDatabase.ExecuteSQL('DELETE FROM [User] WHERE Name_Key = ''' + FNameKey + '''');
      FNameKey := '';
      PopulateUserList;
    end;
end;  // bbUserDelClick

//==============================================================================
procedure TfrmUserConfig.bbEditSaveClick(Sender: TObject);
var
  i : Integer;
  lRestrictFull, nameChanged: Boolean;
  lAccess: TUserAccessLevel;
begin
  //Set user key
  ValidateValue((eUserName.Text <> ''), ResStr_EnterValidName, eUserName);
  ValidateValue(dmGeneralData.CheckIndividual(eUserName, FNameKey),
                ResStr_InvalidName, eUserName);

  //Check user does not already exist, whether added or edited.
  for i:= 0 to lvUsers.Items.Count - 1 do
    if (TKeyData(lvUsers.Items[i].Data).ItemKey = FNameKey) and
       ((EditMode = emAdd) or (lvUsers.ItemIndex <> i)) then
      raise EUserConfigError.CreateNonCritical(ResStr_UserExists);

  //Save changes
  lRestrictFull := cmbAccessLevel.ItemIndex = ACC_FULL_OWN;
  case cmbAccessLevel.ItemIndex of
    ACC_READ_ONLY: lAccess := ualReadOnly;
    ACC_RECORDER : lAccess := ualRecorder;
    ACC_ADD_ONLY : lAccess := ualAddOnly;
    ACC_FULL_OWN,
    ACC_FULL_EDIT: lAccess := ualFullUser;
    ACC_ADMIN    : lAccess := ualAdmin;
  else
    lAccess := ualReadOnly;
  end;

  if EditMode = emAdd then FOldNameKey := FNameKey;

  nameChanged := FNameKey <> FOldNameKey;

  for i := 0 to clbSurveys.Items.Count - 1 do
    if (not FOriginalSurveys[i]) and (clbSurveys.Checked[i] or nameChanged) then
    begin
      dmDatabase.RunStoredProc('usp_Survey_Allow_User_Access',
          ['@User_Name_Key', FOldNameKey,
           '@Survey_Key', TKeyData(clbSurveys.Items.Objects[i]).ItemKey]);
    end;

  if EditMode = emAdd then
    dmDatabase.ExecuteSQL(Format(
        'INSERT INTO [User] (Name_Key, Password, Security_Level, '
        + 'First_Login, Full_Edit_Own_Data) '
        + 'VALUES(''%s'',''%s'',%d, 1, %s)',
        [FNameKey, DEFAULT_USER_PASSWORD, Integer(lAccess) + 1, SQLBoolString[lRestrictFull]]))
  else
    dmDatabase.ExecuteSQL(Format(
        'UPDATE [User] SET Name_Key = ''%s'', Security_Level = %d, '
        + 'Full_Edit_Own_Data = %s WHERE Name_Key = ''%s''',
        [FNameKey, Integer(lAccess) + 1, SQLBoolString[lRestrictFull], FOldNameKey]));

  for i := 0 to clbSurveys.Items.Count - 1 do
  begin
    if (FOriginalSurveys[i] or nameChanged) and not clbSurveys.Checked[i] then
      dmDatabase.RunStoredProc('usp_Survey_Deny_User_Access',
          ['@User_Name_Key', FNameKey,
           '@Survey_Key', TKeyData(clbSurveys.Items.Objects[i]).ItemKey]);

    FOriginalSurveys[i] := clbSurveys.Checked[i];
  end;

  //Set edit mode
  EditMode:= emView;
  PopulateUserList;
end;  // bbEditSaveClick

//==============================================================================
procedure TfrmUserConfig.bbEditCancelClick(Sender: TObject);
begin
  EditMode:= emView;

  lvUsersSelectItem(nil, lvUsers.Selected, True);
end;  // bbEditCancelClick

//==============================================================================
procedure TfrmUserConfig.SetEditMode(const Value: TEditMode);
var
  ltfView, ltfAddEdit: boolean;
begin
  FEditMode  := Value;
  ltfView    := (Value = emView);
  ltfAddEdit := (Value in [emAdd, emEdit]);

  //Set controls
  bbUserEdit.Enabled     := ltfView;
  bbUserAdd.Enabled      := ltfView;
  bbUserDel.Enabled      := ltfView;

  bbEditSave.Enabled     := ltfAddEdit;
  bbEditCancel.Enabled   := ltfAddEdit;
  eUserName.Enabled      := ltfAddEdit and ((FNameKey <> AppSettings.UserID) or (Value = emAdd));
  bbUserFind.Enabled     := eUserName.Enabled;
  cmbAccessLevel.Enabled := eUserName.Enabled;
  bbSelectAll.Enabled    := ltfAddEdit;
  bbUnselectAll.Enabled  := ltfAddEdit;

  //Set focus
  case Value of
    emAdd, emEdit:
      if eUserName.Showing and (FNameKey <> AppSettings.UserID) then
        eUserName.SetFocus
      else
        clbSurveys.SetFocus;
    emView:
      if lvUsers.Showing then lvUsers.SetFocus;
  end;
end;  // SetEditMode

//==============================================================================
procedure TfrmUserConfig.ApplySecurity;
begin 
  bbUserEdit.Enabled := AppSettings.UserAccessLevel = ualAdmin;
  bbUserAdd.Enabled  := AppSettings.UserAccessLevel = ualAdmin;
  bbUserDel.Enabled  := AppSettings.UserAccessLevel = ualAdmin;
end;

{-------------------------------------------------------------------------------
  If the user attempts to change the selection in the Users list view, the
  change is only allowed if the form is in view mode.
}
procedure TfrmUserConfig.lvUsersChanging(Sender: TObject; Item: TListItem;
  Change: TItemChange; var AllowChange: Boolean);
begin
  inherited;
  AllowChange := EditMode = emView;
end;

{-------------------------------------------------------------------------------
  Flips the checked state of a checkbox if the user clicks on it while not in
  add or edit mode, thus setting it back to what it was before they clicked it.
  Essentially makes the Surveys list read only in other modes (in the absence
  of what would have seemed to be an obvious property to have for the control
  in the first place).
}
procedure TfrmUserConfig.clbSurveysOnClickCheck(Sender: TObject);
begin
  inherited;
  if not (EditMode in [emAdd, emEdit]) then
    clbSurveys.Checked[clbSurveys.ItemIndex] :=
        not clbSurveys.Checked[clbSurveys.ItemIndex];
end;

{-------------------------------------------------------------------------------
  Checks all items in the Surveys list.
}
procedure TfrmUserConfig.bbSelectAllClick(Sender: TObject);
begin
  inherited;
  SetCheckedAll(True);
end;

{-------------------------------------------------------------------------------
  Unchecks all items in the Surveys list.
}
procedure TfrmUserConfig.bbUnselectAllClick(Sender: TObject);
begin
  inherited;
  SetCheckedAll(False);
end;

{-------------------------------------------------------------------------------
  Sets the checked state of all items in the Surveys list to either True or
  False.
}
procedure TfrmUserConfig.SetCheckedAll(value: Boolean);
var
  i : integer;
begin
  for i := 0 to clbSurveys.Items.Count - 1 do
    clbSurveys.Checked[i] := value;
end;

{-------------------------------------------------------------------------------
}
procedure TfrmUserConfig.lvUsersSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
begin
  inherited;
  if Assigned(lvUsers.Selected) then
  begin
    //Set the details controls
    FNameKey                 := TKeyData(lvUsers.Selected.Data).ItemKey;
    eUserName.Text           := dmGeneralData.GetIndividualName(FNameKey);
    cmbAccessLevel.ItemIndex := StrToInt(TKeyData(lvUsers.Selected.Data).ItemAdditional);

    PopulateSurveyList;
  end;
end;

{-------------------------------------------------------------------------------
  Changes the tooltip of the Surveys list to be the text of the item the mouse
  is currently over. That way, if the name is too long for the box, it can still
  be read.
}
procedure TfrmUserConfig.clbSurveysMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  index : Integer;
begin
  index := clbSurveys.ItemAtPos(Point(X, Y), True);
  if  (index <= clbSurveys.Items.Count) and (index > -1) then
    clbSurveys.Hint := clbSurveys.Items[index]
  else
    clbSurveys.Hint := '';
end;

end.
