//==============================================================================
//  Unit:        AddIn
//
//  Implements:  TdlgAddin
//
//  Description: Implements the interface for adding extra functionality to
//               Recorder 2000 via COM Add-ins.
//
//  Author:      John van Breda
//  Created:     8 April 1999
//
//  Last Revision Details:
//    $Revision: 43 $
//    $Date: 19/02/09 11:07 $
//    $Author: Andrewkemp $
//
//  $History: AddIn.pas $
//  
//  *****************  Version 43  *****************
//  User: Andrewkemp   Date: 19/02/09   Time: 11:07
//  Updated in $/JNCC/Development/Build/Source
//  VI 18006 (CCN 262) - done
//  Updated to reflect change in `IRecorderAddInRegistration'.
//
//  *****************  Version 42  *****************
//  User: Andrewkemp   Date: 18/02/09   Time: 19:10
//  Updated in $/JNCC/Development/Build/Source
//  VI 18006 (CCN 262) - done
//  Supports UAC elevation under Vista for installing and removing add-ins.
//  The actual installation / removal process is now delegated to a COM
//  component in an external DLL.
//  
//  *****************  Version 41  *****************
//  User: Ericsalmon   Date: 24/10/08   Time: 15:38
//  Updated in $/JNCC/Development/Build/Source
//  Fix for XPMenu and reformatting here and there.
//  
//  *****************  Version 40  *****************
//  User: Rickyshrestha Date: 28/12/07   Time: 10:45
//  Updated in $/JNCC/Development/Build/Source
//  Changed some hardcoded strings to resourcestring
//  
//  *****************  Version 39  *****************
//  User: Rickyshrestha Date: 11/12/07   Time: 14:29
//  Updated in $/JNCC/Development/Build/Source
//  Changed some constant to resourcestring (ResStr_NoAbout,
//  ResStr_CantRegister,  ResStr_Installed,  ResStr_PartialInstall,
//  ResStr_InstallFailed,  ResStr_FailReason, ResStr_InstallProblem,
//  ResStr_AddinExists,  ResStr_FileNotExists,  ResStr_CannotDelete)
//  
//  *****************  Version 38  *****************
//  User: Johnvanbreda Date: 5/07/06    Time: 14:55
//  Updated in $/JNCC/Development/Build/Source
//  Removed local machine administrator check as this not reliable.
//  
//  *****************  Version 37  *****************
//  User: Ericsalmon   Date: 26/08/04   Time: 12:06
//  Updated in $/JNCC/Development/Build/Source
//  Cleanup.
//  
//  *****************  Version 36  *****************
//  User: Johnvanbreda Date: 21/07/04   Time: 9:28
//  Updated in $/JNCC/Development/Build/Source
//  IR6633
//  Updated addin installation error handling
//  
//  *****************  Version 35  *****************
//  User: Ericsalmon   Date: 2/04/04    Time: 17:25
//  Updated in $/JNCC/Development/Build/Source
//  Changed registry key values to use constants. Also changed registry
//  root key to "\Software\Dorset Software\Recorder 6".
//  
//  *****************  Version 34  *****************
//  User: Ericsalmon   Date: 17/02/04   Time: 16:19
//  Updated in $/JNCC/Development/Build/Source
//  Replaced unnecessary TBitBtn with TButton.
//  
//  *****************  Version 33  *****************
//  User: Johnvanbreda Date: 20/03/03   Time: 15:30
//  Updated in $/JNCC/Source
//  Better error reporting
//  
//  *****************  Version 32  *****************
//  User: Andrewkemp   Date: 10/02/03   Time: 14:17
//  Updated in $/JNCC/Source
//  Fixed leak of TStringList.
//  
//  *****************  Version 31  *****************
//  User: Ericsalmon   Date: 16/07/02   Time: 17:24
//  Updated in $/JNCC/Source
//  Update. Install and Remove buttons remain disabled if current user does
//  not have admin rights on an NT/2000/XP machine. Enabled on Win95/98/ME.
//
//  *****************  Version 30  *****************
//  User: Ericsalmon   Date: 20/06/02   Time: 10:17
//  Updated in $/JNCC/Source
//  Replaced BitBtns with ImageListButtons
//
//  Copyright © Dorset Software Services Ltd, 2001
//
//==============================================================================

unit AddIn;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, Buttons, ApplicationSettings, Registry, OnlineHelp,
  GeneralData, ExceptionForm, GeneralFunctions, Constants, ImageListButton,
  RecorderAddInInstaller_TLB;

type
  EAddinDialogException = class(TExceptionPath);

  TdlgAddIn = class(TForm)
    lbAddIns: TListBox;
    mmDescription: TMemo;
    Label1: TLabel;
    Bevel1: TBevel;
    dlgOpenFile: TOpenDialog;
    bbInstall: TImageListButton;
    bbRemove: TImageListButton;
    bbAbout: TImageListButton;
    bbClose: TButton;
    bbShieldInstall: TBitBtn;
    bbShieldRemove: TBitBtn;
    procedure bbCloseClick(Sender: TObject);
    procedure bbInstallClick(Sender: TObject);
    procedure lbAddInsDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure lbAddInsClick(Sender: TObject);
    procedure bbAboutClick(Sender: TObject);
    procedure bbRemoveClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    hlpAddIn: TOnlineHelp;
    procedure PopulateListBox;
    procedure RemoveToolButton(iClsID: TGuid);
    procedure RefreshComInformation;
    procedure RefreshControls;
    procedure ShowShieldButtons;
    function CreateAddInInstaller: IRecorderAddInRegistration;
  public
    constructor Create(Aowner: TComponent); override;
  end;

// Global methods, that are also called when AppSettings autoinstalls addins
function GetLibraryName(iFileName: string): string;

//==============================================================================
implementation

{$R *.DFM}

uses
  FormActions, ComAddinUnit, OLETools, ComObj, MainTBar, Recorder2000_TLB,
  Menus, OLEContainer, APIUtils, ActiveX, PrivilegedComObj, StockIcons;
  
resourcestring
  ResStr_NoAbout            = 'No about box is available for the selected addin.';
  ResStr_Installed          = 'The following addin has been successfully installed: ';
  ResStr_PartialInstall     =
      'The %s addin was installed but the following errors were reported: '#13#10'%s';
  ResStr_InstallFailed      = 'The following file was not installed: ';
  ResStr_FailReason         =
      'The object you tried to install did not contain any valid %s addins.';
  ResStr_FileNotExists      = 'The file you have selected does not exist.';
  ResStr_CannotDelete       =
      'The addin class cannot be deleted.  This is likely to be because you do not have '
      + 'sufficient access rights.';
  ResStr_InstallingAddin    = 'Installing addin...';
  ResStr_RemovingAddin      = 'Removing addin';
  ResStr_ConfirmRemoveAddin = 'Are you sure you want to remove the addin %s?';
  ResStr_NotAnAdministrator =
      'Unable to continue'#10#10'You do not have permission to perform this'
      + ' task.'#10'Please contact your computer administrator for help.';
  ResStr_AddinExists =
      'An addin of that name already exists.  Would you like to replace it? Select no to install '+
          'from the copy of the addin file which already exists.';
  
//==============================================================================
constructor TdlgAddIn.Create(Aowner: TComponent);
begin
  inherited Create(AOwner);
  if WindowsSupportsElevation then ShowShieldButtons;
  RefreshControls;
end;

//==============================================================================
procedure TdlgAddIn.bbCloseClick(Sender: TObject);
begin
  Close;
end;  // bbCloseClick

//==============================================================================
procedure TdlgAddIn.bbInstallClick(Sender: TObject);
var
  lCursor:TCursor;
  Installer: IRecorderAddInRegistration;
  Result: HResult;
  Messages: WideString;
  doCopy: boolean;
  mr: TModalResult;
begin
  Installer := CreateAddInInstaller;
  if not Assigned(Installer) then Exit;
  
  if dlgOpenFile.Execute then
  begin
    lCursor:=HourglassCursor;
    frmMain.SetStatus(ResStr_InstallingAddin);
    try
      // Check File Exists, if not then bomb out
      if not FileExists(dlgOpenFile.FileName) then begin
        MessageDlg(ResStr_FileNotExists, mtInformation, [mbOK], 0);
        Exit;
      end;
      // if not in addin folder, and file already exists in addin folder, confirm overwrite
      if (CompareText(
          ExtractShortPathName(ExtractFilePath(dlgOpenFile.FileName)),
          ExtractShortPathName(AppSettings.AddInPath)
          ) <> 0) and FileExists(AppSettings.AddInPath + ExtractFileName(dlgOpenFile.FileName)) then begin
        // File already in the AddIn folder.
        // Ask the user if they want to overwrite
        mr := MessageDlg(ResStr_AddinExists, mtConfirmation, [mbYes,mbNo,mbCancel], 0);
        if mr=mrCancel then
          exit
        else doCopy := mr=mrYes;
      end else
        // copy file if not already in addin path
        doCopy := CompareText(
            ExtractShortPathName(ExtractFilePath(dlgOpenFile.FileName)),
            ExtractShortPathName(AppSettings.AddInPath)
          ) <> 0;
      Result := Installer.Install(dlgOpenFile.FileName, AppSettings.AddInPath, doCopy);
      // Result is now S_OK, E_FAIL or S_FALSE (=user aborted)
    finally
      DefaultCursor(lCursor);
      frmMain.TaskFinished;
    end;
    OleCheck(Installer.Get_Messages(Messages));
    if Result = S_OK then
    begin    
      if Messages='' then
        ShowInformation(ResStr_Installed + GetLibraryName(dlgOpenFile.FileName))
      else
        ShowInformation(Format(
            ResStr_PartialInstall,
            [GetLibraryName(dlgOpenFile.FileName), Messages]));
      RefreshComInformation;
    end
    else if Result <> S_FALSE then
    begin
      if Messages = '' then
        MessageDlg(ResStr_InstallFailed + GetLibraryName(dlgOpenFile.FileName) + #13#10 +
                                    Format(ResStr_FailReason, [Application.Title]),
                                    mtInformation, [mbOk], 0)
      else
        MessageDlg(
            ResStr_InstallFailed
                + GetLibraryName(dlgOpenFile.FileName)
                + #13#10 + Messages,
            mtInformation, [mbOk], 0);
    end;
  end;
end;  // bbInstallClick

//==============================================================================
procedure TdlgAddIn.lbAddInsDrawItem(Control: TWinControl; Index: Integer;
  Rect: TRect; State: TOwnerDrawState);
begin
  if lbAddIns.Items.Objects[Index] <> nil then begin
    with lbAddIns do begin
      Canvas.FillRect(Rect);
      AppSettings.ComAddins.Images.Draw(
          Canvas,Rect.Left + 2,
          Rect.Top + 2,
          (TAddinInfo(lbAddins.Items.Objects[Index])).ImageIndex);
      Canvas.TextOut(
          Rect.Left + BITMAP_SIZE + 4,
          Rect.Top + (lbAddins.ItemHeight - Canvas.TextHeight(Items[Index])) div 2,
          Items[Index]);
    end;
  end;
end;  // lbAddInsDrawItem

//==============================================================================
{ Set the description box to the currently selected item's text }
procedure TdlgAddIn.lbAddInsClick(Sender: TObject);
begin
  mmDescription.Text := TAddInInfo(lbAddIns.Items.Objects[lbAddIns.Itemindex]).Description;
end;

//==============================================================================
procedure TdlgAddIn.PopulateListBox;
var
  lCount : integer;
begin
  { Clear the list first }
  lbAddins.Clear;
  with AppSettings.ComAddins do begin
    for lCount := 0 to AddinCount - 1 do begin
      lbAddIns.Items.AddObject(AddinList[lCount].Name, AddinList[lCount]);
    end;
  end;
end;

//==============================================================================
{ Display the about box for the currently selected installed addin, if
    available }
procedure TdlgAddIn.bbAboutClick(Sender: TObject);
var
  lOleProxy : TOLEProxy;
begin
  if lbAddins.ItemIndex <> -1 then
    try
      lOleProxy :=
          TOLEProxy.Create(Self, TAddInInfo(lbAddIns.Items.Objects[lbAddIns.Itemindex]).ClsID);
      lOleProxy.ShowAboutBox;
    except
      on EOleSysError do  // no about box
        MessageDlg(ResStr_NoAbout, mtInformation, [mbOK], 0);
    end; // try..except
end;

//==============================================================================
{ Extracts the filename (no extension) from a full file name including path }
function GetLibraryName(iFileName: string): string;
var
  lFileName : string;
  lFileExt : string;
begin
  lFileName := ExtractFileName(iFileName);
  lFileExt := ExtractFileExt(iFilename);
  { Lop off the extendion }
  Result := Copy(lFileName, 1, Length(lFileName) - Length(lFileExt));
end;

//==============================================================================
procedure TdlgAddIn.bbRemoveClick(Sender: TObject);
var
  Installer: IRecorderAddInRegistration;
  lCursor:TCursor;
  ClassID: TGUID;
begin
  Installer := CreateAddInInstaller;
  if not Assigned(Installer) then Exit;

  if lbAddIns.ItemIndex <> -1 then begin
    if MessageDlg(Format(
        ResStr_ConfirmRemoveAddin, [lbAddins.Items[lbAddins.ItemIndex]]),
        mtconfirmation, [mbYes, mbNo], 0) <> mrYes then
      Exit;
    lCursor := HourglassCursor;
    frmMain.SetStatus(ResStr_RemovingAddin);
    try
      ClassID := TAddInInfo(lbAddIns.Items.Objects[lbAddIns.ItemIndex]).ClsID;
      
      if Installer.Remove(ClassID) <> S_OK then
      begin
        raise EAddinDialogException.CreateNonCritical(ResStr_CannotDelete);      
      end 
      else begin
        RemoveToolButton(ClassID);
        RefreshComInformation;
      end;
    finally
      DefaultCursor(lCursor);
      frmMain.TaskFinished;
    end;
  end;
end;

//==============================================================================
{ Removes Toolbar buttons and actions that are associated with a COM Addin }
procedure TdlgAddin.RemoveToolButton(iClsID: TGuid);
var
  i, lReIndexLoop: integer;
begin
  with frmMain.tbMainToolbar do begin
    for i := ButtonCount - 1 downto 0 do
    begin
      if Buttons[i].Action is TComAction then
        if IsEqualGUID(TComAction(Buttons[i].Action).ClsID, iClsID) then
          Buttons[i].Free;
    end;

    for i := dmFormActions.alForms.ActionCount - 1 downto 0 do
    begin
      if dmFormActions.alForms.Actions[i] is TComAction then
        if IsEqualGUID(TCOMAction(dmFormActions.alForms.Actions[i]).ClsID, iClsID) then
        begin
          dmFormActions.alForms.Actions[i].Free;
          { Need to reindex any buttons linked to subsequent actions }
          for lReIndexLoop := 0 to ButtonCount - 1 do
            if Buttons[lReIndexLoop].Tag > i then
              Buttons[lReIndexLoop].Tag := Buttons[lReIndexLoop].Tag - 1;
        end;
    end;
  end;
end;

//==============================================================================
{ Reloads all the Com addins. }
procedure TdlgAddIn.RefreshComInformation;
var
  lIndex: integer;
begin
  { Get rid of the old settings, menu options, actions etc }
  AppSettings.ComAddins.Free;
  { close any COM forms }
  for lIndex := 0 to Screen.FormCount - 1 do
  begin
    if Screen.Forms[lIndex] is TfrmMDIContainer then
      Screen.Forms[lIndex].Close;
  end;
  { drop COM actions }
  for lIndex := dmFormActions.alforms.ActionCount - 1 downto 0 do
    if dmFormActions.alforms.Actions[lIndex] is TCOMAction then
      dmFormActions.alforms.Actions[lIndex].Free;
  { Reload it all over again! }
  AppSettings.InitComAddins;
  RefreshControls;
  mmDescription.Clear;
  // Refresh menus, or they won't blend in properly.
  frmMain.RefreshXPMenu;
end;

//==============================================================================
procedure TdlgAddIn.FormCreate(Sender: TObject);
begin
  //Help Setup
  hlpAddIn := TOnlineHelp.Create(Self.Handle);
  OnHelp := hlpAddIn.OnHelpReplacement;
  HelpContext := IDH_ADDIN;
end;

//==============================================================================
procedure TdlgAddIn.FormDestroy(Sender: TObject);
begin
  hlpAddIn.Free;
end;

//==============================================================================
procedure TdlgAddin.RefreshControls;
begin
  PopulateListBox;
  if lbAddins.Items.Count = 0 then
    bbRemove.Enabled := False
  else begin
    bbRemove.Enabled := True;
    lbAddins.ItemIndex := 0;
  end;
end;  // RefreshControls

{ ------------------------------------------------------------------------------
  Replaces the default Install and Remove buttons with versions that display
  the UAC elevation shield.
}
procedure TdlgAddIn.ShowShieldButtons;
begin
  SetUACShield(bbShieldInstall);
  SetUACShield(bbShieldRemove);  
  bbInstall.Visible := False;
  bbRemove.Visible := False;
  bbShieldInstall.Visible := True;
  bbShieldRemove.Visible := True;
end;

{ ------------------------------------------------------------------------------
  Constructs an object used to register or unregister add-ins.

  On Windows Vista or later this requires elevation to administrator level.
  If the user cancels the elevation prompt, or otherwise fails to elevate then
  nil is returned.
}
function TdlgAddIn.CreateAddInInstaller: IRecorderAddInRegistration;
begin
  Result := CreateComObjectAsAdmin(
      Handle,
      CLASS_RecorderAddInRegistration) as IRecorderAddInRegistration;

  if Assigned(Result) then
  begin
    if not Result.IsElevated then
    begin
      // UAC is disabled, and the user is not an administrator.
      // Our message and choice of icon for this dialog are consistent with
      // Windows' own dialogs for this situation.
      MessageDlg(ResStr_NotAnAdministrator, mtError, [mbOK], 0);
      Result := nil;
    end;
  end;
end;

end.
