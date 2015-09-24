{===============================================================================
  Unit:        ServerSelectPage

  Defines:     TfraServerSelect

  Description:

  Model:       Server Install 6.mpb

  Created:     November 2004

  Last revision information:
    $Revision: 15 $
    $Date: 13/04/09 16:59 $
    $Author: Pauldavies $

===============================================================================}

unit ServerSelectPage;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, BasePage, FolderBrowser, StdCtrls, RestrictedEdits, Registry,
  ExtCtrls, BaseSQLExpressSettingsPage, DBFileLocationSelectFrame;

type
  TfraServerSelect = class(TfraBaseSQLExpressSettings)
    cmbInstances: TComboBox;
    lblRequiredSpace: TLabel;
    lblAvailable: TLabel;
    lblDisk: TLabel;
    lblInfo: TLabel;
    lblInstanceName: TLabel;
    lblNotLocal: TLabel;
    lblRequired: TLabel;
    lblWarning: TLabel;
    lblDropDownInfo: TLabel;
    fraDBFileLocationSelect: TfraDBFileLocationSelect;
    procedure CheckChange(Sender: TObject);
  private
    function CheckDriveIsValid: Boolean;
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
  
//==============================================================================
implementation

{$R *.dfm}

uses
  SiteInfoPage, LoginPage, Settings, SetupConstants, TextMessages, Functions,
  GeneralFunctions;

resourcestring
  ResStr_AvailableSpace = 'Available disk space on %s:';
  ResStr_Unknown        = 'Unknown';
  ResStr_DatabaseFolderMissing =
      'The selected Database folder does not exist. Create now?';


{-==============================================================================
    TfraServerSelect
===============================================================================}
{-------------------------------------------------------------------------------
}
procedure TfraServerSelect.CheckChange(Sender: TObject);
var reg: TRegistry;

    function CheckTrustedConnection: boolean;
    var
      lKeyOpened: Boolean;
    begin
      lKeyOpened := reg.OpenKey(Format(REG_KEY_SQLSERVER_NAMED,
                 [GetRegistryInstanceName(cmbInstances.Text)]), false);
      if lKeyOpened then
        if reg.KeyExists(REG_LOGINMODE) and (reg.ReadInteger(REG_LOGINMODE) = 1)
            and (not lblWarning.Visible) then begin
          MessageDlg(ResStr_WindowsAuthenticationWarning, mtWarning, [mbOk], 0);
          // No point showing the warning if this is not a local instance
          lblWarning.Visible := not lblNotLocal.Visible;
          Settings.TrustedLogin := true;
        end;
      result :=  lKeyOpened;
    end;

begin
  inherited;
  if Loading then Exit;
  lblWarning.Visible := False;
  Settings.TrustedLogin := false;
  // Check registry for Trusted Connection-Only login mode
  reg := TRegistry.Create;

  try
    reg.RootKey := HKEY_LOCAL_MACHINE;
    reg.Access := KEY_READ;
    if not CheckTrustedConnection then begin
      reg.Access := KEY_READ OR KEY_WOW64_64KEY;
      CheckTrustedConnection;
    end;
  finally
    reg.Free;
  end;
  ChangedContent;
end;  // TfraServerSelect.CheckChange

{-------------------------------------------------------------------------------
}
function TfraServerSelect.CheckDriveIsValid: Boolean;
var
  lDrive: String;
  lRegKey: String;
  lFree, lFreeForCaller, lTotal: TLargeInteger;
begin
  Result := False;
  if SameText(cmbInstances.Text, Settings.ComputerName) then
    lRegKey := REG_KEY_SQLEXPRESS_DEFAULT_SETUP
  else
    lRegKey := Format(REG_KEY_SQLSERVER_NAMED_SETUP,
                      [Copy(cmbInstances.Text, Length(Settings.ComputerName + PathDelim) + 1, 255)]); 
  
  with TRegistry.Create() do
    try
      RootKey := HKEY_LOCAL_MACHINE;
      if OpenKeyReadOnly(lRegKey) then
        lDrive := ExtractFileDrive(ReadString(REG_INSTANCE_PATH));
      lDrive := IncludeTrailingPathDelimiter(UpperCase(lDrive));
      if not (lDrive[1] in ['A'..'Z']) then lDrive := 'C' + DriveDelim + PathDelim;
    finally
      Free;
    end;

  // Drive must be local
  if GetDriveType(PChar(lDrive)) <> DRIVE_FIXED then begin
    lblNotLocal.Visible := True;
    // No point showing the warning if this is not a local instance
    lblWarning.Visible := False;
    Result := False;
  end else
    lblNotLocal.Visible := False; 

  if fraDBFileLocationSelect.FilePath <> '' then
    lDrive := UpperCase(fraDBFileLocationSelect.FilePath[1]) + DriveDelim + PathDelim;
    
  // Update caption
  lblDisk.Caption := Format(ResStr_AvailableSpace, [lDrive[1]]);

  // Drive must have enough room to accomodate unzipped database.
  if SysUtils.GetDiskFreeSpaceEx(PChar(lDrive), lFreeForCaller, lTotal, @lFree) then begin
    lblAvailable.Caption := FormatBytes(lFree);
    if lFree >= Settings.RequiredDiskSpace then begin
      lblAvailable.Font.Color := clWindowText;
      Result := True;
    end else
      lblAvailable.Font.Color := clRed;
  end else
    lblAvailable.Caption := ResStr_Unknown;
end;  // TfraServerSelect.CheckDriveIsValid

{-------------------------------------------------------------------------------
}
function TfraServerSelect.GetConfirmCancel: Boolean;
begin
  Result := False;
end;  // TfraServerSelect.GetConfirmCancel 

{-------------------------------------------------------------------------------
}
function TfraServerSelect.GetHasNext: Boolean;
var
  lName: String;
begin
  lName := UpperCase(cmbInstances.Text);
  Result := (lName <> '') and
            (SameText(lName, '(Local)') or SameText(lName, Settings.ComputerName) or
             (Pos(UpperCase(Settings.ComputerName) + PathDelim, lName) = 1)) and
            CheckDriveIsValid and
            fraDBFileLocationSelect.IsValid;
end;  // TfraServerSelect.GetHasNext 

{-------------------------------------------------------------------------------
}
function TfraServerSelect.GetHasPrevious: Boolean;
begin
  Result := True;
end;  // TfraServerSelect.GetHasPrevious

{-------------------------------------------------------------------------------
}
function TfraServerSelect.GetNext: TBasePageClass;
begin
  Result := TfraLogin;
  GetRegistryInstanceName(cmbInstances.Text);
end;  // TfraServerSelect.GetNext

{-------------------------------------------------------------------------------
}
function TfraServerSelect.GetPrevious: TBasePageClass;
begin
  Result := TfraSiteInfo;
end;  // TfraServerSelect.GetPrevious 

{-------------------------------------------------------------------------------
}
function TfraServerSelect.GetResourceImage: String;
begin
  Result := ResImg_SQLEXPRESS;
end;  // TfraServerSelect.GetResourceImage 

{-------------------------------------------------------------------------------
}
procedure TfraServerSelect.LoadContent;
begin
  inherited;
  fraDBFileLocationSelect.OnChange := CheckChange;
  fraDBFileLocationSelect.Settings := Settings;

  if Settings.SpecifyOwnDatabasePath then
    fraDBFileLocationSelect.FilePath := Settings.DatabasePath
  else
    fraDBFileLocationSelect.FilePath := '';

  with cmbInstances do begin
    // Only want local instances, no network stuff.
    Items.Assign(Settings.LocalInstanceNames);
  
    // If nothing uet set for default existing instance, select first one.
    ItemIndex := Items.IndexOf(Settings.ServerName);
    if (ItemIndex = -1) and (Items.Count > 0) then ItemIndex := 0;
  end;
  lblRequired.Caption := FormatBytes(Settings.RequiredDiskSpace);
  CheckTrustedConnectionMode(cmbInstances.Text, lblWarning);
  ChangedContent;
end;  // TfraServerSelect.LoadContent 

{-------------------------------------------------------------------------------
}
procedure TfraServerSelect.SaveContent;
begin
  inherited;
  Settings.DatabasePath           := fraDBFileLocationSelect.FilePath;
  Settings.SpecifyOwnDatabasePath := fraDBFileLocationSelect.FilePath <> '';
  Settings.ServerName             := cmbInstances.Text;
  Settings.TrustedLogin           := lblWarning.Visible;
end;  // TfraServerSelect.SaveContent

{-------------------------------------------------------------------------------
  Validates the controls on this page.
}
procedure TfraServerSelect.ValidateContent;
begin
  // Gives the user a chance to create the folder they selected, if it does not
  // already exist.
  // Only allow moving on to next page if folder gets created, or already exists.
  if (fraDBFileLocationSelect.FilePath <> '') and
        (not Settings.FolderExists(fraDBFileLocationSelect.FilePath, True, True,
                                   ResStr_DatabaseFolderMissing)) then
    Abort;
end;  // TfraSQLExpressSettings.ValidateContent

end.


