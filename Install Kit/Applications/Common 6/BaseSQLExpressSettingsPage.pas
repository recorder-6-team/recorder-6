{===============================================================================
  Unit:        BaseSQLExpressSettingsPage

  Defines:     TfraBaseSQLExpressSettings

  Description: A base frame for pages that request SQLExpress settings

  Model:

  Created:     March 2009

  Last revision information:
    $Revision: 5 $
    $Date: 15/04/09 11:05 $
    $Author: Pauldavies $

===============================================================================}
unit BaseSQLExpressSettingsPage;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, BasePage, Registry, StdCtrls, ComObj, ExtCtrls;

resourcestring
  ResStr_InvalidInstanceName =
   'Unable to reach the specified SQL Server/MSDE instance.'#13#10
   + 'If you entered the instance name manually, please check it is spelt correctly.'#13#10#13#10
   + 'The error was as follows:'#13#10 + '''%s''';

type
  TfraBaseSQLExpressSettings = class(TBasePage)
  protected
    function GetRegistryInstanceName(const AUserInstanceName: string): string;
    procedure CheckTrustedConnectionMode(const AUserInstanceName: string; AWarningLabel: TLabel);
    function ValidInstanceName(const AInstanceName: String): Boolean;
    function ValidPassword(password, passwordCheck: string): Boolean;
    function ServerPathIsValid(path: string; lblNotLocal, lblNoSpace: TLabel): Boolean;
  end;

{==============================================================================}
implementation

{$R *.dfm}

uses
  SetupConstants, TextMessages, GeneralFunctions, Functions;

resourcestring
  ResStr_NameInUse   = 'Instance name already in use.';
  ResStr_InvalidName = 'Invalid instance name.';

{-------------------------------------------------------------------------------
  Retrieve the registry version of a user entered instance name.  In SQL 2005
    these are not the same!
}              
function TfraBaseSQLExpressSettings.GetRegistryInstanceName(const AUserInstanceName: string): string;
begin
  Result := Copy(AUserInstanceName, Length(Settings.ComputerName) + 2, 255);
  with TRegistry.Create do
    try
      RootKey := HKEY_LOCAL_MACHINE;
      Access  := KEY_READ;
      if KeyExists(REG_KEY_SQL2005_INSTANCES) then begin
        if OpenKeyReadOnly(REG_KEY_SQL2005_INSTANCES) then begin
          if ValueExists(Result) then
            Result := ReadString(Result)
          else if GetOSVersion > wvWin2k then begin
            // if higher than win2K, then we need to check 64 bit registry as well.
            Access  := KEY_READ OR KEY_WOW64_64KEY;
            if OpenKey(REG_KEY_SQL2005_INSTANCES, false) then
              if ValueExists(Result) then
                Result := ReadString(Result);
          end;
        end;
      end;
    finally
      Free;
    end;
end;

{-------------------------------------------------------------------------------
  Warns the user if a selected instance only supports a trusted connection
}
procedure TfraBaseSQLExpressSettings.CheckTrustedConnectionMode(const AUserInstanceName: string;
    AWarningLabel: TLabel);
var
  lKeyOpened: boolean;
  reg: TRegistry;

  procedure OpenRegKey;
  begin
    // don't openKeyReadOnly here, as we have already set the access for the registry.
    if CompareText(AUserInstanceName, Settings.ComputerName) = 0 then
      lKeyOpened := reg.OpenKey(REG_KEY_MSDE_DEFAULT, false)
    else
      lKeyOpened := reg.OpenKey(Format(REG_KEY_SQLSERVER_NAMED,
                    [GetRegistryInstanceName(AUserInstanceName)]), false);
  end;

begin
  // Check registry for Trusted Connection-Only login mode
  reg := TRegistry.Create;
  with reg do
    try
      RootKey := HKEY_LOCAL_MACHINE;
      Access  := KEY_READ;
      OpenRegKey;
      if not lKeyOpened then begin
        // try forcing 64 bit registry access in case it is a 64 bit instance
        Access  := KEY_READ OR KEY_WOW64_64KEY;
        OpenRegKey;
      end;
      AWarningLabel.Visible :=
          lKeyOpened and
          (ValueExists(REG_LOGINMODE) and (ReadInteger(REG_LOGINMODE) = 1));

    finally
      Free;
    end
end;

{-------------------------------------------------------------------------------
  Check the specified server/instance exists and can be reached.
}
function TfraBaseSQLExpressSettings.ValidInstanceName(const AInstanceName: String): Boolean;
var
  i, slashCount: integer;
begin
  result := Settings.LocalInstanceNames.IndexOf(AInstanceName) <> -1;
  slashCount := 0;
  if not result then begin
    result := AInstanceName[1] in ['_', 'a'..'z', 'A'..'Z'];

    for i := 2 to Length(AInstanceName) do begin
      if AInstanceName[i]='\' then
        inc(slashCount)
      else
        result := result and (AInstanceName[i] in ['-','_', 'a'..'z', 'A'..'Z',
                                                            '$', '0'..'9']);
    end;
  end;
  result := result and (slashCount <= 1);
end;

{-----------------------------------------------------------------------------
  Returns true if the password matches its password check, is at least 8 characters
  long, and contains 3 or more different character types, or false otherwise.
}
function TfraBaseSQLExpressSettings.ValidPassword(password, passwordCheck: string): Boolean;
var
  i, upper, lower, digit, other: integer;
begin
  if (password <> passwordCheck) or (Length(password) < 8) then
    Result := False
  else begin
    upper := 0;
    lower := 0;
    digit := 0;
    other := 0;

    // We set the values to 1 if they are found- could have used booleans, but this
    // way it's easier to check how many types there are by just adding them together
    // at the end.
    for i := 1 to Length(password) do
      case password[i] of
        '0'..'9': digit := 1;
        'A'..'Z': upper := 1;
        'a'..'z': lower := 1;
      else
        other := 1;
      end;

    Result := (digit + upper + lower + other >= 3);
  end;
end;

{-------------------------------------------------------------------------------
  Check whether or not the server path is valid.
}
function TfraBaseSQLExpressSettings.ServerPathIsValid(path: string; lblNotLocal,
    lblNoSpace: TLabel): Boolean;
var
  drive: String;

    {-------------------------------------------------------------------------------
      Checks whether there is sufficient disk space on the selected drive.
    }
    function CheckAvailableDiskSpace: Boolean;
    var
      lFree, lFreeForCaller, lTotal: TLargeInteger;
    begin
      if drive = PathDelim then
        Result := False
      else if SysUtils.GetDiskFreeSpaceEx(PChar(drive), lFreeForCaller, lTotal, @lFree) then
        Result := lFree > I_SQLEXPRESSSIZE
      else
        Result := False;
    end;  // CheckAvailableDiskSpace

begin
  Result := True;
  drive  := ExtractFileDrive(path) + PathDelim;
  lblNotLocal.Visible := False;
  lblNoSpace.Visible  := False;

  if drive = PathDelim then
    Result := False
  else
  if GetDriveType(PChar(drive)) <> DRIVE_FIXED then begin
    lblNotLocal.Visible := True;
    Result              := False;
  end else
  if not CheckAvailableDiskSpace then begin
    lblNoSpace.Visible := True;
    Result             := False;
  end;
end;

end.
