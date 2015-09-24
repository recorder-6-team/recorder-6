{===============================================================================
  Unit:        RebootFunctions

  Description: Functions to reboot a computer.

  Created:     March 2004

  Last revision information:
    $Revision: 1 $
    $Date: 14/05/04 17:03 $
    $Author: Ericsalmon $

===============================================================================}

unit RebootFunctions;

interface

uses
  Windows, SysUtils;

procedure RebootMachine;

implementation

//------------------------------------------------------------------------------
// Sets privileges under Windows NT ...
function SetPrivilegeForReboot(AEnable: Boolean): Boolean;
var lTPPrev, lTP: TTokenPrivileges;
    lToken      : THandle;
    ldwRetLen   : DWord;
begin
  Result := False;
  if not OpenProcessToken(GetCurrentProcess, TOKEN_ADJUST_PRIVILEGES or TOKEN_QUERY, lToken)
      then
    raise Exception.Create('Error: OpenProcessToken');

  lTP.PrivilegeCount := 1;
  if LookupPrivilegeValue(nil, 'SeShutdownPrivilege', lTP.Privileges[0].LUID) then
  begin
    if AEnable then lTP.Privileges[0].Attributes := SE_PRIVILEGE_ENABLED
               else lTP.Privileges[0].Attributes := 0;

    ldwRetLen := 0;
    if AdjustTokenPrivileges(lToken, False, lTP, SizeOf(lTPPrev), lTPPrev, ldwRetLen) then
      Result := True
    else
      raise Exception.Create('Error: AdjustTokenPrivileges');
  end;
  CloseHandle(lToken);
end;  // SetPrivilegeForReboot

//------------------------------------------------------------------------------
procedure RebootMachine;
begin
  // Easy if not NT onward
  if Win32Platform <> VER_PLATFORM_WIN32_NT then begin
    ExitWindowsEx(EWX_LOGOFF + EWX_FORCE, 0);  // Force Explorer to logoff user
    ExitWindowsEx(EWX_REBOOT + EWX_FORCE, 0);  // Now we can ask for a reboot
  end else
  // Otherwise, need to do a bit more work
  if SetPrivilegeForReboot(True) then
  begin
    ExitWindowsEx(EWX_REBOOT + EWX_FORCE, 0);
    SetPrivilegeForReboot(False);
  end;
end;  // RebootMachine

end.
