{===============================================================================
  Unit:         PrivilegedComObj

  Defines:      WindowsSupportsElevation
                IsElevated
                CreateComObjectAsAdmin
                SetUACShield

  Description:  Creation of elevated COM objects.

  Created:      February 2009

  Last revision information:
    $Revision: 4 $
    $Date: 18/02/09 19:07 $
    $Author: Andrewkemp $

  Copyright © Dorset Software Services Ltd, 2009

===============================================================================}
unit PrivilegedComObj;

interface

uses
  Windows, StdCtrls, Buttons;

function WindowsSupportsElevation: Boolean;
function IsElevated: Boolean;
function CreateComObjectAsAdmin(HWnd: HWND; const ClassID: TGUID): IUnknown;
procedure SetUACShield(Button: TButton); overload;
procedure SetUACShield(Button: TBitBtn); overload;

  
implementation

{$WARN SYMBOL_PLATFORM OFF}

uses
  ActiveX, SysUtils, ComObj, Graphics, StockIcons;

type
  TBindOpts3 = record
    cbStruct: DWord;
    grfFlags: DWord;
    grfMode: DWord;
    dwTickCountDeadline: DWord;
    dwTrackFlags: DWord;
    dwClassContext: DWord;
    locale: LCID;
    pServerInfo: PCoServerInfo;
    HWnd: HWND;
  end;

  PBindOpts3 = ^TBindOpts3;

  TTokenElevation = record
    TokenIsElevated: DWORD;
  end;

  PTokenElevation = ^TTokenElevation;

const
  BCM_FIRST = $1600;
  BCM_SETSHIELD = BCM_FIRST + $000C;

  TokenElevation: TTokenInformationClass = TTokenInformationClass(20);


{ ------------------------------------------------------------------------------
  Gets a value indicating whether the current version of Windows supports
  COM elevation (i.e. it is Windows Vista, or later).
}
function WindowsSupportsElevation;
begin
  Result := (Win32Platform > VER_PLATFORM_WIN32_NT)
      or ((Win32Platform = VER_PLATFORM_WIN32_NT) and (Win32MajorVersion >= 6));
end;

{ ------------------------------------------------------------------------------
  Gets a value indicating whether the access token of the current process is
  elevated.

  Returns True when any of the following are true:

    * The current version of Windows is earlier than Vista.
    * UAC is enabled, and an administrator elevated the process.
    * UAC is disabled, and the current user is an administrator.

  Otherwise, returns False.
}
function IsElevated: Boolean;
var
  Token: THandle;
  Length: DWord;
  Buffer: Pointer;
begin
  if not WindowsSupportsElevation then
  begin
    Result := True;
    Exit;
  end;
  
  Win32Check(OpenProcessToken(GetCurrentProcess(), TOKEN_QUERY, Token));  
  try
    // find the required buffer length; this is not checked because it will
    // always return a failure code (a buffer of length zero is too small)
    GetTokenInformation(Token, TokenElevation, nil, 0, Length);

    GetMem(Buffer, Length);
    try
      Win32Check(
          GetTokenInformation(Token, TokenElevation, Buffer, Length, Length));
      Result := PTokenElevation(Buffer).TokenIsElevated <> 0;
    finally
      FreeMem(Buffer);
    end;  
  finally
    Win32Check(CloseHandle(Token));
  end;
end;

{ ------------------------------------------------------------------------------
  Creates a COM object, elevated to administrator level.
}
function CoCreateInstanceAsAdmin(
  HWnd: HWND;
  const clsid: TCLSID;
  const iid: TIID;
  out pv): HResult;
var
  Moniker: WideString;
  bo: TBindOpts3;
begin
  Moniker := 'Elevation:Administrator!new:' + GUIDToString(clsid);
  FillChar(bo, SizeOf(TBindOpts3), 0);
  bo.cbStruct := SizeOf(TBindOpts3);
  bo.HWnd := HWnd;
  bo.dwClassContext := CLSCTX_LOCAL_SERVER;
  Result := CoGetObject(PWideString(Moniker), @bo, iid, @pv);
end;

{ ------------------------------------------------------------------------------
  Creates a COM object, elevated to administrator level if the current version
  of Windows supports COM elevation.

  The given window handle identifies the parent of the elevation /
  administrator consent prompt.

  Returns Nil if the user cancels the elevation / administrator consent prompt.
}
function CreateComObjectAsAdmin(
  HWnd: HWND;
  const ClassID: TGUID): IUnknown;
var
  Hr: HResult;
begin
  if not WindowsSupportsElevation then
    Result := CreateComObject(ClassID)
  else
  begin
    Hr := CoCreateInstanceAsAdmin(HWnd, ClassID, IUnknown, Result);
    if HResultCode(Hr) = ERROR_CANCELLED then
      Result := Nil
    else
      OleCheck(Hr);
  end;
end;

{ ------------------------------------------------------------------------------
  If the current version of Windows supports COM elevation, then displays the
  UAC shield icon on the given button.

  This is Microsoft's recommended method of showing the shield on a button.
  However, note that this only works if the application manifest specifies a
  dependency on version 6 of comctl32.dll ("Microsoft.Windows.Common-Controls").
  If that is not possible, then use the overload for TBitBtn instead.
}
procedure SetUACShield(Button: TButton);
begin
  if WindowsSupportsElevation then
  begin
    SendMessage(Button.Handle, BCM_SETSHIELD, 0, Integer(True));
  end;
end;

{ ------------------------------------------------------------------------------
  If the current version of Windows supports COM elevation, then displays the
  UAC shield icon on the given button.
}
procedure SetUACShield(Button: TBitBtn);
var
  Icon: TIcon;
begin
  if WindowsSupportsElevation then
  begin
    Button.Kind := bkCustom;
    Button.Glyph.Width := GetSystemMetrics(SM_CXSMICON);
    Button.Glyph.Height := GetSystemMetrics(SM_CYSMICON);

    Icon := CreateStockIcon(siidShield, sisSmall);
    try  
      Button.Glyph.Canvas.Draw(0, 0, Icon);
    finally
      Icon.Free;
    end;
  end;
end;

end.
