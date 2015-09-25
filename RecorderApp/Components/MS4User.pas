unit MS4User;

interface

uses
  Wintypes, messages, ms4;

const
  WMU_MAPUSERDRAW = WM_USER + $FF;
  USER_SHEET_TYPE = 3;

type
  sUserData = record
    hMapWnd : HWND;
    hdc : HDC;
    sViewExtent : msExtent;
    sViewRect : TRect;
    sUpdateExtent : msExtent;
    sUpdateRect : TRect;
    sUserUpdateExtent : msExtent;
    sUserUpdateRect : TRect;
  end;

  psUserData = ^sUserData;

  function MapUserCreateNewSheet( hMapWnd : HWnd; szFileName, szName, szDesc : pChar ) : Longint; stdcall;
  function MapUserGetDataLength( hMapWnd : hWnd; nSheet : LongInt; var dwLength : dWord ):LongInt; stdcall;
  function MapUserReadData( hMapWnd : hWnd ; nSheet : LOngint; var szData : pchar; dwLength, dwStart : dWord ):LongInt; stdcall;
  function MapUserRegisterHwnd( hMapWnd : hWnd; hRegisterWnd : HWnd ) : LongInt; stdcall;

implementation

function MapUserCreateNewSheet( hMapWnd : HWnd; szFileName, szName, szDesc : pChar ) : Longint; external 'MS4User.dll';
function MapUserGetDataLength( hMapWnd : hWnd; nSheet : LongInt; var dwLength : dWord ):LongInt; external 'MS4User.dll';
function MapUserReadData( hMapWnd : hWnd ; nSheet : LOngint; var szData : pchar; dwLength, dwStart : dWord ):LongInt; external 'MS4User.dll';
function MapUserRegisterHwnd( hMapWnd : hWnd; hRegisterWnd : HWnd ) : LongInt; external 'MS4User.dll';

end.
