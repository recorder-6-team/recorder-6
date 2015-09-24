{===============================================================================
  Unit:         BlockRecorder

  Defines:      AcquireRecorderLock
                ReleaseRecorderLock

  Description:  A function that can be used to protect against running code
                when there is an instance of Recorder in another process.

  Created:      February 2009

  Last revision information:
    $Revision: 1 $
    $Date: 23/02/09 19:26 $
    $Author: Simonwood $

  Copyright © Dorset Software Services Ltd, 2009

===============================================================================}
unit RecorderLock;


interface

function AcquireRecorderLock: Boolean;
procedure ReleaseRecorderLock;


implementation

{$WARN SYMBOL_PLATFORM OFF}

uses
  Windows, SysUtils;

var
  Handle: THandle;

{ ------------------------------------------------------------------------------
  Attempts to acquire access to the named 'Recorder 2000' mutex.
  Returns True if successful, otherwise False.
}
function AcquireRecorderLock: Boolean;
begin
  Assert(Handle = 0, 'Lock already acquired');
  
  Handle := CreateMutex(nil, True, 'Recorder 2000');  
  if Handle <> 0 then
  begin
    Result := GetLastError <> ERROR_ALREADY_EXISTS;
    if not Result then Handle := 0;
  end
  else
  begin
    Result := False;
    if GetLastError <> ERROR_ACCESS_DENIED then RaiseLastOSError;
  end
end;

{ ------------------------------------------------------------------------------
  Releases access to the named 'Recorder 2000' mutex.
}
procedure ReleaseRecorderLock;
begin
  Assert(Handle <> 0, 'Lock not held');
  try
    Win32Check(ReleaseMutex(Handle));
    Win32Check(CloseHandle(Handle));
  finally
    Handle := 0;
  end;
end;

end.
