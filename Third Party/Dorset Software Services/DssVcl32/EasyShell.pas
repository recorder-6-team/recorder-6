{ Unit to facilitate shelling documents.
    If shell doesn't work, tries to use QuikView if its available.

    Call ShellFile( filename );
    Raise EShellErrors for failures

    Copyright Dorset Software Services Ltd 1999
}

unit easyshell;

interface

uses Windows, Registry, Sysutils, ShellApi;

type
  EShellError = class(Exception);

function ShellFile( const iFile : string ): Boolean; overload;
function ShellFile( const iFile, iParams, iWorkingDir  : string ): Boolean; overload;
function TryQuickView( const iFile : string ): Boolean;
function CheckRetCode(iRetCode: integer): Boolean;


implementation

resourcestring
  ResStr_OSOutOfMemory    ='The operating system is out of memory or resources.';
  ResStr_FileNotFound     ='The specified file was not found.';
  ResStr_PathNotFound     ='The specified path was not found.';
  ResStr_BadFormat        ='The .EXE file is invalid (non-Win32 .EXE or error in .EXE image).';
  ResStr_AccessDenied     ='The operating system denied access to the specified file.';
  ResStr_AssocIncomplete  ='The filename association is incomplete or invalid.';
  ResStr_DDEBusy          ='The DDE transaction could not be completed because other DDE transactions were being processed.';
  ResStr_DDEFail          ='The DDE transaction failed.';
  ResStr_DDETimeOut       ='The DDE transaction could not be completed because the request timed out.';
  ResStr_DllNotFound      ='The specified dynamic-link library was not found.';
  ResStr_ERRORM           ='There was not enough memory to complete the operation.';
  ResStr_ERRShare         ='A sharing violation occurred.';


 {Call this function to attempt to shell a file}
function ShellFile( const iFile : string ): Boolean;
var
  szCmdString   : array[0..255] of char;
  szParamString  : array[0..255] of char;
  lRetCode       : longint;
begin
  StrPCopy( szCmdString, iFile );
  StrPCopy( szParamString, '' );
  lRetCode:=ShellExecute(0,'open',szCmdString,szParamString, '',SW_SHOW);
  if lRetCode = SE_ERR_NOASSOC then
    Result := TryQuickView( iFile )
  else
    Result := CheckRetCode(lRetCode);
end;  // ShellFile


{ This version allows you to specify the working dir and parameters }
function ShellFile( const iFile, iParams, iWorkingDir  : string ): Boolean;
var
  szCmdString    : array[0..255] of char;
  szParamString  : array[0..255] of char;
  szWorkDirString: array[0..255] of char;
  lRetCode       : longint;
begin
  StrPCopy( szCmdString, iFile );
  StrPCopy( szParamString, iParams );
  StrPCopy( szWorkDirString, iWorkingDir );
  lRetCode:=ShellExecute(0,'open',szCmdString,szParamString,
                 szWorkDirString, SW_SHOW);
  if lRetCode = SE_ERR_NOASSOC then
    Result := TryQuickView( iFile )
  else
    Result := CheckRetCode(lRetCode);
end;


//==============================================================================
{ Assuming a normal ShellExecute has failed, this procedure will try to shell
     an external reference using quick view }
function TryQuickView( const iFile : string ): Boolean;
var
  rRegistryList : TRegistry ;
  lQuikViewPath : string;
  lWindowsDirPath : array[0..255] of char;
  lRetCode : integer;
  szCmdString   : array[0..255] of char;
  szFileString  : array[0..255] of char;
begin
  // find out if this machine has quikview.exe
  rRegistryList := TRegistry.Create;
  try
    rRegistryList.RootKey := HKEY_CLASSES_ROOT;
    if rRegistryList.KeyExists( 'QUICKVIEW' ) then
    begin
      {False because we do not want to create it if it doesn’t exists}
      if rRegistryList.OpenKey('QuickView\shell\open\command',False) then
      begin
        StrPCopy( szFileString, iFile );
        lQuikViewPath := rRegistryList.ReadString('');
        { Check for systemroot use and covert to real path }
        if Pos('%SystemRoot%', lQuikViewPath) <> 0 then
        begin
          GetWindowsDirectory( lWindowsDirPath, 255 );
          lQuikViewPath := lWindowsDirPath +
                            Copy(lQuikViewPath,
                            Pos('%SystemRoot%', lQuikViewPath) + 12, 255);
        end;
        StrPCopy( szCmdString, lQuikViewPath );
      end;
      lRetCode:=ShellExecute(0,'open',szCmdString,szFileString, '',SW_SHOW);
      Result := CheckRetCode(lRetCode);
    end else
      Result := False;
  finally
    rRegistryList.CloseKey;
    rRegistryList.Free;
  end; // try
end;  // TryQuickView


//==============================================================================
{ Check the return code from a call to ShellExecute.  Display a message dialog
     if the call failed }
function CheckRetCode(iRetCode: integer): Boolean;
var
  lstMessage     : string;
begin
  Result := True;
  case iRetCode of
    0                      : lstMessage:=ResStr_OSOutOfMemory;
    ERROR_FILE_NOT_FOUND   : lstMessage:=ResStr_FileNotFound;
    ERROR_PATH_NOT_FOUND   : lstMessage:=ResStr_PathNotFound;
    ERROR_BAD_FORMAT       : lstMessage:=ResStr_BadFormat;
    SE_ERR_ACCESSDENIED    : lstMessage:=ResStr_AccessDenied;
    SE_ERR_ASSOCINCOMPLETE : lstMessage:=ResStr_AssocIncomplete;
    SE_ERR_DDEBUSY         : lstMessage:=ResStr_DDEBusy;
    SE_ERR_DDEFAIL         : lstMessage:=ResStr_DDEFail;
    SE_ERR_DDETIMEOUT      : lstMessage:=ResStr_DDETimeOut;
    SE_ERR_DLLNOTFOUND     : lstMessage:=ResStr_DllNotFound;
    SE_ERR_OOM             : lstMessage:=ResStr_ERRORM;
    SE_ERR_SHARE           : lstMessage:=ResStr_ERRShare;
  end; // case
  if lstMessage<>'' then
    Raise EShellError.Create(lstMessage);
end;  // CheckRetCode


end.
