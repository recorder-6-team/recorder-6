{-------------------------------------------------------------------------------
  Unit:         APIUtils.pas

  Defines:      EAPIError
                TAPIObject         Basic handle to API object.
                TAPIMutex          Basic Unnamed Mutex.
                TEventObject       Basic Event Object.
                TNamedEvent
                TSemaphore
                TApplicationEvent  Application event tracker base class.
                TGlobalData
                ETITCError
                TITCMsg            Record structure.
                TITCMsgList        Record structure.
                TInterThreadComms
                TTaggedThread

  Description:  This defines some basic wrapper objects for the win32 api, and
                some error checking. It also has an inter thread comms object,
                to allow threads to communicate asyncrhonously.
                The Mutex and Event objects wrap the OS objects.
                The TTagged Thread object automatically registers with ITC.

  Author:       Toby Milne
  Created:      18 September 1996

  Additional Changes:
    12/02/98: Rationalized object hierarchy by making a tapi object which holds
              the handle. This means destructors are common.
              Made TnamedEvent a subclass of teventobject, to share code.
    10/04/98: Added TSemaphore.
    20/04/98: Added timed wait to api object.
    19/11/98: Added gfnGetTempPath (AJWK)
    18/02/99: Added ErrorCode and CreateFromCode to EAPIError (AJWK)

  Last revision information:
    $Revision: 22 $
    $Date: 7/05/09 16:50 $
    $Author: Ericsalmon $

  Copyright © Dorset Software Services Ltd, 2003

-------------------------------------------------------------------------------}

unit ApiUtils;

{$I DelphiVersions.Inc}

interface

uses
  SysUtils, Classes, Windows, ComObj, ShellAPI, Messages;

const
  STOP_MSG = 1;

type
  EAPIError = class(Exception)
  private
    FErrorCode: Integer;
  public
    constructor CreateFromCode(win32ErrorCode: Integer);
    property ErrorCode: Integer read FErrorCode;
  end;  // class EAPIError

  EMutex = class(EAPIError);

  {basic handle to API OBJECT}
  TAPIObject = class
  private
    FHandle : THandle;
  public
    function  TimedWait(iTime : integer) : boolean;
    Constructor Create;
    Destructor Destroy; override;
    property Handle : THandle read FHandle;
  end;{tapiobject}

  {Basic Unnamed Mutex}
  TAPIMutex = class(TAPIObject)
  public
    procedure Get;
    procedure Release;
    constructor Create;
    constructor CreateOrGetExisting(const stName : AnsiString);
  end;{TAPIMutex}

  {Basic Event Object}
  TEventObject=class(TAPIObject)
  public
    constructor Create(tfManualReset, tfInitialState : boolean);
    constructor CreateEmpty;
    procedure Signal;
    procedure Reset;
    procedure Wait;
    procedure Pulse;
    function Test : boolean;
  end;{TEventObject}

  {named event}
  TNamedEvent=class(TEventObject)
  public
    constructor Create(tfManualReset, tfInitialState : boolean;stName : AnsiString);
    constructor CreateExisting(stName : AnsiString);
  end; {TNamedEvent}

  {semaphore}
  TSemaphore=class(TAPIObject)
  public
    procedure Release;
    constructor Create(const iiInitialCount,iiMaxCount : integer);
  end;

  {Application Event Tracker Base Class}
  TApplicationEvent = class
  private
    FEvent : TEventObject;
    FtfAppExit : boolean;
    function GetExitHandle : integer;
  public
    procedure AppExit;
    property AppExiting : boolean read FtfAppExit;
    property ExitHandle : integer read GetExitHandle;
    constructor Create;
    destructor Destroy; override;
  end;{TApplicationEvent}

  {TGlobalData}
  TGlobalData=class
  private
    FDataMutex : TAPIMutex;
  protected
    procedure WaitForAccess;
    procedure ReleaseAccess;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  {TITC - inter thread comms}

  ETITCError=class(Exception);

  TITCMsg = record
    FiSeq : integer;
    FiCode : integer;
    FpData :  pointer;
    FiSize : integer;
  end;{titcmsg}

  TITCMsgList = record
    FiTag : integer;
    FEvent : TEventObject;
    FList : TList;
  end;

  pTM = ^TITCMsg;
  pTML = ^TITCMsgList;

  TInterThreadComms= class
  private
    FMsgLists : TList;
    FUpdateMutex : TAPIMutex;
    FiTagMax : integer;
    FiMaxSeq : integer;
    procedure TITCError(stMsg : AnsiString);

    function GetEvent(index : integer) : integer;
    function GetCount(index : integer) : integer;

    procedure WaitForMutex;
    procedure ReleaseMutex;
    function  GetMsgListForTag(iTag : integer) : TList;
    function  GetListForTag(iTag : integer) : pTML;
    procedure ClearMessages(aList : TList);
    procedure CheckEvent(iTag : integer);
    procedure ClearLists;
    procedure RemoveTag(iTag : integer);
    procedure BroadCast(Msg : TITCMsg);
  public
    function AddThread : integer;
    procedure RemoveThread(index : integer);

    function SendThreadMessage(iTag : integer;Msg : TITCMsg): integer;
    function GetThreadMessage(iTag : integer) : TITCMsg;
    procedure BroadcastStop;

    constructor Create;
    destructor Destroy; override;

    property Event[index : integer] : integer read GetEvent;
    property MessageCount[index : integer] : integer read GetCount;
  end;{titc}

  {Tagged Thread Object}
  TTaggedThread= class(TThread)
  private
    FiTag : integer;
  public
    property ITCTag : integer read FiTag;
    constructor Create(tfCreateSuspended : boolean);
    destructor Destroy; override;
  end;

const
  {thread messages}
  TC_NO_MSG = 0;
  INVALID_MAP_HANDLE = 0;

var
   gAppITC : TInterThreadComms;

// Additional functions.
{Get Last Error & make a string}
function gpcGetOSError: AnsiString;

{checking functions}
procedure gpcNilCheck(iResult: integer);
procedure gpcAPIResultCheck(iResult : integer);
procedure gpcAPICheck(tfvalue : Boolean);
procedure gpcHandleCheck(iHandle : THandle);
procedure gpcWaitCheck(iWaitReturn : LongWord);
function gpcPendingIO : boolean;

function gpcGetComputerName : AnsiString;

function gfnGetTempPath: AnsiString;

{$IFDEF DELPHI7UP}
// Function to copy files. File name can use wildcard.
function CopyFiles(const AFileName, ADestPath: String): Boolean;
// Function to remove a single file or a complete folder and its content.
function RemoveFileOrFolder(const AName: String): Boolean;
// Equivalent to Borland-supplied RegisterComServer function, but do the opposite.
procedure UnregisterComServer(const DLLName: String);
{$ENDIF}

// Functions to launch an external process, waiting for it to finish or not
function WinExecAndWait32(const ACommandLine, AWorkDir: String; AVisibility : Integer): Cardinal;
function WinExec32(const ACommandLine, AWorkDir: String; AVisibility: Integer): Cardinal; overload;
// The following is useful if WinExecAndWait32 doesn't work for a particular process
// but app still need to be able to wait for it to finish. AProcessInfo is the link to
// the launched process and can be used to determine when it finishes.
function WinExec32(const ACommandLine, AWorkDir: String; AVisibility: Integer;
  var AProcessInfo: TProcessInformation): Cardinal; overload;
// Checks a process is finished
function IsProcessFinished(AProcessInfo: TProcessInformation; var AExitCode: Cardinal): Boolean;

// Functions to trigger a reboot.
procedure RebootMachine;
function SetPrivilegeForReboot(AEnable: Boolean): Boolean;

//==============================================================================
implementation

const
  {TAGGED THREAD & ITC}
  BROADCAST = 0;

  UNIT_NAME      = 'APIUtils';
  VERSION_STRING = '0.2';
  ERROR_BUFFER=2048;

resourcestring
  ResStr_TagUnknown  = 'Unknown Thread Tag.';
  ResStr_TagMax      = 'Maximum Tag Limit.';
  ResStr_InvalidHandle = 'Invalid API Handle';
  ResStr_APIError      = 'Error %d: %s';

//==============================================================================
{------------------------------------------------------------------------------}
{Prog stuff}
{------------------------------------------------------------------------------}
{------------------------------------------------------------------------------}
function MakeLangID(lang,sublang: word) : integer;
begin
  {#define MAKELANGID(p, s) ((((WORD) (s)) << 10) | (WORD) (p))}
  Result:=lang or (sublang shl 10);
end;

{------------------------------------------------------------------------------}
{------------------------------------------------------------------------------}
function gpcGetOSError: AnsiString;
var
 liErrorNum : integer;
begin
  liErrorNum:=GetLastError;
  {get error message from system}
  Result:='Error : '+IntToStr(liErrorNum)+' : '+SysErrorMessage(liErrorNum);
end;

{------------------------------------------------------------------------------}
{------------------------------------------------------------------------------}
procedure gpcNilCheck(iResult: integer);
begin
  if iResult=0 then gpcApiCheck(False);
end;

{------------------------------------------------------------------------------}
{------------------------------------------------------------------------------}
procedure gpcAPIResultCheck(iResult : integer);
begin
  if iResult <> ERROR_SUCCESS then
    raise EAPIError.CreateFromCode(iResult);
end;

{------------------------------------------------------------------------------}
{------------------------------------------------------------------------------}
procedure gpcAPICheck(tfValue: Boolean);
begin
  if not tfValue then raise EAPIError.CreateFromCode(GetLastError);
end;

{------------------------------------------------------------------------------}
{------------------------------------------------------------------------------}
procedure gpcHandleCheck(iHandle : THandle);
begin
  if iHandle=INVALID_HANDLE_VALUE then gpcAPICheck(False);
end;

{------------------------------------------------------------------------------}
{------------------------------------------------------------------------------}
procedure gpcWaitCheck(iWaitReturn : LongWord);
begin
  if iWaitReturn=WAIT_FAILED then gpcAPICheck(False);
end;

{------------------------------------------------------------------------------}
{------------------------------------------------------------------------------}
function gpcPendingIO : boolean;
begin
  Result:=(GetLastError=ERROR_IO_PENDING);
end;

{------------------------------------------------------------------------------}
{------------------------------------------------------------------------------}
function gpcGetComputerName : AnsiString;
var
  lszBuffer: array[0..256] of char;
  liSize : DWORD;
begin
  liSize:=255;
  Windows.GetComputerName(lszBuffer,lisIze);
  Result:=StrPas(lszBuffer);
end;


// =============================================================================
// EAPIError
// =============================================================================
constructor EAPIError.CreateFromCode(win32ErrorCode: Integer);
begin
  inherited Create(Format(ResStr_APIError, [win32ErrorCode, SysErrorMessage(win32ErrorCode)]));
  FErrorCode := win32ErrorCode;
end;  // EAPIError.CreateFromCode

{------------------------------------------------------------------------------}
{api object}
{------------------------------------------------------------------------------}
constructor TAPIObject.Create;
begin
  inherited Create;
  FHandle:=INVALID_HANDLE_VALUE;
end;

{------------------------------------------------------------------------------}
{------------------------------------------------------------------------------}
destructor TAPIObject.Destroy;
begin
  {if handle not invalid then close}
  if FHandle<>INVALID_HANDLE_VALUE then
    gpcAPICheck(CloseHandle(FHandle));
  inherited Destroy;
end;

{------------------------------------------------------------------------------}
{------------------------------------------------------------------------------}
function TAPIObject.TimedWait(iTime : integer) : boolean;
var
  liRet : integer;
begin
  {check handle valid}
  gpcHandleCheck(FHandle);

  liRet:=WaitForSingleObject(FHandle,iTime);
  gpcWaitCheck(liRet);
  Result:=(liRet=WAIT_OBJECT_0);
end;

{------------------------------------------------------------------------------}
{mutex object}
{------------------------------------------------------------------------------}
procedure TAPIMutex.Get;
begin
  gpcWaitCheck(WaitForSingleObject(FHandle,INFINITE));
end;

{------------------------------------------------------------------------------}
{------------------------------------------------------------------------------}
procedure TAPIMutex.Release;
begin
  gpcAPICheck(ReleaseMutex(FHandle));
end;

{------------------------------------------------------------------------------}
{------------------------------------------------------------------------------}
constructor TAPIMutex.Create;
begin
  inherited Create;
  FHandle:=CreateMutex(nil,False,nil);
  gpcHandleCheck(FHandle);
end;

{------------------------------------------------------------------------------}
{------------------------------------------------------------------------------}
constructor TAPIMutex.CreateOrGetExisting(const stName : AnsiString);
begin
  inherited Create;
  {create handle with name}
  FHandle:=CreateMutex(nil,False,PChar(stName));
  gpcHandleCheck(FHandle);
end;

{------------------------------------------------------------------------------}
{TEventObject}
{------------------------------------------------------------------------------}
function TEventObject.Test : boolean;
var
  liWR : integer;
begin
  liWR:=WaitForSingleObject(Handle,0);
  gpcWaitCheck(liWR);
  Result:=(liWR=WAIT_OBJECT_0)
end;

{------------------------------------------------------------------------------}
{------------------------------------------------------------------------------}
procedure TEventObject.Pulse;
begin
  gpcApiCheck(PulseEvent(Handle));
end;

{------------------------------------------------------------------------------}
{------------------------------------------------------------------------------}
procedure TEventObject.Wait;
begin
  gpcWaitCheck(WaitForSingleObject(Handle,INFINITE));
end;

{------------------------------------------------------------------------------}
{------------------------------------------------------------------------------}
procedure TEventObject.Reset;
begin
  gpcApiCheck(ResetEvent(FHandle));
end;

{------------------------------------------------------------------------------}
{------------------------------------------------------------------------------}
procedure TEventObject.Signal;
begin
  gpcApiCheck(SetEvent(FHandle));
end;

{------------------------------------------------------------------------------}
{------------------------------------------------------------------------------}
constructor TEventObject.Create(tfManualReset, tfInitialState : boolean);
begin
  inherited Create;
  FHandle:=CreateEvent(nil, {no sid}
                       tfManualReset, {manual reset}
                       tfInitialState, {initial non signal}
                       nil); {noname}
  gpcNilCheck(FHandle);
end;

{------------------------------------------------------------------------------}
{------------------------------------------------------------------------------}
constructor TEventObject.CreateEmpty;
begin
  {simply call inherited}
  inherited Create;
end;

{------------------------------------------------------------------------------}
{TNamedEvent}
{------------------------------------------------------------------------------}
constructor TNamedEvent.Create(tfManualReset, tfInitialState : boolean;stName : AnsiString);
var
  lszName: array[0..256] of char;
begin
  inherited CreateEmpty;
  StrPCopy(lszName,stName);
  FHandle:=CreateEvent(nil, {no sid}
                       tfManualReset, {manual reset}
                       tfInitialState, {initial non signal}
                       lszName); {name}
  gpcNilCheck(FHandle);
end;

{------------------------------------------------------------------------------}
{------------------------------------------------------------------------------}
constructor TNamedEvent.CreateExisting(stName : AnsiString);
var
 lszName: array[0..256] of char;
begin
  inherited CreateEmpty;
  StrPCopy(lszName,stName);
  FHandle:=OpenEvent(EVENT_ALL_ACCESS,
                     False, {noinherite}
                     lszName); {name}
  gpcNilCheck(FHandle);
end;

{------------------------------------------------------------------------------}
{TSemaphore}
{------------------------------------------------------------------------------}
procedure TSemaphore.Release;
begin
  gpcApiCheck(ReleaseSemaphore(FHandle,1,nil));
end;

{------------------------------------------------------------------------------}
{------------------------------------------------------------------------------}
constructor TSemaphore.Create(const iiInitialCount,iiMaxCount : integer);
begin
  inherited Create;
  {create unnamed semaphore}
  FHandle:=CreateSemaphore(nil,iiInitialCount,iiMaxCount,nil);
  gpcNilCheck(FHandle);
end;

{------------------------------------------------------------------------------}
{application event object}
{------------------------------------------------------------------------------}
procedure TApplicationEvent.AppExit;
begin
  FtfAppExit:=True;
  FEvent.Signal;
end;

{------------------------------------------------------------------------------}
{------------------------------------------------------------------------------}
function TApplicationEvent.GetExitHandle : integer;
begin
  Result:=FEvent.Handle;
end;

{------------------------------------------------------------------------------}
{------------------------------------------------------------------------------}
constructor TApplicationEvent.Create;
begin
  inherited Create;
  FEvent:=TEventObject.Create(True,False);
end;
{------------------------------------------------------------------------------}
{------------------------------------------------------------------------------}
destructor TApplicationEvent.Destroy;
begin
  FEvent.Destroy;
  inherited Destroy;
end;

{------------------------------------------------------------------------------}
{TITC code}
{------------------------------------------------------------------------------}
procedure TInterThreadComms.TITCError(stMsg : AnsiString);
begin
  raise ETITCError.Create(stMsg);
end;

{------------------------------------------------------------------------------}
{------------------------------------------------------------------------------}
function TInterThreadComms.GetEvent(index : integer) : integer;
var
  lThisList : TITCMsgList;
begin
  lThisList:=TITCMsgList(GetListForTag(index)^);
  Result:=lThisList.FEvent.Handle;
end;

{------------------------------------------------------------------------------}
{------------------------------------------------------------------------------}
function TInterThreadComms.GetCount(index : integer) : integer;
var
  lThisList : TITCMsgList;
begin
  WaitForMutex;
  lThisList:=TITCMsgList(GetListForTag(index)^);
  Result:=lThisList.FList.Count;
  ReleaseMutex;
end;

{------------------------------------------------------------------------------}
{------------------------------------------------------------------------------}
procedure TInterThreadComms.WaitForMutex;
begin
  FUpdateMutex.Get;
end;

{------------------------------------------------------------------------------}
{------------------------------------------------------------------------------}
procedure TInterThreadComms.ReleaseMutex;
begin
  FUpdateMutex.Release;
end;

{------------------------------------------------------------------------------}
{------------------------------------------------------------------------------}
function TInterThreadComms.AddThread : integer;
var
  lpNewMsgList : pTML;
begin
  if FiTagMax=High(Integer) then TITCError(ResStr_TagMax);
  WaitForMutex;
  lpNewMsgList:=nil;
  try
    lpNewMsgList:=New(pTML);
    with lpNewMsgList^ do
    begin
      Result:=FiTagMax;
      FiTag:=FiTagMax;
      Inc(FiTagMax);
      FEvent:=nil;
      FList:=nil;
      FEvent:=TEventObject.Create(False,False);
      FList:=TList.Create;
    end;
    FMsgLists.Add(lpNewMsgList);
    lpNewMsgList:=nil;
  finally
    if Assigned(lpNewMsgList) then
    begin
      if Assigned(lpNewMsgList^.FEvent) then lpNewMsgList^.FEvent.Free;
      if Assigned(lpNewMsgList^.FList) then lpNewMsgList^.FList.Free;
      Dispose(lpNewMsgList);
    end;
    ReleaseMutex;
  end;{try}
end;

{------------------------------------------------------------------------------}
{------------------------------------------------------------------------------}
procedure TInterThreadComms.RemoveThread(index : integer);
var
  lpThisList : pTML;
begin
  WaitForMutex;
  lpThisList:=GetListForTag(index);
  try
    if Assigned(lpThisList^.FEvent) then lpThisList^.FEvent.Free;
    if Assigned(lpThisList^.FList) then
    begin
      ClearMessages(lpThisList^.FList);
      lpThisList^.FList.Free;
    end;
    RemoveTag(index);
  finally
    ReleaseMutex;
  end;
end;

{------------------------------------------------------------------------------}
{------------------------------------------------------------------------------}
procedure TInterThreadComms.BroadCast(Msg : TITCMsg);
var
  liTag,liLoop : integer;
  lpTML : pTML;
begin
  for liLoop:=0 to FMsgLists.Count-1 do
  begin
    lpTML:=pTML(FMsgLists.Items[liLoop]);
    liTag:=lpTML^.FiTag;
    SendThreadMessage(liTag,Msg);
  end;
end;

{------------------------------------------------------------------------------}
{------------------------------------------------------------------------------}
procedure TInterThreadComms.ClearMessages(aList : TList);
var
  liLoop : integer;
begin
  try
    WaitForMutex;
    for liLoop:=0 to aList.Count-1 do
    begin
      if Assigned(aList.Items[0]) then
      begin
        Dispose(pTM(aList.Items[0]));
        aList.Delete(0);
      end;
    end;
  finally
    ReleaseMutex;
  end;{try}
end;

{------------------------------------------------------------------------------}
{------------------------------------------------------------------------------}
procedure TInterThreadComms.ClearLists;
var
  liLoop : integer;
  lpMsgList : pTML;
begin
  try
    WaitForMutex;
    for liLoop:=0 to FMsgLists.Count-1 do
    begin
      if Assigned(FMsgLists.Items[0]) then
      begin
        lpMsgList:=pTML(FMsgLists.Items[0]);
        RemoveThread(lpMsgList.FiTag);
      end;
    end;
  finally
    ReleaseMutex;
  end;{try}
end;

{------------------------------------------------------------------------------}
{------------------------------------------------------------------------------}
procedure TInterThreadComms.CheckEvent(iTag : integer);
var
  lpThisList : pTML;
begin
  WaitForMutex;
  lpThisList:=GetListForTag(iTag);
  try
    with lpThisList^ do
      if FList.Count>0 then FEvent.Signal;
  finally
    ReleaseMutex;
  end;{try}
end;

{------------------------------------------------------------------------------}
{------------------------------------------------------------------------------}
function TInterThreadComms.SendThreadMessage(iTag : integer;Msg : TITCMsg): integer;
var
  lThisList : TList;
  lpNewMsg : pTM;
begin
  Result:=-1;
  if iTag=0 then
    BroadCast(Msg)
  else begin
    lpNewMsg:=nil;
    try
      WaitForMutex;
      {set seq no.}
      Msg.FiSeq:=FiMaxSeq;
      Result:=FiMaxSeq;
      if (FiMaxSeq=High(Integer)) then TITCError(ResStr_TagMax)
      else Inc(FiMaxSeq);

      lThisList:=GetMsgListForTag(iTag);
      lpNewMsg:=New(pTM);
      lpNewMsg^:=Msg;
      lThisList.Add(lpNewMsg);
      lpNewMsg:=nil;
      CheckEvent(iTag);
    finally
      Dispose(lpNewMsg);
      ReleaseMutex;
    end;{try}
  end;
end;

{------------------------------------------------------------------------------}
{------------------------------------------------------------------------------}
function TInterThreadComms.GetThreadMessage(iTag : integer) : TITCMsg;
var
  lThisList : TList;
begin
  try
    WaitForMutex;
    lThisList:=GetMsgListForTag(iTag);
    if lThisList.Count>0 then
    begin
      Result:=TITCMsg(lThisList.Items[0]^);
      Dispose(pTM(lThisList.Items[0]));
      lThisList.Delete(0);
    end else begin
      Result.FiCode:=TC_NO_MSG;
      Result.FpData:=nil;
      Result.FiSize:=0;
    end;{no msgs}
    CheckEvent(iTag);
  finally
    ReleaseMutex;
  end;{try}
end;

{------------------------------------------------------------------------------}
{------------------------------------------------------------------------------}
procedure TInterThreadComms.BroadCastStop;
var
  lMsg: TITCMsg;
begin
  {broadcast stop msg}
  lMsg.FiCode:=STOP_MSG;
  lMsg.FiSeq:=0;
  lMsg.FiSize:=0;
  lMsg.FpData:=nil;
  SendThreadMessage(0,lMsg);
end;

{------------------------------------------------------------------------------}
{------------------------------------------------------------------------------}
function TInterThreadComms.GetMsgListForTag(iTag : integer) : TList;
var
  lThisList : pTML;
begin
  lThisList:=GetListForTag(iTag);
  Result:=lThisList.FList;
end;

{------------------------------------------------------------------------------}
{------------------------------------------------------------------------------}
function TInterThreadComms.GetListForTag(iTag : integer) : pTML;
var
  liLoop : integer;
  ltfFound : boolean;
begin
  liLoop:=0;
  ltfFound:=False;

  repeat
    Result:=pTML(FMsgLists.Items[liLoop]);
    if Result^.FiTag=iTag then ltfFound:=True;
    Inc(liLoop);
  until (ltfFound) or (liLoop=FMsgLists.Count);
  if not(ltfFound) then TITCError(ResStr_TagUnknown);
end;


{------------------------------------------------------------------------------}
{------------------------------------------------------------------------------}
procedure TInterThreadComms.RemoveTag(iTag : integer);
var
  liLoop : integer;
  ltfFound : boolean;
  lpThisList : pTML;
begin
  liLoop:=0;
  ltfFound:=False;

  repeat
    lpThisList:=pTML(FMsgLists.Items[liLoop]);
    if lpThisList^.FiTag=iTag then
    begin
      ltfFound:=True;
      Dispose(lpThisList);
      FMsgLists.Delete(liLoop);
    end;
    Inc(liLoop);
  until (ltfFound) or (liLoop=FMsgLists.Count);
  if not(ltfFound) then TITCError(ResStr_TagUnknown);
end;

{------------------------------------------------------------------------------}
{------------------------------------------------------------------------------}
constructor TInterThreadComms.Create;
begin
  inherited Create;
  {create autoreset mutex}
  FUpdateMutex:=TAPIMutex.Create;
  FMsgLists:=TList.Create;
  FiTagMax:=1;
end;

{------------------------------------------------------------------------------}
{------------------------------------------------------------------------------}
destructor TInterThreadComms.Destroy;
begin
  ClearLists;
  FMsgLists.Free;
  FUpdateMutex.Free;
  inherited Destroy;
end;

{------------------------------------------------------------------------------}
{TTaggedThread}
{------------------------------------------------------------------------------}
constructor TTaggedThread.Create(tfCreateSuspended : boolean);
begin
  inherited Create(True);
  FiTag:=gAppITC.AddThread;
  if not(tfCreateSuspended) then Resume;
end;

{------------------------------------------------------------------------------}
{------------------------------------------------------------------------------}
destructor TTaggedThread.Destroy;
begin
  gAppITC.RemoveThread(FiTag);
  inherited Destroy;
end;

{------------------------------------------------------------------------------}
{Tglobal data}
{------------------------------------------------------------------------------}
procedure TGlobalData.WaitForAccess;
begin
  FDataMutex.Get;
end;
{------------------------------------------------------------------------------}
{------------------------------------------------------------------------------}
procedure TGlobalData.ReleaseAccess;
begin
  FDataMutex.Release;
end;
{------------------------------------------------------------------------------}
{------------------------------------------------------------------------------}
constructor TGlobalData.Create;
begin
  inherited Create;
  FDataMutex:=TAPIMutex.Create;
end;
{------------------------------------------------------------------------------}
{------------------------------------------------------------------------------}
destructor TGlobalData.Destroy;
begin
  FDataMutex.Free;
  inherited Destroy;
end;

// -----------------------------------------------------------------------------
// gfnGetTempPath
// The path of the current Windows temporary directory (with trailing '\' if
// required).
// -----------------------------------------------------------------------------
function gfnGetTempPath: AnsiString;
var
  bufLen:    Integer;
  tempPath:  AnsiString;
begin
  // get required buffer length
  bufLen := GetTempPath(0, PChar(tempPath));

  // read path
  if bufLen > 0 then begin
    SetLength(tempPath, bufLen);
    bufLen := GetTempPath(bufLen, PChar(tempPath));
  end;  // if bufLen > 0

  // exception in case of failure
  if bufLen = 0 then raise EAPIError.CreateFromCode(GetLastError);

  Result := Copy(tempPath, 1, bufLen);  // removes zero terminator
end;  // gfnGetTempPath

//==============================================================================

{$IFDEF DELPHI7UP}
// Copied and adapted from ComObj.RegisterComServer
procedure UnregisterComServer(const DLLName: String);
type
  TRegProc = function: HResult; stdcall;
const
  RegProcName = 'DllUnregisterServer'; { Do not localize }
var
  Handle: THandle;
  RegProc: TRegProc;
begin
  Handle := SafeLoadLibrary(DLLName);
  if Handle <= HINSTANCE_ERROR then
    raise Exception.CreateFmt('%s: %s', [SysErrorMessage(GetLastError), DLLName]);
  try
    RegProc := GetProcAddress(Handle, RegProcName);
    if Assigned(RegProc) then OleCheck(RegProc) else RaiseLastOSError;
  finally
    FreeLibrary(Handle);
  end;
end;

//------------------------------------------------------------------------------
function CopyFiles(const AFileName, ADestPath: String): Boolean;
var
  FO: TShFileOpStruct;
begin
  FillChar(FO, SizeOf(FO), #0);
  FO.Wnd   := 0;
  FO.wFunc := FO_COPY;
  FO.pFrom := PChar(AFileName + #0#0);
  FO.pTo   := PChar(ADestPath + #0#0);
  FO.fFlags:= FOF_NOCONFIRMATION or FOF_SILENT;
  FO.fAnyOperationsAborted := False;
  FO.hNameMappings := nil;
  Result := ShFileOperation(FO) = 0;
end;

//------------------------------------------------------------------------------
function RemoveFileOrFolder(const AName: String): Boolean;
var lFileOpStruct: TSHFileOpStruct;
    lszFrom      : Array[0..255] of Char;
begin
  FillChar(lszFrom, SizeOf(lszFrom), 0);
  // Set properties
  StrPcopy(lszFrom, ExpandFileName(ExcludeTrailingPathDelimiter(AName)) + #0#0);
  with lFileOpStruct do begin
    wnd := 0;
    wFunc := FO_DELETE;
    pFrom := lszFrom;
    pTo   := nil;
    fFlags:= FOF_NOCONFIRMATION or FOF_SILENT;
    fAnyOperationsAborted := False;
    hNameMappings := nil;
  end;
  // Call API function
  Result := ShFileOperation(lFileOpStruct) = 0;
end;
{$ENDIF}

//==============================================================================
// Execute requested application, and either wait for it to complete or not,
// depending on AWait value.
function WinExecute(const ACommandLine, AWorkDir: String; AVisibility: Integer;
  AWait: Boolean; var AProcessInfo: TProcessInformation): Cardinal;
var
  lzCommandLine: array[0..512] of char;
  lzCurDir     : array[0..255] of char;
  lStartupInfo : TStartupInfo;
  lProcessInfo : TProcessInformation;

  procedure WaitForProcess(AProcessHandle: THandle);
  var lMsg: TMsg;
      lRet: DWORD;
  begin
    repeat
      lRet := MsgWaitForMultipleObjects(
                   1,                { 1 handle to wait on }
                   AProcessHandle,   { the handle }
                   False,            { wake on any event }
                   INFINITE,         { wait without timeout }
                   QS_PAINT or       { wake on paint messages }
                   QS_SENDMESSAGE or { or messages from other threads }
                   QS_TIMER          { or timer messages, extremely useful this one }
              );
      if lRet = WAIT_FAILED then Exit; { can do little here }

      // Find out if there are any messages to be processed
      if lRet = (WAIT_OBJECT_0 + 1) then begin
        { Woke on a message, process paint messages only. Calling
          PeekMessage gets messages send from other threads processed. }
        while PeekMessage(lMsg, 0, WM_PAINT, WM_PAINT, PM_REMOVE) do
          DispatchMessage(lMsg);
        { Woke on a message, process timer messages only. Calling
          PeekMessage gets messages send from other threads processed. }
        while PeekMessage(lMsg, 0, WM_TIMER, WM_TIMER, PM_REMOVE) do
          DispatchMessage(lMsg);
      end;
    until lRet = WAIT_OBJECT_0;
  end; { WaitForProcess }

begin
  StrPCopy(lzCommandLine, ACommandLine);
  StrPCopy(lzCurDir, AWorkDir);
  FillChar(lStartupInfo, Sizeof(lStartupInfo), #0);
  lStartupInfo.cb := Sizeof(lStartupInfo);

  lStartupInfo.dwFlags := STARTF_USESHOWWINDOW;
  lStartupInfo.wShowWindow := AVisibility;
  gpcApiCheck(CreateProcess(nil,
                   lzCommandLine,             { pointer to command line string }
                   nil,                       { pointer to process security attributes }
                   nil,                       { pointer to thread security attributes }
                   false,                     { handle inheritance flag }
                   CREATE_NEW_CONSOLE or      { creation flags }
                   NORMAL_PRIORITY_CLASS,
                   nil,                       { pointer to new environment block }
                   lzCurDir,                  { pointer to current directory name }
                   lStartupInfo,              { pointer to STARTUPINFO }
                   lProcessInfo));            { pointer to PROCESS_INF }

  // Requested to wait until called app has finished executing
  if AWait then begin
    WaitForProcess(lProcessInfo.hProcess);
    GetExitCodeProcess(lProcessInfo.hProcess, Result);
    CloseHandle(lProcessInfo.hProcess);
    CloseHandle(lProcessInfo.hThread);
  end else begin
    GetExitCodeProcess(lProcessInfo.hProcess, Result);
    AProcessInfo := lProcessInfo;
  end;
end;  // WinExecute

{-------------------------------------------------------------------------------
  Execute a program and wait for it to finish }
function WinExecAndWait32(const ACommandLine, AWorkDir: String; AVisibility: Integer): Cardinal;
var lDummy: TProcessInformation;
begin
  Result := WinExecute(ACommandLine, AWorkDir, AVisibility, True, lDummy);
end;  // WinExecAndWait32

{-------------------------------------------------------------------------------
  Execute a program, but doesn't wait for it to finish }
function WinExec32(const ACommandLine, AWorkDir: String; AVisibility: Integer): Cardinal;
var lDummy: TProcessInformation;
begin
  Result := WinExecute(ACommandLine, AWorkDir, AVisibility, False, lDummy);
end;  // WinExec32

{-------------------------------------------------------------------------------
  Execute a program, doesn't wait for it to finish, but return the process information
  so that it can still be tracked. Some applications don't really get on with
  WinExecAndWait, so this is the way around. }
function WinExec32(const ACommandLine, AWorkDir: String; AVisibility: Integer;
  var AProcessInfo: TProcessInformation): Cardinal;
begin
  Result := WinExecute(ACommandLine, AWorkDir, AVisibility, False, AProcessInfo);
end;  // WinExec32

{-------------------------------------------------------------------------------
  Returns true if process is no longer active, i.e. finished. Also returns the
  exit code, in case it is of any use for the calling application.
}
function IsProcessFinished(AProcessInfo: TProcessInformation; var AExitCode: Cardinal): Boolean;
begin
  GetExitCodeProcess(AProcessInfo.hProcess, AExitCode);
  Result := AExitCode <> STILL_ACTIVE;
end;

{-------------------------------------------------------------------------------
  Reboot machine.
}
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
end;

{-------------------------------------------------------------------------------
  Sets privileges under Windows NT ...
}
function SetPrivilegeForReboot(AEnable: Boolean): Boolean;
var lTPPrev, lTP: TTokenPrivileges;
    lToken      : THandle;
    ldwRetLen   : DWord;
begin
  Result := False;
  if not OpenProcessToken(GetCurrentProcess, TOKEN_ADJUST_PRIVILEGES or TOKEN_QUERY, lToken) then
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
end;

//==============================================================================
initialization
  {$IFDEF TITC}
  gAppITC:=TInterThreadComms.Create;
  {$ENDIF}

finalization
  {$IFDEF TITC}
  gAppITC.Free;
  {$ENDIF}

end.
