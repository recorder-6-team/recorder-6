unit SQLList;

interface

uses
  SysUtils, Classes, Dialogs;

type
  TAPIException = class (Exception);

function SQLAllocHandle(HandleType: smallint;
  InputHandle: THandle; OutputHandlePtr: Pointer): smallint; stdcall;

function SQLSetEnvAttr(EnvironmentHandle: THandle; Attribute: integer;
  Value: Pointer; StringLength: Integer): SmallInt; stdcall;

function SQLBrowseConnect(hdbc: THandle; szConnStrIn: String;
  cbConnStrIn: Integer; szConnStrOut: PChar; const cbConnStrOutMax: Integer;
  var pcbconnstrout: Integer): SmallInt; stdcall;

function SQLFreeHandle(HandleType: SmallInt; Handle: THandle): SmallInt; stdCall;

function SQLDisconnect(hdbc: THandle): SmallInt; stdCall;

function SQLError(henv: Integer; hdbc: THandle; hstmt: integer; szSqlState: PChar;
  pfNativeError: integer;  szErrorMsg: PChar;  cbErrorMsgMax: Integer;
  pcbErrorMsg: Integer): Integer; stdCall;

function SQLFreeConnect(hdbc: THandle): Integer; stdCall;

function SQLFreeEnv(henv: integer): Integer; stdCall;

procedure PopulateServerList(AItems: TStrings);
function StServerList: String;

//==============================================================================
implementation

uses
  StrUtils;

resourcestring
  ResStr_OddEventOccured = 'Something Odd has occurred';
  ResStr_InvalidHandle = 'Invalid Handle';
  ResStr_SQLError = 'SQL Error';
  ResStr_UnknownODBCError = 'Unknown ODBC Call Result';

const
  SQL_ERROR = -1;
  SQL_INVALID_HANDLE = -2;
  SQL_NEED_DATA = 99;
  SQL_NO_DATA_FOUND = 100;
  SQL_SUCCESS = 0;
  SQL_SUCCESS_WITH_INFO = 1;

  //  SQLError defines
  SQL_NULL_HENV : integer = 0;
  SQL_NULL_HDBC : integer = 0;
  SQL_NULL_HSTMT : integer = 0;
  SQL_NULL_HANDLE : integer = 0;

  SQL_HANDLE_ENV : smallint = 1;
  SQL_HANDLE_DBC : smallint = 2;
  SQL_HANDLE_STMT : smallint = 3;
  SQL_HANDLE_DESC : smallint = 4;
  SQL_ATTR_ODBC_VERSION : integer = 200;
  SQL_OV_ODBC3  = Ptr(3);

function SQLFreeEnv; external 'odbc32.dll';
function SQLFreeConnect; external 'odbc32.dll';
function SQLError; external 'odbc32.dll';
function SQLDisconnect;external 'odbc32.dll';
function SQLFreeHandle;external 'odbc32.dll';
function SQLBrowseConnect;external 'odbc32.dll';
function SQLSetEnvAttr;external 'odbc32.dll';
function SQLAllocHandle;external 'odbc32.dll';

//==============================================================================
{Description:   Determine the success of returning a server name
'Created:       25/07/2002}
procedure SQLCheck(Const iSQLResult: SmallInt);
begin
  Case iSQLResult of
    SQL_SUCCESS: ;

    SQL_SUCCESS_WITH_INFO:
      raise TAPIException.Create(ResStr_OddEventOccured);
    SQL_INVALID_HANDLE:
      raise TAPIException.Create(ResStr_InvalidHandle);
    SQL_ERROR:
      raise TAPIException.Create(ResStr_SQLError);
    Else
      raise TAPIException.Create(ResStr_UnknownODBCError);
  end;
end;

{Description: gets a list of servers present on the network
'Created: 25/07/02}
{adapted by Polly Shaw for Delphi.}
function StServerList: String;
var
    lhndEnv : THandle;
    lhndDBc : THandle;
    lstCon : String;
    lszConOut : PChar;
    liConOut : Integer;
    lichBegin : Integer;
    lichEnd : Integer;
    lstOut: String;
    lstFullList : String;
begin
  lszConOut := nil;
  try
    GetMem(lszConOut, 2); // initialise to a string length 2
    SQLCheck(SQLAllocHandle(SQL_HANDLE_ENV, SQL_NULL_HANDLE, @lhndEnv));
    SQLCheck(SQLSetEnvAttr(lhndEnv, SQL_ATTR_ODBC_VERSION, SQL_OV_ODBC3, 0));
    SQLCheck(SQLAllocHandle(SQL_HANDLE_DBC, lhndEnv, @lhndDBc));
    lstCon:='DRIVER=SQL SERVER';

    // Get the size of the buffer to create
    liConOut := 0;
    SQLBrowseConnect(lhndDBc, lstCon, length(lstCon), lszConOut ,2 , liConOut);

    //Reinitialise it to the proper length string
    FreeMem(lszConOut);
    GetMem(lszConOut, liConOut + 1);

    //Get the actual server list
    SQLBrowseConnect(lhndDBc, lstCon, Length(lstCon) + 1, lszConOut, liConOut + 1, liConOut);
    lstFullList := lszConOut;

    // Parse out the server list
    lichBegin := PosEx('{', lstFullList, Pos(lstFullList, 'Server='));
    lstOut    := RightStr(lstFullList, Length(lstFullList) - lichBegin);
    lichEnd   := Pos('}', lstOut);
    lstOut    := LeftStr(lstOut, lichEnd - 1);
    Result    := lstOut;
  finally
    // Disconnect, free the connection handle, then
    // free the environment handle.
    FreeMem(lszConOut);
    SQLDisconnect(lhndDBc);
    SQLFreeHandle(SQL_HANDLE_DBC, lhndDBc);
    SQLFreeHandle(SQL_HANDLE_DBC, lhndEnv);
  end;
End;

//------------------------------------------------------------------------------
{Description: Gets a collection containing the servers present on the network
                        A local database is always returned, since the ODBC browse connection
                       call is unreliable.
Created: 25/07/02}
{adapted by Polly Shaw for Delphi.}
procedure PopulateServerList(AItems: TStrings);
var lstServers: String;
begin
  lstServers := '';
  lstServers := StServerList;

  if not Assigned(AItems) then
    AItems := TStringList.Create
  else
    AItems.Clear;

  AItems.CommaText := lstServers;

  // If no servers listed, then add 'local' because this is never detected without a hub attached
  If AItems.Count = 0 then
    AItems.Add('(local)');
end;

//==============================================================================
end.

