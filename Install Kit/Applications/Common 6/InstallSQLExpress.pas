{===============================================================================
  Unit:        InstallSQLExpress.pas

  Defines:     TInstallSQLExpress

  Description: Fucntionality to install SQLExpress on target machine.

  Model:

  Created:     March 2009

  Last revision information:
    $Revision: 6 $
    $Date: 10/02/10 17:25 $
    $Author: Simonlewis $

===============================================================================}

unit InstallSQLExpress;

interface

uses
  Sysutils, Classes, GeneralFunctions, Windows, TextMessages, APIUtils, Forms,
  Settings;

const
  I_IN_PROGRESS = 259; //Indicates that the installation is in progress
  I_SUCCESS = 0;       //Indicates that the installation was successful

type
  TInstallSQLExpress = class(TObject)
  private
    FProcessInfo: TProcessInformation;
    FSettings: TSettings;
    function GetMessageFromLog(line: string): String;
    function GetLocalSystemAccountName: String;
  public
    constructor Create(ASettings: TSettings);
    function DoInstall(AShowInfo: Boolean; const ACollation, ADataDir, AInstanceName: String):
        Boolean;
    function GetError: String;
    function IsInstallFinished(var AExitCode: Cardinal): Boolean;
  end;
  
//==============================================================================
implementation

uses
  SetupConstants, StrUtils, Registry, Dialogs, WbemScripting_TLB;

const
  DEFAULT_PARAMS = 'start /wait '
      + STR_SQLEXPRESS_FILE
      + ' -q /q%s '
      + 'ADDLOCAL=SQL_Engine '
      + 'COLLATION="%s" '
      + 'SQLACCOUNT="%s" '             // e.g. NT AUTHORITY\System (if English).
      + 'SQLBROWSERACCOUNT="%s" '
      + 'REBOOTPROMPT=Supress '
      + 'REBOOT=ReallySuppress '
      + 'SQLAUTOSTART=1 '
      + 'SQLBROWSERAUTOSTART=1 '
      + 'SECURITYMODE=SQL '
      + 'SAPWD="%s"';

resourcestring
  ResStr_LogError = 'Error String    : '; // Needs to match the string in the output
                                          // file precisely, including spaces.

{-==============================================================================
    TInstallSQLExpress
===============================================================================}
{-------------------------------------------------------------------------------
}
constructor TInstallSQLExpress.Create(ASettings: TSettings);
begin
  FSettings := ASettings;
end;  // TInstallSQLExpress.Create

{-------------------------------------------------------------------------------
  Gets the full name of the local system account in the form DOMAIN\USERNAME, in
  a way that works independently of the language of the current Windows version.
}
function TInstallSQLExpress.GetLocalSystemAccountName(): String;
var
  Locator: ISWbemLocator;
  Services: ISWbemServices;
  WmiObject: ISWbemObject;
  UserName, Domain: String;
begin
  Locator := CoSWbemLocator.Create();
  Services := Locator.ConnectServer('', 'root\CIMV2', '', '', '', '', 0, nil);
  WmiObject := Services.Get('Win32_SID.SID=''S-1-5-18''', 0, nil);

  Domain := WmiObject.Properties_.Item('ReferencedDomainName', 0).Get_Value();
  UserName := WmiObject.Properties_.Item('AccountName', 0).Get_Value();
  Result := Domain + '\' + UserName;
end;

{-------------------------------------------------------------------------------
}
function TInstallSQLExpress.DoInstall(AShowInfo: Boolean; const ACollation, ADataDir, AInstanceName:
    String): Boolean;
var
  lParams: String;
  lReturnCode: Cardinal;
  lShowMode: String;
begin
  FSettings.SQLExpressInstallState := isInstalling;
  
  // Select the appropriate command line parameter for the required GUI
  if AShowInfo then
    lShowMode := 'b'  // basic GUI
  else
    lShowMode := 'n'; // no GUI
  lParams := Format(
      DEFAULT_PARAMS,
      [lShowMode,
       ACollation,
       GetLocalSystemAccountName(),
       GetLocalSystemAccountName(),
       FSettings.Password]);
  if AInstanceName <> '' then
    lParams := lParams + ' INSTANCENAME=' + AInstanceName;

  if ADataDir <> '' then
    // Make sure the path ends with a backslash.
    lParams := lParams + ' INSTALLSQLDATADIR="' + IncludeTrailingPathDelimiter(ADataDir) + '"';

  lReturnCode := WinExec32(
      '"' + FSettings.RootFolder + STR_SQLEXPRESS_SETUP + '" ' + lParams,
      FSettings.RootFolder + STR_SQLEXPRESS_PATH,
      SW_SHOWNORMAL,
      FProcessInfo);
  
  Result := ((lReturnCode = I_IN_PROGRESS) or (lReturnCode = I_SUCCESS));
end;  // TInstallSQLExpress.DoInstall

{-------------------------------------------------------------------------------
}
function TInstallSQLExpress.GetError: String;
var
  lFileStream: TFileStream;
  lStreamSize: Integer;
  WS: Widestring;
  S: String;
  i: Integer;
  lLog: TStringList;
  path: string;
begin
  Result := ResStr_NoMoreInfo;
  // Gets the path to the log file, using the program files location taken from
  // the registry.
  with TRegistry.Create do
    try
      RootKey := HKEY_LOCAL_MACHINE;
      OpenKey(STR_WIN_REGISTRY, True);
      path := Format(STR_INSTALL_LOG_PATH, [ReadString(STR_PROGRAM_FILES)]);
    finally
      Free;
    end;
    
  if FileExists(path) then begin
    lFileStream := TFileStream.Create(path, fmOpenRead);
    lStreamSize := lFileStream.Size;
    SetLength(WS, lStreamSize div 2);
    lFileStream.Read(WS[1], lStreamSize);
    lFileStream.Free;
    S := WS;
    // search for known errors so we can have a decent error message

    if Pos(ResStr_InvalidInstance, S) > 0 then
      Result := ResStr_InvalidInstanceReason
    else begin
      // other errors must extract text from the log
      lLog := TStringlist.Create;
      lLog.Text := S;
      for i := 1 to lLog.Count - 1 do begin
        if Pos(ResStr_LogError, lLog[i]) > 0 then begin
          Result := GetMessageFromLog(lLog[i]);
          Break;
        end;
      end;
      lLog.Free;
    end;
  end;
end;  // TInstallSQLExpress.GetError

{-------------------------------------------------------------------------------
}
function TInstallSQLExpress.GetMessageFromLog(line: string): String;
begin
  Result := RightStr(line, StrLen(PAnsiChar(line)) - StrLen(PAnsiChar(ResStr_LogError)));
end;  // TInstallSQLExpress.GetMessageFromLog 

{-------------------------------------------------------------------------------
}
function TInstallSQLExpress.IsInstallFinished(var AExitCode: Cardinal): Boolean;
begin
  Result := IsProcessFinished(FProcessInfo, AExitCode);
end;  // TInstallSQLExpress.IsInstallFinished

end.
