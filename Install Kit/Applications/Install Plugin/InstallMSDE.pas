{-------------------------------------------------------------------------------
  Unit:        InstallMSDE.pas

  Defines:     TInstallMSDE

  Description: Fucntionality to install MSDE on target machine.

  Created:     February 2003

  Last revision information:
    $Revision: 7 $
    $Date: 24/04/03 12:23 $
    $Author: Ericsalmon $

-------------------------------------------------------------------------------}

unit InstallMSDE;

interface

uses
  Sysutils, Classes, GeneralFunctions, Windows, TextMessages, APIUtils, Forms,
  Settings;

const
  // Windows Installer, silent mode return code 1603 indicates an error occurred.
  I_SILENT_MODE_ERROR_CODE = 1603;

type
  TInstallMSDE = class
  private
    FSettings: TSettings;
    FProcessInfo: TProcessInformation;
    function GetMessageFromLog(ALog: TStringList; ALineNumber: integer): string;
  public
    constructor Create(ASettings: TSettings);
    function GetError: string;
    function DoInstall(AShowInfo: Boolean; const ACollation, ADataDir,
      AInstanceName: string): Boolean;
    function IsInstallFinished(var AExitCode: Cardinal): Boolean;
  end;

//==============================================================================
implementation

const
  DEFAULT_PARAMS = 'blanksapwd=1 /q%s /l* %sinst.log COLLATION="%s" ' +
                   'SECURITYMODE=SQL REBOOTPROMPT=Supress REBOOT=ReallySuppress';

//------------------------------------------------------------------------------
constructor TInstallMSDE.Create(ASettings: TSettings);
begin
  FSettings := ASettings;
end;

{-------------------------------------------------------------------------------
  Do the installation of MSDE.
    ShowInfo - true gives basic installation feedback, false for none
    ACollation - collation sequence
    ADataDir and AInstanceName - install parameters - leave blank for default
  Created : 14/02/2003 }
function TInstallMSDE.DoInstall(AShowInfo: boolean; const ACollation,
  ADataDir, AInstanceName: String): Boolean;
var
  lParams : string;
  lReturnCode : cardinal;
  lShowMode : string;
begin
  FSettings.MSDEInstallState := isInstalling;
  
  // Select the appropriate command line parameter for the required GUI
  if AShowInfo then
    lShowMode := 'b'  // basic GUI
  else
    lShowMode := 'n'; // no GUI
  lParams := Format(DEFAULT_PARAMS, [lShowMode, GetWindowsTempDir, ACollation]);
  if AInstanceName <> '' then
    lParams := lParams + ' INSTANCENAME=' + AInstanceName;

  if ADataDir <> '' then
    // Make sure the path ends with a backslash.
    lParams := lParams + ' DATADIR="' + IncludeTrailingPathDelimiter(ADataDir) + '"';

  lReturnCode := WinExec32('"' + ExtractFilePath(Application.ExeName) + 'MSDE\Setup.exe" ' + lParams,
                           ExtractFilePath(Application.ExeName) + 'MSDE\', SW_SHOWNORMAL,
                           FProcessInfo);

  Result := (lReturnCode <> I_SILENT_MODE_ERROR_CODE);
end;

{-------------------------------------------------------------------------------
  14/02/2003
  Retrieves error information from a verbose install log. The log must be in the
  temp folder (called inst.log). If the file is missing, or error cannot be
  found, then returns 'No further error information is available'.
  Some known errors are handled to make a tidy message
}
function TInstallMSDE.GetError: string;
var
  lFileStream: TFileStream;
  lStreamSize: Integer;
  WS   : Widestring;
  S    : String;
  i    : Integer;
  lLog : TStringList;
begin
  Result := ST_NO_MORE_INFO;
  if FileExists(GetWindowsTempDir + 'inst.log') then begin
    lFileStream := TFileStream.Create(GetWindowsTempDir + 'inst.log', fmOpenRead);
    lStreamSize := lFileStream.Size;
    SetLength(WS, lStreamSize div 2);
    lFileStream.Read(WS[1], lStreamSize);
    lFileStream.Free;
    S := WS;
    // search for known errors so we can have a decent error message

    if Pos(ST_INSTANCE_INVALID_LOG, S) > 0 then
      Result := ST_INSTANCE_INVALID_EXPLANATION
    else begin
      // other errors must extract text from the log
      lLog := TStringlist.Create;
      lLog.Text := S;
      for i := 1 to lLog.Count - 1 do begin
        if Pos('Return value 3', lLog[i]) > 0 then begin
          Result := GetMessageFromLog(lLog, i);
          Break;
        end;
      end;
      lLog.Free;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  14/02/2003
  Having located the error in the log (Return value 3 marks this) retrieve the
  preceding relevant lines
}
function TInstallMSDE.GetMessageFromLog(ALog: TStringList; ALineNumber: Integer): String;
var
  lCountBackToError : Integer;
  lError : String;
begin
  lCountBackToError := ALineNumber - 1;
  // read from start of previous action start to get error, or 10 lines max
  while (Copy(ALog[lCountBackToError], 1, 12) <> 'Action start') and
        (Copy(ALog[lCountBackToError], 1, 22) <> 'Starting custom action') and
      (ALineNumber - lCountBackToError < 10) do begin
    lError := ALog[lCountBackToError] + #13#10 + lError;
    Dec(lCountBackToError);
  end;
  Result := lError;
end;

{-------------------------------------------------------------------------------
  Returns true if process is no longer active, i.e. finished.
}
function TInstallMSDE.IsInstallFinished(var AExitCode: Cardinal): Boolean;
begin
  Result := IsProcessFinished(FProcessInfo, AExitCode);
end;

//------------------------------------------------------------------------------
end.
