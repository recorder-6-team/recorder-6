{===============================================================================
  Unit:        InstallMSDE.pas

  Defines:     TInstallMSDE

  Description: Fucntionality to install MSDE on target machine.

  Model:

  Created:     February 2003

  Last revision information:
    $Revision: 3 $
    $Date: 11/02/09 15:31 $
    $Author: Ericsalmon $

===============================================================================}

unit InstallMSDE;

interface

uses
  Sysutils, Classes, GeneralFunctions, Windows, TextMessages, APIUtils, Forms,
  Settings;

const
  // Windows Installer, silent mode return code 1603 indicates an error occurred.
  I_SILENT_MODE_ERROR_CODE = 1603;

type
  TInstallMSDE = class(TObject)
  private
    FProcessInfo: TProcessInformation;
    FSettings: TSettings;
    function GetMessageFromLog(ALog: TStringList; ALineNumber: Integer): String;
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
  SetupConstants;

const
  DEFAULT_PARAMS = 'blanksapwd=1 /q%s /l* %sinst.log COLLATION="%s" ' +
                   'SECURITYMODE=SQL REBOOTPROMPT=Supress REBOOT=ReallySuppress';

{-==============================================================================
    TInstallMSDE
===============================================================================}
{-------------------------------------------------------------------------------
}
constructor TInstallMSDE.Create(ASettings: TSettings);
begin
  FSettings := ASettings;
end;  // TInstallMSDE.Create 

{-------------------------------------------------------------------------------
}
function TInstallMSDE.DoInstall(AShowInfo: Boolean; const ACollation, ADataDir, AInstanceName:
    String): Boolean;
var
  lParams: String;
  lReturnCode: Cardinal;
  lShowMode: String;
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
  
  lReturnCode := WinExec32('"' + FSettings.RootFolder + STR_MSDE_SETUP + '" ' + lParams,
                           FSettings.RootFolder + STR_MSDE_PATH, SW_SHOWNORMAL,
                           FProcessInfo);
  
  Result := (lReturnCode <> I_SILENT_MODE_ERROR_CODE);
end;  // TInstallMSDE.DoInstall 

{-------------------------------------------------------------------------------
}
function TInstallMSDE.GetError: String;
var
  lFileStream: TFileStream;
  lStreamSize: Integer;
  WS: Widestring;
  S: String;
  i: Integer;
  lLog: TStringList;
begin
  Result := ResStr_NoMoreInfo;
  if FileExists(GetWindowsTempDir + 'inst.log') then begin
    lFileStream := TFileStream.Create(GetWindowsTempDir + 'inst.log', fmOpenRead);
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
        if Pos('Return value 3', lLog[i]) > 0 then begin
          Result := GetMessageFromLog(lLog, i);
          Break;
        end;
      end;
      lLog.Free;
    end;
  end;
end;  // TInstallMSDE.GetError 

{-------------------------------------------------------------------------------
}
function TInstallMSDE.GetMessageFromLog(ALog: TStringList; ALineNumber: Integer): String;
var
  lCountBackToError: Integer;
  lError: String;
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
end;  // TInstallMSDE.GetMessageFromLog 

{-------------------------------------------------------------------------------
}
function TInstallMSDE.IsInstallFinished(var AExitCode: Cardinal): Boolean;
begin
  Result := IsProcessFinished(FProcessInfo, AExitCode);
end;  // TInstallMSDE.IsInstallFinished 

end.
