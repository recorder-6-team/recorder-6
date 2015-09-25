unit Logger;

interface

procedure Log(const AText: string; AIndent : boolean);

implementation

uses GeneralFunctions, Sysutils;

var
  mFirstLogEntry : boolean;
  mOutputFile : TextFile;

{-------------------------------------------------------------------------------
  Description : Writes text out into the log.  Entry is indented if required
  Created : 07/03/2003 }
procedure Log(const AText : string; AIndent : boolean);
begin
  if mFirstLogEntry then begin
    // prepare a TextFile so we can append to a log
    AssignFile(mOutputFile, GetWindowsTempDir + 'UpgradeLog.txt');
    Rewrite(mOutputFile);
    mFirstLogEntry := False;
  end;
  if AIndent then
    WriteLn(mOutputFile, #9'[' + TimeToStr(now) + ']  ' + AText)
  else
    WriteLn(mOutputFile, '[' + TimeToStr(now) + ']  ' + AText);
  Flush(mOutputFile);
end;


initialization
  mFirstLogEntry := True;

finalization
  if not mFirstLogEntry then
    CloseFile(mOutputFile);

end.
