{------------------------------------------------------------------------------}
{------------------------------------------------------------------------------}
{ VersionInfo unit
    Contains methods for extracting the version information string from an
    application.  Version information must be included in the project options
  Date:
    3/3/99
  Compatability:
    Version 3 + later of Delphi
  Notes:
    Further ideas for extracting version related information are in issue 27 of
    the Delphi magazine - feel free to extend this unit!

  3/12/99
    Overloaded function to allow for retrieval of any file's version info.
}
{------------------------------------------------------------------------------}
{------------------------------------------------------------------------------}

unit VersionInfo;

interface

uses
  Forms, Windows, SysUtils;

function GetFileVersion: string; overload;
function GetFileVersion(const AFileName:string): string; overload;

//==============================================================================
implementation

const
  DEFAULT_VERSION = 'x.x.x.x';

//==============================================================================
function FileVersion(const AFileName:string): string;
var
  lSize:          DWORD;
  lVersionData:   string;
  lFixedInfoPtr:  PVSFixedFileInfo;
begin
  // Set default return value
  Result := DEFAULT_VERSION;

  lSize := GetFileVersionInfoSize(PChar(AFileName), lSize);
  if lSize <= 0 then Exit;

  // Reserve space to read version resource
  SetLength(lVersionData, lSize);

  // Read version resource
  if not GetFileVersionInfo(PChar(AFileName), 0, lSize, PChar(lVersionData)) then
    Exit;

  // Get fixed file info from version resource
  if not VerQueryValue(PChar(lVersionData), '\', Pointer(lFixedInfoPtr), lSize) then
    Exit;
  if lSize < SizeOf(TVSFixedFileInfo) then
    Exit;

  // Build version string
  with lFixedInfoPtr^ do begin
    Result := Format('%d.%d.%d.%d',
                     [HIWORD(dwFileVersionMS), LOWORD(dwFileVersionMS),
                      HIWORD(dwFileVersionLS), LOWORD(dwFileVersionLS)]);
  end;  // with pFixedInfoPtr
end;  // FileVersion

//==============================================================================
function GetFileVersion: string;
begin
  Result:=FileVersion(Application.ExeName);
end;  // GetFileVersion

//==============================================================================
function GetFileVersion(const AFileName:string): string;
begin
  if AFileName<>'' then
    Result:=FileVersion(AFileName)
  else
    Result:=DEFAULT_VERSION;
end;  //GetFileVersion

//==============================================================================
end.
