{ ------------------------------------------------------------------------------
//  Unit:         VersionInfo2.pas
//
//  Implements:   TVersionInfo
//
//  Description:  Object representation of a file's version information
//                resource
//
//  Author:       AJWK
//  Created:      2003-02-21
//
//  Last Revision Details:
//    $Revision: 3 $
//    $Date: 24/02/03 14:23 $
//    $Author: Andrewkemp $
//
//  Copyright © Dorset Software Services Ltd, 2003
//
// --------------------------------------------------------------------------- }
unit VersionInfo2;

interface

uses
  Windows;

type
  TVersionInfo = class(TObject)
  protected { implementation }
    FFileName: string;
    FImplementationSize: Integer;
    FImplementationPtr: Pointer;

    function ImplementationSize: Integer; virtual;
    function ValueDefined(const SubBlockPath: string): Boolean; virtual;
    function FixedFileInfo: PVSFixedFileInfo; virtual;
    function FlagSet(FileFlag: Cardinal): Boolean; virtual;
    function LangCodePageName: string; virtual;
    function LocalisedString(const ValueName: string): string; virtual;

    procedure Load; virtual;
    procedure GetValueLocation(const SubBlockPath: string;
                               out ValuePtr: Pointer;
                               out ValueLength: Cardinal); virtual;

  public { lifetime }
    constructor Create;
    constructor CreateFromFile(const FileName: string);
    destructor Destroy; override;

  public { data access }
    property FileName: string read FFileName;

    function MajorVersion: Integer; virtual;
    function MinorVersion: Integer; virtual;
    function Release: Integer; virtual;
    function Build: Integer; virtual;

    function DebugBuild: Boolean; virtual;
    function Patched: Boolean; virtual;
    function PreRelease: Boolean; virtual;
    function PrivateBuild: Boolean; virtual;
    function SpecialBuild: Boolean; virtual;

    function Comments: string; virtual;
    function CompanyName: string; virtual;
    function FileDescription: string; virtual;
    function FileVersion: string; virtual;
    function InternalName: string; virtual;
    function LegalCopyright: string; virtual;
    function LegalTrademarks: string; virtual;
    function OriginalFilename: string; virtual;
    function ProductName: string; virtual;
    function ProductVersion: string; virtual;
    function PrivateBuildDetails: string; virtual;
    function SpecialBuildDetails: string; virtual;

  end;  { class TVersionInfo }




implementation

uses
  Forms, SysUtils;


{ ------------------------------------------------------------------------------
// Version information for application's executable file
// --------------------------------------------------------------------------- }
constructor TVersionInfo.Create;
begin
  CreateFromFile(Application.ExeName);
end;

{ ------------------------------------------------------------------------------
// Version information for named file
// --------------------------------------------------------------------------- }
constructor TVersionInfo.CreateFromFile(const FileName: string);
begin
  inherited Create;
  FFileName := FileName;
  Load;
end;

{ ------------------------------------------------------------------------------
// --------------------------------------------------------------------------- }
destructor TVersionInfo.Destroy;
begin
  if Assigned(FImplementationPtr) then FreeMem(FImplementationPtr);
  inherited;
end;

{ ------------------------------------------------------------------------------
// Size of Windows version information structure
// --------------------------------------------------------------------------- }
function TVersionInfo.ImplementationSize: Integer;
var
  Handle: DWORD;
begin
  if FImplementationSize = 0 then begin
    FImplementationSize := GetFileVersionInfoSize(PChar(FileName), Handle);
    if FImplementationSize = 0 then RaiseLastOSError;
  end;
  Result := FImplementationSize;
end;

{ ------------------------------------------------------------------------------
// Load version information into new buffer pointed to by `FImplementationPtr'
// --------------------------------------------------------------------------- }
procedure TVersionInfo.Load;
begin
  GetMem(FImplementationPtr, ImplementationSize);
  Win32Check( GetFileVersionInfo(PChar(FileName),
                                 0,
                                 ImplementationSize,
                                 FImplementationPtr) );
end;

{ ------------------------------------------------------------------------------
// Is specified value defined by the version resource?
// --------------------------------------------------------------------------- }
function TVersionInfo.ValueDefined(const SubBlockPath: string): Boolean;
var
  ValuePtr: Pointer;
  ValueLength: UINT;
begin
  Assert( Assigned(FImplementationPtr), 'Version info not loaded' );
  Result := VerQueryValue(FImplementationPtr,
                          PChar(SubBlockPath),
                          ValuePtr,
                          ValueLength);
end;

{ ------------------------------------------------------------------------------
// Obtain position and length of a value, selected using a sub-block path
// --------------------------------------------------------------------------- }
procedure TVersionInfo.GetValueLocation(const SubBlockPath: string;
                                        out ValuePtr: Pointer;
                                        out ValueLength: Cardinal);
begin
  Assert( ValueDefined(SubBlockPath), 'Value not defined' );
  Win32Check( VerQueryValue(FImplementationPtr,
                            PChar(SubBlockPath),
                            ValuePtr,
                            ValueLength) );
end;

{ ------------------------------------------------------------------------------
// Pointer to locale independent version information record
// --------------------------------------------------------------------------- }
function TVersionInfo.FixedFileInfo: PVSFixedFileInfo;
var
  ValuePtr: Pointer;
  ValueLength: Cardinal;
begin
  if not ValueDefined('\') then
    Result := nil
  else begin
    GetValueLocation('\', ValuePtr, ValueLength);
    Result := PVSFixedFileInfo(ValuePtr);
  end;
end;

{ ------------------------------------------------------------------------------
// Is specified flag set?
// --------------------------------------------------------------------------- }
function TVersionInfo.FlagSet(FileFlag: Cardinal): Boolean;
var
  FixedPtr: PVSFixedFileInfo;
begin
  FixedPtr := FixedFileInfo;
  if not Assigned(FixedPtr) then
    Result := False
  else begin
    Result := (FixedPtr^.dwFileFlags
                and FixedPtr^.dwFileFlagsMask
                and FileFlag) > 0;
  end;
end;

{ ------------------------------------------------------------------------------
// Lang-codepage name used to look up localised values
//
// Ideally this should be selected from those available based on the user's
// locale, but we just pick the first one -- this is what Windows Explorer
// does!  The Delphi IDE only lets you define version information in one
// locale anyway, so this probably isn't a problem in practice.
// --------------------------------------------------------------------------- }
function TVersionInfo.LangCodePageName: string;
var
  ValuePtr: Pointer;
  ValueLength: Cardinal;
  LocalePairPtr: PCardinal;
begin
  GetValueLocation('\VarFileInfo\Translation', ValuePtr, ValueLength);
  LocalePairPtr := PCardinal( ValuePtr );
  Result := Format('%.4x%.4x', [(LocalePairPtr^ and $0000ffff),
                                (LocalePairPtr^ and $ffff0000) shr 16]);
end;

{ ------------------------------------------------------------------------------
// Named string value from locale identified by `LangCodePageName'
// --------------------------------------------------------------------------- }
function TVersionInfo.LocalisedString(const ValueName: string): string;
var
  ValueSubBlock: string;
  ValuePtr: Pointer;
  ValueLength: Cardinal;
begin
  ValueSubBlock := '\StringFileInfo\' + LangCodePageName + '\' + ValueName;
  if not ValueDefined(ValueSubBlock) then
    Result := ''
  else begin
    GetValueLocation(ValueSubBlock, ValuePtr, ValueLength);
    SetString(Result, PChar(ValuePtr), ValueLength - 1);
  end;
end;

{ ------------------------------------------------------------------------------
// Major version number
// --------------------------------------------------------------------------- }
function TVersionInfo.MajorVersion: Integer;
var
  FixedPtr: PVSFixedFileInfo;
begin
  FixedPtr := FixedFileInfo;
  if not Assigned(FixedPtr) then
    Result := 0
  else
    Result := (FixedPtr^.dwFileVersionMS and $ffff0000) shr 16;
end;

{ ------------------------------------------------------------------------------
// Minor version number
// --------------------------------------------------------------------------- }
function TVersionInfo.MinorVersion: Integer;
var
  FixedPtr: PVSFixedFileInfo;
begin
  FixedPtr := FixedFileInfo;
  if not Assigned(FixedPtr) then
    Result := 0
  else
    Result := FixedPtr^.dwFileVersionMS and $0000ffff;
end;

{ ------------------------------------------------------------------------------
// Release number
// --------------------------------------------------------------------------- }
function TVersionInfo.Release: Integer;
var
  FixedPtr: PVSFixedFileInfo;
begin
  FixedPtr := FixedFileInfo;
  if not Assigned(FixedPtr) then
    Result := 0
  else
    Result := (FixedPtr^.dwFileVersionLS and $ffff0000) shr 16;
end;

{ ------------------------------------------------------------------------------
// Build number
// --------------------------------------------------------------------------- }
function TVersionInfo.Build: Integer;
var
  FixedPtr: PVSFixedFileInfo;
begin
  FixedPtr := FixedFileInfo;
  if not Assigned(FixedPtr) then
    Result := 0
  else
    Result := FixedPtr^.dwFileVersionLS and $0000ffff;
end;

{ ------------------------------------------------------------------------------
// Does file contain debugging information or have debugging features enabled?
// --------------------------------------------------------------------------- }
function TVersionInfo.DebugBuild: Boolean;
begin
  Result := FlagSet(VS_FF_DEBUG);
end;

{ ------------------------------------------------------------------------------
// Has file been modified from shipping version with same version number?
// --------------------------------------------------------------------------- }
function TVersionInfo.Patched: Boolean;
begin
  Result := FlagSet(VS_FF_PATCHED);
end;

{ ------------------------------------------------------------------------------
// Is this a development version?
// --------------------------------------------------------------------------- }
function TVersionInfo.PreRelease: Boolean;
begin
  Result := FlagSet(VS_FF_PRERELEASE);
end;

{ ------------------------------------------------------------------------------
// Was file not built using standard release procedures?
// --------------------------------------------------------------------------- }
function TVersionInfo.PrivateBuild: Boolean;
begin
  Result := FlagSet(VS_FF_PRIVATEBUILD);
end;

{ ------------------------------------------------------------------------------
// Is file a variation on the normal file with same version number?
// --------------------------------------------------------------------------- }
function TVersionInfo.SpecialBuild: Boolean;
begin
  Result := FlagSet(VS_FF_SPECIALBUILD);
end;

{ ------------------------------------------------------------------------------
// Additional information displayed for diagnostic purposes
// --------------------------------------------------------------------------- }
function TVersionInfo.Comments: string;
begin
  Result := LocalisedString('Comments');
end;

{ ------------------------------------------------------------------------------
// Company that produced file
// --------------------------------------------------------------------------- }
function TVersionInfo.CompanyName: string;
begin
  Result := LocalisedString('CompanyName');
end;

{ ------------------------------------------------------------------------------
// Description suitable for presentation to users
// --------------------------------------------------------------------------- }
function TVersionInfo.FileDescription: string;
begin
  Result := LocalisedString('FileDescription');
end;

{ ------------------------------------------------------------------------------
// Version
// --------------------------------------------------------------------------- }
function TVersionInfo.FileVersion: string;
begin
  Result := LocalisedString('FileVersion');
end;

{ ------------------------------------------------------------------------------
// Internal name, if any (e.g. module name for a DLL)
// --------------------------------------------------------------------------- }
function TVersionInfo.InternalName: string;
begin
  Result := LocalisedString('InternalName');
end;

{ ------------------------------------------------------------------------------
// Copyright notices, trademarks, registered trademarks
// --------------------------------------------------------------------------- }
function TVersionInfo.LegalCopyright: string;
begin
  Result := LocalisedString('LegalCopyright');
end;

{ ------------------------------------------------------------------------------
// Trademarks, registered trademarks
// --------------------------------------------------------------------------- }
function TVersionInfo.LegalTrademarks: string;
begin
  Result := LocalisedString('LegalTrademarks');
end;

{ ------------------------------------------------------------------------------
// Original file name
// --------------------------------------------------------------------------- }
function TVersionInfo.OriginalFilename: string;
begin
  Result := LocalisedString('OriginalFilename');
end;

{ ------------------------------------------------------------------------------
// Name of product with which file is distributed
// --------------------------------------------------------------------------- }
function TVersionInfo.ProductName: string;
begin
  Result := LocalisedString('ProductName');
end;

{ ------------------------------------------------------------------------------
// Version of product with which file is distributed
// --------------------------------------------------------------------------- }
function TVersionInfo.ProductVersion: string;
begin
  Result := LocalisedString('ProductVersion');
end;

{ ------------------------------------------------------------------------------
// By whom, where and why this private version was built
// --------------------------------------------------------------------------- }
function TVersionInfo.PrivateBuildDetails: string;
begin
  Result := LocalisedString('PrivateBuild');
end;

{ ------------------------------------------------------------------------------
// How this version differs from the normal version
// --------------------------------------------------------------------------- }
function TVersionInfo.SpecialBuildDetails: string;
begin
  Result := LocalisedString('SpecialBuild');
end;

end.
