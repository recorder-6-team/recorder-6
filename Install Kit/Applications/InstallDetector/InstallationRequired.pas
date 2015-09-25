//==============================================================================
//  Unit: InstallationRequired
//
//  Implements:   GetInstallationRequired
//
//  Description: Allows detection of whether a full installation is required, or
//               just an upgrade
//
//  Author:      John van Breda
//  Created:     11/06/2002
//
//  Changes:
//
//  Last Revision Details:
//    $Revision: 2 $
//    $Date: 24/04/03 12:23 $
//    $Author: Ericsalmon $
//
//  $History: InstallationRequired.pas $
//  
//  *****************  Version 2  *****************
//  User: Ericsalmon   Date: 24/04/03   Time: 12:23
//  Updated in $/JNCC/Development/Install Kit/Applications/InstallDetector
//
//  *****************  Version 1  *****************
//  User: Johnvanbreda Date: 12/06/02   Time: 2:07p
//  Created in $/JNCC/Installation Kit/InstallDetector
//  First version of InstallDetector tool
//
//==============================================================================
unit InstallationRequired;

interface

uses Sysutils, Classes, Registry, Windows;

type
  TInstallRequired= (irFullInstall, irUpgrade);

function GetInstallationRequired : TInstallRequired;
//  function InstallationFilesPresent : boolean;

//==============================================================================
implementation

{==============================================================================
  Return true if the database file is found as expected. }
function InstallationFilesPresent : boolean;
var
  lRegistry : TRegistry;
  lDBPath : string;
begin
  Result := False; // default not found
  lRegistry := TRegistry.Create;
  try
    if lRegistry.OpenKey('Software\JNCC\Recorder\Settings', false) then begin
      lDBPath := lRegistry.ReadString('Database Path');
      if (lDBPath <> '') and (FileExists(lDBPath)) then
        Result := True;
    end;
  finally
    lRegistry.Free;
  end;
end;


{==============================================================================
  Return true if the Current User settings are present for Recorder 2002. }
function RegistrySettingsPresent : boolean;
var
  lRegistry : TRegistry;
begin
  lRegistry := TRegistry.Create;
  try
    Result := lRegistry.OpenKey('Software\JNCC\Recorder\Settings', false);
  finally
    lRegistry.Free;
  end;
end;


{==============================================================================
  Clear out the current user and local machine registry settings. }
procedure CleanRegistrySettings;
var
  lRegistry : TRegistry;

    // delete a key plus all contained folders (though only 1 deep)
    procedure DeleteKeyAndContents(const iKey : string);
    var
      lKeys : TStringList;
      i : integer;
    begin
      lKeys := TStringList.Create;
      try
        if lRegistry.OpenKey(iKey, False) then begin
          lRegistry.GetKeyNames(lKeys);
          lRegistry.CloseKey; // otherwise it is locked
          for i := 0 to lKeys.Count-1 do
            lRegistry.DeleteKey(iKey + '\' + lKeys[i]);
          lRegistry.DeleteKey(iKey);
        end;
      finally
        lKeys.Free;
      end;
    end;

begin
  lRegistry := TRegistry.Create;
  try
    // Current User settings
    // ignore the return result of deletekey - there is not much we can do if it fails
    DeleteKeyAndContents('Software\JNCC\Recorder\Forms');
    DeleteKeyAndContents('Software\JNCC\Recorder\Settings');
    lRegistry.DeleteKey('Software\JNCC\Recorder');
    // Local Machine settings (if we have the access rights)
    lRegistry.RootKey := HKEY_LOCAL_MACHINE;
    DeleteKeyAndContents('Software\JNCC\Recorder\Installed Addins');
    lRegistry.DeleteKey('Software\JNCC\Recorder');    
  finally
    lRegistry.Free;
  end;
end;


{==============================================================================
  Public function - Returns whether a full install or an upgrade is required. }
function GetInstallationRequired : TInstallRequired;
begin
  if RegistrySettingsPresent then begin
    if InstallationFilesPresent then
      Result := irUpgrade
    else begin
      CleanRegistrySettings;
      Result := irFullInstall;
    end;
  end else
    Result := irFullInstall;
end;

end.
