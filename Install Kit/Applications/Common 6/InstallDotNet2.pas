{===============================================================================
  Unit:        InstallDotNet2.pas

  Defines:     TInstallDotNet2

  Description: Fucntionality to install Dot Net Framework 2.0 on target machine.

  Model:

  Created:     April 2009

  Last revision information:
    $Revision: 1 $
    $Date: 24/04/09 15:10 $
    $Author: Pauldavies $

===============================================================================}

unit InstallDotNet2;

interface

uses
  Windows, Settings, ExceptionForm;

type
  EDotNet2InstallError = class (TExceptionPath)
  end;

  TInstallDotNet2 = class(TObject)
  private
    FProcessInfo: TProcessInformation;
    FSettings: TSettings;
  public
    constructor Create(settings: TSettings);
    function DoInstall: Boolean;
    function IsInstallFinished(var AExitCode: Cardinal): Boolean;
    function PatchesToInstall: Integer;
    function InstallPatch(index: integer): Boolean;
  end;

implementation

uses
  SetupConstants, ApiUtils, TextMessages, SysUtils {delete},Dialogs {/delete};

const
  INSTALL_PARAMS =
      '%s /q';

  I_IN_PROGRESS = 259; //Indicates that the installation is in progress
  I_SUCCESS = 0;       //Indicates that the installation was successful

resourcestring
  ResStr_PatchIndexOutOfRange = 'Patch Index out of range.';

{===============================================================================

  TInstallDotNet2

===============================================================================}

{-------------------------------------------------------------------------------
  Creates the TInstallDotNet2 object.
}
constructor TInstallDotNet2.Create(settings: TSettings);
begin
  FSettings := settings;
end;

{-------------------------------------------------------------------------------
  Returns the number of patches to install.
}
function TInstallDotNet2.PatchesToInstall;
begin
  Result := I_DOTNET_PATCH_COUNT;
end;

{-------------------------------------------------------------------------------
  Installs the Dot Net 2.0 framework
}
function TInstallDotNet2.DoInstall: Boolean;
var
  returnCode: Cardinal;
begin
  FSettings.SQLExpressInstallState := isInstalling;

  returnCode := WinExec32(
      Format(INSTALL_PARAMS, [FSettings.RootFolder + STR_DOTNET2_SETUP]),
      FSettings.RootFolder + STR_DOTNET2_PATH,
      SW_SHOWNORMAL,
      FProcessInfo);

  Result := ((returnCode = I_IN_PROGRESS) or (returnCode = I_SUCCESS));
end;

{-------------------------------------------------------------------------------
  Returns whether or not the install has completed.
}
function TInstallDotNet2.IsInstallFinished(var AExitCode: Cardinal): Boolean;
begin
  Result := IsProcessFinished(FProcessInfo, AExitCode);
end;  // TInstallDotNet2.IsInstallFinished

{-------------------------------------------------------------------------------
  Installs the patch with the specified index.
}
function TInstallDotNet2.InstallPatch(index: integer): Boolean; 
var
  returnCode: Cardinal;
begin
  if (index < 1) or (index > I_DOTNET_PATCH_COUNT) then
    raise EDotNet2InstallError.Create(ResStr_PatchIndexOutOfRange)
  else
    returnCode := WinExec32(
            Format(INSTALL_PARAMS,
                   [FSettings.RootFolder + STR_DOTNET2_PATH + DOTNET2_PATCHES[index]]),
            FSettings.RootFolder + STR_DOTNET2_PATH,
            SW_SHOWNORMAL,
            FProcessInfo);

  Result := ((returnCode = I_IN_PROGRESS) or (returnCode = I_SUCCESS));
end;

end.