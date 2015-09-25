{===============================================================================
  Library:      RecorderAddInInstaller

  Description:  An ActiveX library containing a component that can be used
                to install or remove Recorder add-ins, with support for COM
                elevation on versions of Windows from Vista onwards.

  Created:      February 2009

  Last revision information:
    $Revision: 2 $
    $Date: 17/02/09 16:39 $
    $Author: Andrewkemp $

  Copyright © Dorset Software Services Ltd, 2009

===============================================================================}
library RecorderAddInInstaller;

uses
  ComServ,
  LocOnFly,
  RecorderAddInInstaller_TLB in 'RecorderAddInInstaller_TLB.pas',
  Recorder2000_TLB           in 'Recorder2000_TLB.pas',
  RecorderAddInRegistration  in 'Source\RecorderAddInRegistration.pas',

  ADODB_TLB                  in '..\..\Third Party\Dorset Software Services\DssVcl32\trunk\ADODB_TLB.pas',
  ApiUtils                   in '..\..\Third Party\Dorset Software Services\DssVcl32\trunk\ApiUtils.pas',
  CustomOleCtrls7            in '..\..\Third Party\Dorset Software Services\DssVcl32\trunk\CustomOleCtrls7.pas',
  PrivilegedComObj           in '..\..\Third Party\Dorset Software Services\DssVcl32\trunk\PrivilegedComObj.pas',
  PrivilegedObjectFactory    in '..\..\Third Party\Dorset Software Services\DssVcl32\trunk\PrivilegedObjectFactory.pas',
  StockIcons                 in '..\..\Third Party\Dorset Software Services\DssVcl32\trunk\StockIcons.pas',
  OleTools                   in '..\..\Third Party\Dorset Software Services\DssVcl32\trunk\OleTools.pas';

{$R RecorderAddInInstaller.KLR}

exports
  DllGetClassObject,
  DllCanUnloadNow,
  DllRegisterServer,
  DllUnregisterServer;

{$R *.TLB}

{$R *.RES}

// A resource file containing Recorder's application name, for display in
// UAC elevation prompts.
{$R ApplicationName.RES ApplicationName.RC}

begin
end.
