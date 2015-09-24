{===============================================================================
  ActiveX Component: DMAPServer

  Description:       COM object which provides the export functionality from
                     Recorder to DMAP.

  Model:             <none>

  Last revision information:
    $Revision: $
    $Date: $
    $Author: $

===============================================================================}

library DMAPServer;

uses
  ComServ,
  
  DMAPServer_TLB         in 'DMAPServer_TLB.pas',
  Recorder2000_TLB       in 'Recorder2000_TLB.pas',

  Exporter               in 'Source\Exporter.pas',

  DataClasses            in '..\..\Components\DataClasses.pas',
  GenFuncs               in '..\..\Components\GenFuncs.pas',
  JNCCDatasets           in '..\..\Components\JNCCDatasets.pas',
  SpatialRefFuncs        in '..\..\Components\SpatialRefFuncs.pas',
  SQLConverter           in '..\..\Components\SQLConverter.pas',
  VagueDate              in '..\..\Components\VagueDate.pas',

  ApiUtils               in '..\..\Third Party\Dorset Software Services\DssVcl32\ApiUtils.pas',
  DisplayFuncs           in '..\..\Third Party\Dorset Software Services\DssVcl32\DisplayFuncs.pas',
  ExceptionForm          in '..\..\Third Party\Dorset Software Services\DssVcl32\ExceptionForm.pas' {frmException: TfrmException},
  GeneralFunctions       in '..\..\Third Party\Dorset Software Services\DssVcl32\GeneralFunctions.pas',
  ListDlls               in '..\..\Third Party\Dorset Software Services\DssVcl32\ListDlls.pas',
  VersionInfo            in '..\..\Third Party\Dorset Software Services\DssVcl32\VersionInfo.pas';  

{$E ocx}

exports
  DllGetClassObject,
  DllCanUnloadNow,
  DllRegisterServer,
  DllUnregisterServer;

{$R DMAPServer.TLB}

{$R *.RES}

begin
end.
