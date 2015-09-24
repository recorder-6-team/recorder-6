{===============================================================================
  Library:     StdValLib

  Description: Standard validation library for Recorder.

  Model:       <none>

  Last revision information:
    $Revision: $
    $Date: $
    $Author: $

===============================================================================}

library StdValLib;

uses
  ComServ,

  StdValLib_TLB          in 'StdValLib_TLB.pas',
  Recorder2000_TLB       in 'Recorder2000_TLB.pas',

  Recorder2000Validation in 'Source\Recorder2000Validation.pas',
  SpatialComAddins       in 'Source\SpatialComAddins.pas',

  COMClasses             in '..\..\Components\COMClasses.pas',
  DataClasses            in '..\..\Components\DataClasses.pas',
  DatabaseUtilities      in '..\..\Components\DatabaseUtilities.pas',
  GenFuncs               in '..\..\Components\GenFuncs.pas',
  GridSquareItem         in '..\..\Components\GridSquareItem.pas',
  JNCCDatasets           in '..\..\Components\JNCCDatasets.pas',
  JNCCRelationships      in '..\..\Components\JNCCRelationships.pas',
  JRO_TLB                in '..\..\Components\JRO_TLB.pas',
  SpatialRefFuncs        in '..\..\Components\SpatialRefFuncs.pas',
  SQLConverter           in '..\..\Components\SQLConverter.pas',
  VagueDate              in '..\..\Components\VagueDate.pas',

  Constants              in '..\..\Source\Constants.pas',
  DatabaseAccessADO      in '..\..\Source\DatabaseAccessADO.pas' {dmDatabase: TdmDatabase},
  SQLConstants           in '..\..\Source\SQLConstants.pas',
  ValidationData         in '..\..\Source\ValidationData.pas' {dmValidation: TDataModule},

  ADODB_TLB              in '..\..\..\Third Party\Dorset Software Services\DssVcl32\ADODB_TLB.pas',
  ADOX_TLB               in '..\..\..\Third Party\Dorset Software Services\DssVcl32\ADOX_TLB.pas',
  ApiUtils               in '..\..\..\Third Party\Dorset Software Services\DssVcl32\ApiUtils.pas',
  BaseADODataModule      in '..\..\..\Third Party\Dorset Software Services\DssVcl32\BaseADODataModule.pas',
  DisplayFuncs           in '..\..\..\Third Party\Dorset Software Services\DssVcl32\DisplayFuncs.pas',
  ExceptionForm          in '..\..\..\Third Party\Dorset Software Services\DssVcl32\ExceptionForm.pas' {frmException: TfrmException},
  GeneralFunctions       in '..\..\..\Third Party\Dorset Software Services\DssVcl32\GeneralFunctions.pas',
  ListDlls               in '..\..\..\Third Party\Dorset Software Services\DssVcl32\ListDlls.pas',
  Relationships_ADO      in '..\..\..\Third Party\Dorset Software Services\DssVcl32\Relationships_ADO.pas',
  VersionInfo            in '..\..\..\Third Party\Dorset Software Services\DssVcl32\VersionInfo.pas';

exports
  DllGetClassObject,
  DllCanUnloadNow,
  DllRegisterServer,
  DllUnregisterServer;
                        
{$R *.TLB}

{$R *.RES}

begin
end.
