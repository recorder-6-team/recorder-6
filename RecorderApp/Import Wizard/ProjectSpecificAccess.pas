{===============================================================================
  Unit:        ProjectSpecificAccess

  Defines:     <nothing>

  Description: Bridge between application and Addinxxx units.

  Model:       <none>

  Last revision information:
    $Revision: 1 $
    $Date: 15/06/04 11:25 $
    $Author: Ericsalmon $

===============================================================================}

unit ProjectSpecificAccess;

interface

uses
  Sysutils, AddinSearchManager, AddinInterfaceDataModule, DatabaseAccessADO;

function dmGeneral: TdmDatabase;

var
  dmInterface: TdmAddinInterface;

//==============================================================================
implementation

{-------------------------------------------------------------------------------
}
function dmGeneral: TdmDatabase;
begin
  Result := dmDatabase;
end;

initialization
  dmInterface := TdmAddinInterface.Create(nil);

finalization
  FreeAndNil(dmInterface);

end.