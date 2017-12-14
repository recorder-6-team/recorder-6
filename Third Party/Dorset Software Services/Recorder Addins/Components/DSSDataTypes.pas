{===============================================================================
  Unit:        DSSDataTypes

  Defines:     Enumerations
               ============
               TEditMode
               TUserAccessLevel

  Description: Various types, enumerations and interfaces shared between
               Recorder and addins

  Model:       <none>

  Created:     December 2005

  Last revision information:
    $Revision: 2 $
    $Date: 8/12/05 13:19 $
    $Author: Johnvanbreda $

===============================================================================}

unit DSSDataTypes;

interface

uses
  SysUtils;

type

  TUserAccessLevel = (ualReadOnly, ualRecordOnly, ualAddOnly, ualFullUser, ualAdmin);

  TEditMode = (emBrowse, emEdit);

//==============================================================================

implementation

end.
