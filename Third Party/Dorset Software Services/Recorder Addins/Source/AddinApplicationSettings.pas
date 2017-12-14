{===============================================================================
  Unit:        AddinApplicationSettings

  Defines:     TAddinAppSettings

  Description: Defines a base class for addin settings and implements
               commonly used settings.

  Model:       Addins.mpb

  Created:     April 2004

  Last revision information:
    $Revision: 1 $
    $Date: 2/09/04 15:21 $
    $Author: Andrewkemp $

===============================================================================}

unit AddinApplicationSettings;

interface

uses
  SysUtils, Graphics, Windows, ExceptionForm;

type
  {-----------------------------------------------------------------------------
    Base class for application level settings held by an addin.  This class implements
    commonly used settings such as those held in Recorder or Recorder's registry.
  }
  TAddinAppSettings = class(TObject)
  private
    FAddinPath: String;
    function GetDisableDragDropFrames: Boolean;
    function GetDragDestinationColour: TColor;
    function GetDragSourceColour: TColor;
    function GetMandatoryColour: TColor;
    function GetUserAccessLevel: Integer;
    procedure ReadRegSettings;
    function GetShowToolTips: Boolean;
  public
    constructor Create;
    property AddinPath: String read FAddinPath;
    property DisableDragDropFrames: Boolean read GetDisableDragDropFrames;
    property DragDestinationColour: TColor read GetDragDestinationColour;
    property DragSourceColour: TColor read GetDragSourceColour;
    property MandatoryColour: TColor read GetMandatoryColour;
    property UserAccessLevel: Integer read GetUserAccessLevel;
    property ShowToolTips: Boolean read GetShowToolTips;
  end;
  
//==============================================================================
implementation

uses
  AddinConstants, AddinGeneralData, ProjectSpecificAccess, Recorder2000_TLB, Registry,
  BaseADODataModule;

{-==============================================================================
    TAddinAppSettings
===============================================================================}
{-------------------------------------------------------------------------------
  Object initialisation 
}
constructor TAddinAppSettings.Create;
begin
  inherited;
  ReadRegSettings;
end;  // TAddinAppSettings.Create 

{-------------------------------------------------------------------------------
  Retrieve the DisableDragDropFrames value from Recorder. 
}
function TAddinAppSettings.GetDisableDragDropFrames: Boolean;
begin
  Result := dmGeneral.Recorder.CurrentSettings.DisableDragDropFrames;
end;  // TAddinAppSettings.GetDisableDragDropFrames 

{-------------------------------------------------------------------------------
  Retrieve the DragDestinationColour value from Recorder. 
}
function TAddinAppSettings.GetDragDestinationColour: TColor;
begin
  Result := dmGeneral.Recorder.CurrentSettings.DragDestinationColour;
end;  // TAddinAppSettings.GetDragDestinationColour 

{-------------------------------------------------------------------------------
  Retrieve the DragSourceColour value from Recorder. 
}
function TAddinAppSettings.GetDragSourceColour: TColor;
begin
  Result := dmGeneral.Recorder.CurrentSettings.DragSourceColour;
end;  // TAddinAppSettings.GetDragSourceColour 

{-------------------------------------------------------------------------------
  Retrieve the MandatoryColour value from Recorder. 
}
function TAddinAppSettings.GetMandatoryColour: TColor;
begin
  Result := dmGeneral.Recorder.CurrentSettings.MandatoryColour;
end;  // TAddinAppSettings.GetMandatoryColour

{-------------------------------------------------------------------------------
}
function TAddinAppSettings.GetUserAccessLevel: Integer;
begin
  Result := dmGeneral.GetStoredProcOutputParam
                        ('usp_UserAccessLevel_Get',
                         ['@Key', dmGeneral.Recorder.CurrentSettings.UserIDKey],
                         '@AccessLevel');
end;  // TAddinAppSettings.GetUserAccessLevel 

{-------------------------------------------------------------------------------
  Read the value for settings that are stored in the registry. 
}
procedure TAddinAppSettings.ReadRegSettings;
var
  lReg: TRegistry;
begin
  lReg := TRegistry.Create;
  try
    lReg.RootKey := HKEY_LOCAL_MACHINE;
    lReg.OpenKeyReadOnly(REG_KEY_RECORDER);
    FAddinPath := lReg.ReadString(REG_VALUE_ADDIN_PATH);
  finally
    lReg.Free;
  end;
end;  // TAddinAppSettings.ReadRegSettings 

function TAddinAppSettings.GetShowToolTips: Boolean;
var
  lReg: TRegistry;
begin
  lReg := TRegistry.Create;
  try
    lReg.RootKey := HKEY_CURRENT_USER;
    lReg.OpenKeyReadOnly(REG_KEY_SETTINGS);
    Result := lReg.ReadBool(REG_TOOL_TIPS);
  finally
    lReg.Free;
  end;
end;

end.
