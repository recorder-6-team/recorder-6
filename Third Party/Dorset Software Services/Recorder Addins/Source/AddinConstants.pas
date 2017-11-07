{===============================================================================
  Unit:        AddinConstants

  Defines:     <nothing>

  Description: Constants used in Addinxxx units.

  Model:       <none>

  Last revision information:
    $Revision: 4 $
    $Date: 21/07/09 10:01 $
    $Author: Ericsalmon $

===============================================================================}

unit AddinConstants;

interface

uses
  Messages;

const
  // Registry constants
  REG_KEY_RECORDER             = '\Software\Dorset Software\Recorder 6';
  REG_KEY_ADDIN                = REG_KEY_RECORDER + '\Installed Addins';
  REG_KEY_SETTINGS             = REG_KEY_RECORDER + '\Settings';
  REG_KEY_LOCATION_SEARCH_COLS = REG_KEY_SETTINGS + '\Location Search Columns';
  
  REG_VALUE_ADDIN_PATH           = 'Addin Path';
  REG_TOOL_TIPS                  = 'Tool Tips';
  REG_VALUE_LOCATION_TYPE        = 'Location Type';
  REG_VALUE_LOCATION_SPATIAL_REF = 'Spatial Reference';
  REG_VALUE_LOCATION_FILE_CODE   = 'File Code';
  REG_VALUE_SPATIAL_REF_SYSTEM   = 'Spatial Ref System';

  // Table names
  TN_SURVEY_EVENT = 'Survey_Event';
  TN_SAMPLE       = 'Sample';

  WM_REFRESHSEARCHLIST     = WM_APP + 1000;
  WM_DISPLAYPENDINGDATASET = WM_APP + 1001;

type
  // additional columns that may be displayed when searching for locations
  TLocationSearchColumn  = (lscLocationType, lscSpatialReference, lscFileCode);
  TLocationSearchColumns = set of TLocationSearchColumn;

implementation

end.
