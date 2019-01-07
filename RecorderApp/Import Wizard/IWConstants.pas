{===============================================================================
  Unit:        IWConstants

  Defines:     <nothing>

  Description: Constants used in Import Wizard addin.

  Model:

  Last revision information:
    $Revision: 22 $
    $Date: 21/03/13 15:17 $
    $Author: Michaelcaptain $

===============================================================================}

unit IWConstants;

interface

uses
  Messages;

const
  STR_PROTOTYPE_HTML =
      '<HTML><BODY Background="Images\%s" style="background-repeat:no-repeat"><BR><BR><BR><BR>' +
      '<H3>%s</H3><HR><font size=2><P>Instructions</P>';  //</font></BODY></HTML>';

  STR_NOT_AVAILABLE = 'Not available';

  { User Messages. Start at WM_APP + 200 to avoid conflict with Recorder set of messages
    starting at WM_APP + 100. }
  UM_CHANGEPAGE    = WM_APP + 200;
  UM_RETURN        = WM_APP + 201;
  UM_CANCEL_IMPORT = WM_APP + 202;

  // Keys of column types for which special behaviour is defined.
  CT_KEY_LOCATION             = 'SYSTEM0100000000';
  CT_KEY_GRID_REFERENCE       = 'SYSTEM0100000001';
  CT_KEY_DATE                 = 'SYSTEM0100000002';
  CT_KEY_SPECIES              = 'SYSTEM0100000003';
  CT_KEY_OBSERVER             = 'SYSTEM0100000004';
  CT_KEY_DETERMINER           = 'SYSTEM0100000005';
  CT_KEY_REVIEWER             = 'SYSTEM01000000A0';
  CT_KEY_ASSOCIATED_SPECIES   = 'SYSTEM0100000008';
  CT_KEY_ASSOCIATION_TYPE     = 'SYSTEM0100000009';
  CT_KEY_BRC_SOURCE           = 'SYSTEM010000000B';
  CT_KEY_MAPMATE_KEY          = 'SYSTEM010000000E';
  CT_KEY_RECORD_ID            = 'SYSTEM010000000H';
  CT_KEY_RECORD_TYPE          = 'SYSTEM010000000I';
  CT_KEY_SAMPLING_METHOD      = 'SYSTEM010000000J';
  CT_KEY_SITE_ID              = 'SYSTEM010000000K';
  CT_KEY_SPECIMEN_TYPE        = 'SYSTEM010000000N';
  CT_KEY_VICE_COUNTY_NUMBER   = 'SYSTEM010000000R';
  CT_KEY_ABUNDANCE_DATA       = 'SYSTEM010000000S';
  CT_KEY_LOCATION_NAME        = 'SYSTEM010000000T';
  CT_KEY_SPATIAL_SYSTEM       = 'SYSTEM010000000U';
  CT_KEY_DETERMINER_ROLE      = 'SYSTEM010000000V';
  CT_KEY_DETERMINATION_TYPE   = 'SYSTEM010000000W';
  CT_KEY_ABUNDANCE_ACCURACY   = 'SYSTEM010000000X';
  CT_KEY_TEMPOBSERVER         = 'SYSTEM010000000Y';
  CT_KEY_DETERMINATION_DATE   = 'SYSTEM010000000Z';
  CT_KEY_REVIEW_DATE          = 'SYSTEM01000000B0';
  CT_KEY_REVIEWER_ROLE        = 'SYSTEM01000000D0';
  CT_KEY_REVIEW_TYPE          = 'SYSTEM01000000C0';
  CT_KEY_REVIEW_SPECIES       = 'SYSTEM01000000F0';
  CT_KEY_DETERMINER_PREFERRED = 'SYSTEM01000000I0';
  CT_KEY_REVIEWER_PREFERRED   = 'SYSTEM01000000G0';

  // Keys of Match Rules for which special behaviour is defined.
  MR_KEY_NAMES    = 'SYSTEM0100000000';
  MR_KEY_LOCATION = 'SYSTEM0100000003';

  // Default for Sample Type on Missing Data page.
  ST_KEY_FIELD_OBSERVATION = 'NBNSYS0000000001';

  // Keys for Output Fields
  OF_KEY_RECORDERS = 'SYSTEM0100000025';

  // Known and expected field names for matching pages
  FN_IMPORT_VALUE   = 'Import_Value';
  FN_MATCH_COUNT    = 'Match_Count';
  FN_MATCH_VALUE    = 'Match_Value';
  FN_MATCH_KEY      = 'Match_Key';
  FN_REMEMBERED     = 'Remembered';
  FN_TAXON_ORDER    = 'Order';
  FN_MATCH_NOTES    = 'Notes';
  FN_CHECKLIST      = 'Checklist';
  FN_CLASSIFICATION = 'Classification';
  FN_IMPORT_GRIDREF = 'Import_Grid_Reference';
  FN_SPATIAL_REF    = 'Spatial_Ref';

implementation

end.
