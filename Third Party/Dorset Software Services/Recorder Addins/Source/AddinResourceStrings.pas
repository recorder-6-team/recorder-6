{===============================================================================
  Unit:        AddinResourceStrings

  Defines:     <nothing>

  Description: All resource strings used across the whole application.

  Created:     April 2004

  Last revision information:
    $Revision: 3 $
    $Date: 6/11/07 10:35 $
    $Author: Davidkelly $

===============================================================================}

unit AddinResourceStrings;

interface

resourcestring
  ResStr_Individuals = 'Individuals';
  ResStr_InstallationCancelled = 'You have chosen to abort the installation of the addin.'#13#13 +
      'You must re-install the addin before any of its facilities can be used.';
  ResStr_InvalidLogin = 'The login provided is not valid for the Recorder database.';
  ResStr_Locations = 'Locations';
  ResStr_LoginNotDBO = 'The login details supplied do not correspond to the database owner ' +
      'of the Recorder database and therefore cannot be used.  Please enter login details ' +
      'for the Recorder database owner.';
  ResStr_None = 'None';
  ResStr_Organisations = 'Organisations';
  ResStr_Species    = 'Species';
  ResStr_Surveys = 'Surveys';
  ResStr_Biotopes = 'Biotopes';
  ResStr_Names = 'Individuals & Organisations';
  ResStr_Terms = 'Terms';

  ResStr_InvalidMethodCall = 'Invalid call to method %s.';
  ResStr_Find = 'Find';

//==============================================================================
implementation

end.
