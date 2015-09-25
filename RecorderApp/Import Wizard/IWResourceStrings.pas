{===============================================================================
  Unit:        IWResourceStrings

  Defines:     <nothing>

  Description: Resource strings used in Import Wizard.

  Model:

  Created:     April 2004

  Last revision information:
    $Revision: 3 $
    $Date: 10/03/09 12:02 $
    $Author: Ericsalmon $

===============================================================================}

unit IWResourceStrings;

interface

resourcestring
  ResStr_Continue    = '&Continue';
  ResStr_DoImport    = '&Do Import';
  ResStr_Next        = '&Next';
  ResStr_Stop        = '&Stop';
  ResStr_MoreOptions = '<more options>';
  ResStr_Validate    = '&Validate';

  ResStr_ImportAborted = 'Data import stopped.';

  // Messages
  ResStr_SpatialRefQualifierMissing =
      'A Spatial Reference Qualifier must be entered to support the Spatial Reference.';
  ResStr_LocationNotFound =
      'The specified location was not found in the list of Locations. ' +
      'Please make sure that the chosen location is valid.';
  ResStr_LocationNameMissing =
      'The Location Name is either missing or invalid. ' +
      'Please enter a Location, a Spatial Reference or a Location Name.';
  ResStr_SpatialRefOutsideSurveyBoundingBox =
      'The chosen spatial reference is not in the bounding box of the selected survey.';

  ResStr_ObserverNameRequired =
      'The Observer Name is either missing or invalid. ' +
      'Please enter a valid name for the observer.';
  ResStr_DeterminerNameRequired =
      'The Determiner Name is either missing or invalid. ' +
      'Please enter a valid name for the Determiner';

implementation

end.
 