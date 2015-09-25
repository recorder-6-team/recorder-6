/*===========================================================================*\
  Description:	
	Adds an index to the Measurement_Qualifier_Key of Taxon_Occurrence_Data, so
	as to speed up certain queries.

  Created:	January 2009

  Last revision information:
    $Revision: 1 $
    $Date: 26/01/09 14:25 $
    $Author: Pauldavies $

\*===========================================================================*/

CREATE INDEX IX_Taxon_Occurrence_Data_Measurement_Qualifier_Key
ON Taxon_Occurrence_Data (Measurement_Qualifier_Key)