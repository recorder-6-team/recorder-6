/*===========================================================================*\
  Description:	
		Removes the Concept_Lineage and Language tables from exported data.

  Created:	March 2009

  Last revision information:
    $Revision: 1 $
    $Date: 25/03/09 16:36 $
    $Author: Pauldavies $

\*===========================================================================*/

DELETE FROM	Database_Relationship
WHERE		(Detail_Table = 'Concept_Lineage')
		OR	(Master_Table = 'Language')

GO