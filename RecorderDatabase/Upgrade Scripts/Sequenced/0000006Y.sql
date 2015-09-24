/*===========================================================================*\
  Description:	
			Sets the export wizard to follow links between Name records via
			Name_Relation (previously was getting stuck in Name_Relation without
			coming out the other side).

  Created:	April 2009

  Last revision information:
    $Revision: 1 $
    $Date: 21/04/09 10:04 $
    $Author: Pauldavies $

\*===========================================================================*/

UPDATE	Database_Relationship
SET		Follow_Up	=	1
WHERE	Relationship_Key IN ('NBNSYS000000002C', 'NBNSYS000000002D')