/*===========================================================================*\
  Description:
	Changes the list seperator to a comma followed by a space rather than just 
	a comma.

  Created:	April 2009

  Last revision information:
    $Revision: 1 $
    $Date: 15/04/09 14:25 $
    $Author: Pauldavies $

\*===========================================================================*/

UPDATE	Setting
SET		Data		=	', '
WHERE	Name		=	'DBListSep'