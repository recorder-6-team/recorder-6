/*
  Removes the rows from Database_Relationship which are responsible
  for exporting Survey_Tags.

  Last Revision Details:
    $Revision: 1 $
    $Date: 27/04/09 15:51 $
    $Author: Pauldavies $
  
*/

DELETE FROM	Database_Relationship
WHERE		Relationship_Key	IN	('NBNSYS000000004C', 
									 'NBNSYS000000004J')