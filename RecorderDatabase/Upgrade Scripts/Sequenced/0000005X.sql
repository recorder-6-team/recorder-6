/*
  Adds a new column to the IW_Matched_Species table which allows
  temporary matches to be stored against a particular user.

  Last Revision Details:
    $Revision: 1 $
    $Date: 12/01/09 9:45 $
    $Author: Pauldavies $
  
*/

ALTER TABLE IW_Matched_Species
	ADD Temp_User_ID CHAR(16)