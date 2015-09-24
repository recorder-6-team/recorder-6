/*===========================================================================*\
  Description:	
	Modifies the Original_Field column in the Biotope table to be 30 characters
	instead of 20.

  Created:	February 2009

  Last revision information:
    $Revision: 1 $
    $Date: 9/02/09 14:54 $
    $Author: Pauldavies $

\*===========================================================================*/

ALTER TABLE dbo.Biotope ALTER COLUMN Original_Code varchar(30) NULL

GO