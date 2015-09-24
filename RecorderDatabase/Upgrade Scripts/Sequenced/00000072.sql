/*===========================================================================*\
  Description:
	Import Wizard fix. Missing relationship for fields present in the 
	Specimen table.
	Also update column types using "text" to "varchar(8000)" because of 
	subsequent queries using groupings.
	Additional entries in IW_Column_Type_Pattern to get Spatial Ref System
	correctly automapped.

  Created:	April 2009

  Last revision information:
    $Revision: 3 $
    $Date: 28/04/09 16:50 $
    $Author: Ericsalmon $

\*===========================================================================*/

IF NOT EXISTS(
		SELECT 1 
		FROM   IW_Table_Rule_Output_Field 
		WHERE  IW_Table_Rule_Key   = 'SYSTEM010000000G' 
		AND    IW_Output_Field_Key = 'SYSTEM010000001A'
)
	INSERT INTO IW_Table_Rule_Output_Field (IW_Table_Rule_Key, IW_Output_Field_Key, Entered_By, Entry_Date, System_Supplied_Data)
	VALUES ('SYSTEM010000000G', 'SYSTEM010000001A', 'NBNSYS0000000001', GETDATE(), 1)


/*===========================================================================*\
  Change all TEXT to VARCHAR(8000) in IW_Column_Type.
\*===========================================================================*/
UPDATE IW_Column_Type
SET    Field_Type = 'varchar(8000)'
WHERE  Field_Type = 'text'


/*===========================================================================*\
  Additional entries in IW_Column_Type_Pattern to get Spatial Ref System 
  correctly automapped.
\*===========================================================================*/

INSERT INTO IW_Column_Type_Pattern (IW_Column_Type_Key, Pattern, Exclude_Match, Entered_By, System_Supplied_Data) 
VALUES ('SYSTEM0100000001', 'spatial%ref%sys%', 1, 'SYSTEM0000000001', 1)

INSERT INTO IW_Column_Type_Pattern (IW_Column_Type_Key, Pattern, Exclude_Match, Entered_By, System_Supplied_Data) 
VALUES ('SYSTEM0100000001', 'spatial%sys%', 1, 'SYSTEM0000000001', 1)

