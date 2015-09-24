/*===========================================================================*\
  Description:	Adds new table to the Usable_Tables for the report wizard- Admin_Areas
		(linked from sample), also added Sample_Admin_Areas to perform the link.
		
		Renames the Administrative Areas attribute in Sample to Sample Administrative Areas
		so as to make it clearer what it does.
		
		Adds new Administrative Areas attributes for Sample Location and Survey Event Location.

  Created:	April 2009

  Last revision information:
    $Revision: 1 $
    $Date: 3/04/09 13:55 $
    $Author: Pauldavies $

\*===========================================================================*/

DELETE FROM Usable_Table WHERE Usable_Table_Key IN ('ABABABAB0000006D', 'ABABABAB0000006E')

INSERT INTO Usable_Table	(
	Usable_Table_Key,
	Table_Name,
	Link_Table,
	Link,
	Additional_Link,
	Apply_To,
	Join_Order
)
VALUES	(
	'ABABABAB0000006D',
	'Admin_Area ASam',
	'Sample_Admin_Areas SAA',
	'ASam.Admin_Area_Key = SAA.Admin_Area_Key',
	NULL,
	'A',
	3
)

INSERT INTO Usable_Table	(
	Usable_Table_Key,
	Table_Name,
	Link_Table,
	Link,
	Additional_Link,
	Apply_To,
	Join_Order
)
VALUES	(
	'ABABABAB0000006E',
	'Sample_Admin_Areas SAA',
	'Sample',
	'SAA.Sample_Key = Sample.Sample_Key',
	NULL,
	'A',
	2
)

UPDATE	Report_Attribute
SET		Item_Name				=	'Sample Administrative Areas'
WHERE	Report_Attribute_Key	=	'NBNSYS00000000BB'

DELETE FROM Report_Attribute WHERE Report_Attribute_Key IN ('NBNSYS00000000BC', 'NBNSYS00000000BD')

INSERT INTO Report_Attribute	(
	Report_Attribute_Key,
	Item_Group,
	Source_Table,
	Item_Name,
	Attribute_SQL,
	Report_Join_Key,
	Report_Where_Key,
	Entered_By,
	Entry_Date,
	System_Supplied_Data
)
VALUES	(
	'NBNSYS00000000BC',
	'Sample\Location',
	'SAMPLE',
	'Sample Location Administrative Areas',
	'#REPORT_OUTPUT.[Sample Location Administrative Areas] = dbo.ufn_GetLocationAdminAreas(LOCATION.LOCATION_KEY)',
	'NBNSYS0000000009',
	NULL,
	NULL,
	GETDATE(),
	1
)

INSERT INTO Report_Attribute	(
	Report_Attribute_Key,
	Item_Group,
	Source_Table,
	Item_Name,
	Attribute_SQL,
	Report_Join_Key,
	Report_Where_Key,
	Entered_By,
	Entry_Date,
	System_Supplied_Data
)
VALUES	(
	'NBNSYS00000000BD',
	'Event',
	'SURVEY_EVENT',
	'Event Location Administrative Areas',
	'#REPORT_OUTPUT.[Event Location Administrative Areas] = dbo.ufn_GetLocationAdminAreas(LOCATION.LOCATION_KEY)',
	'NBNSYS0000000005',
	NULL,
	NULL,
	GETDATE(),
	1
)

DELETE FROM Report_Field WHERE Report_Field_Key IN ('NBNSYS00000000CD', 'NBNSYS00000000CE')

INSERT INTO Report_Field	(
	Report_Field_Key,
	Report_Attribute_Key,
	Field_Item_Name,
	Field_Type,
	Field_Size,
	Entry_Date,
	System_Supplied_Data
)
VALUES	(
	'NBNSYS00000000CD',
	'NBNSYS00000000BC',
	'Sample Location Administrative Areas',
	'varchar',
	1000,
	GetDate(),
	1
)

INSERT INTO Report_Field	(
	Report_Field_Key,
	Report_Attribute_Key,
	Field_Item_Name,
	Field_Type,
	Field_Size,
	Entry_Date,
	System_Supplied_Data
)
VALUES	(
	'NBNSYS00000000CE',
	'NBNSYS00000000BD',
	'Event Location Administrative Areas',
	'varchar',
	1000,
	GetDate(),
	1
)

GO

/*===========================================================================*\
  Drop function before re-creating.
\*===========================================================================*/
IF Object_Id(N'dbo.ufn_GetLocationAdminAreas') IS NOT NULL
	DROP FUNCTION ufn_GetLocationAdminAreas
GO

/*===========================================================================*\
  Description:	
		Gets a concatenated string of Admin Areas for a Location.

  Parameters:	
		@Location_Key 		The key of the Location of interest.

  Created:	April 2009

  Last revision information:
    $Revision: 1 $
    $Date: 3/04/09 13:55 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE FUNCTION dbo.ufn_GetLocationAdminAreas
(
	@Location_Key		CHAR(16)
)
RETURNS	VARCHAR(1000)

AS
BEGIN
	DECLARE @Seperator	VARCHAR(100)
	
	-- Gets the Seperator from the settings table.
	SELECT	@Seperator	=	Data
	FROM	Setting
	WHERE	Name		=	'DBListSep' 
	
	DECLARE	@ReturnValue	VARCHAR(1000)
	
	SELECT		@ReturnValue	=	
					-- Blank when this is the first value, otherwise
					-- the previous string plus the seperator
					CASE
						WHEN @ReturnValue IS NULL THEN ''
						ELSE @ReturnValue + @Seperator
					END + AA.Item_Name
	FROM		Location_Admin_Areas		LAA
	INNER JOIN	Admin_Area				AA
			ON	LAA.Admin_Area_Key		=	AA.Admin_Area_Key
	WHERE		LAA.Location_Key		=	@Location_Key
	
	RETURN	@ReturnValue
END
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * 
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[ufn_GetLocationAdminAreas]')
	   AND    Type IN ('FN', 'IF', 'TF'))
	BEGIN
    	PRINT 'Setting up security on function ufn_GetLocationAdminAreas'
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
	        	GRANT EXECUTE ON dbo.ufn_GetLocationAdminAreas TO [R2k_AddOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
			GRANT EXECUTE ON dbo.ufn_GetLocationAdminAreas TO [R2k_Administrator]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
			GRANT EXECUTE ON dbo.ufn_GetLocationAdminAreas TO [R2k_FullEdit]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
			GRANT EXECUTE ON dbo.ufn_GetLocationAdminAreas TO [R2k_ReadOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
			GRANT EXECUTE ON dbo.ufn_GetLocationAdminAreas TO [R2k_RecordCardsOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
	        GRANT EXECUTE ON dbo.ufn_GetLocationAdminAreas TO [Dev - JNCC SQL]
	END
GO