/*===========================================================================*\
  Description:
	Table for linking between the Sample and Admin_Areas tables.

  Created:	March 2009

  Last revision information:
    $Revision: 2 $
    $Date: 12/03/09 10:29 $
    $Author: Simonwood $

\*===========================================================================*/

IF NOT EXISTS (SELECT 1 FROM SysObjects WHERE Id = OBJECT_ID('dbo.Sample_Admin_Areas') AND ObjectProperty(Id, N'IsUserTable') = 1)
	CREATE TABLE dbo.Sample_Admin_Areas (
		Sample_Admin_Areas_Key	CHAR(16)		NOT NULL
			CONSTRAINT	PK_Sample_Admin_Areas 
			PRIMARY KEY NONCLUSTERED ,
		Admin_Area_Key			CHAR(16)		NOT NULL
			CONSTRAINT	FK_Sample_Admin_Areas_Admin_Area 
			FOREIGN KEY	(Admin_Area_Key) 
			REFERENCES	Admin_Area	(Admin_Area_Key),
		Sample_Key				CHAR(16)		NOT NULL
			CONSTRAINT	FK_Sample_Admin_Areas_Sample
			FOREIGN KEY	(Sample_Key)
			REFERENCES	Sample		(Sample_Key),
		Entered_By				CHAR(16)		NOT NULL,
		Entry_Date				SMALLDATETIME	NOT NULL
			CONSTRAINT	DF_SampleAdminAreas_EntryDate
			DEFAULT		(getdate()),
		System_Supplied_Data	BIT				NOT NULL
			CONSTRAINT	DF_SampleAdminAreas_SystemSuppliedData
			DEFAULT		(0),
		Custodian				CHAR(8)			NULL
	)

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (	SELECT 1
			FROM   SysObjects 
			WHERE  Id = Object_Id(N'dbo.Sample_Admin_Areas') 
			AND    ObjectProperty(Id, N'IsTable') = 1)
BEGIN
	PRINT 'Setting up security on procedure dbo.Sample_Admin_Areas'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
		GRANT SELECT, INSERT ON dbo.Sample_Admin_Areas TO R2k_AddOnly
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT SELECT, UPDATE , INSERT , DELETE ON dbo.Sample_Admin_Areas TO R2k_Administrator
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT SELECT, UPDATE , INSERT , DELETE ON dbo.Sample_Admin_Areas TO R2k_FullEdit
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT SELECT ON dbo.Sample_Admin_Areas TO R2k_ReadOnly
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT SELECT ON dbo.Sample_Admin_Areas TO R2k_RecordCardsOnly
END
GO

/*===========================================================================*\
  Description:
	Adds records to Database_Relationship representing the link between Sample,
	Sample_Admin_Areas, and Admin_Areas, so that they are picked up by the export
	wizard.

  Created:	March 2009

  Last revision information:
    $Revision: 2 $
    $Date: 12/03/09 10:29 $
    $Author: Simonwood $

\*===========================================================================*/

-- Link Sample to Sample_Admin_Areas
INSERT INTO Database_Relationship (
	Relationship_Key,
	Relationship_Name,
	Master_Table,
	Master_Field,
	Detail_Table,
	Detail_Field,
	Follow_Up,
	Follow_Down,
	One_To_One
)
VALUES (
	'NBNSYS000000004K',
	'SampleSample_Admin_Areas',
	'Sample',
	'Sample_Key',
	'Sample_Admin_Areas',
	'Sample_Key',
	0,
	1,
	0
)

GO

-- Link Sample_Admin_Areas to Admin_Areas
INSERT INTO Database_Relationship (
	Relationship_Key,
	Relationship_Name,
	Master_Table,
	Master_Field,
	Detail_Table,
	Detail_Field,
	Follow_Up,
	Follow_Down,
	One_To_One
)
VALUES (
	'NBNSYS000000004L',
	'Admin_AreaSample_Admin_Areas',
	'Admin_Area',
	'Admin_Area_Key',
	'Sample_Admin_Areas',
	'Admin_Area_Key',
	0,
	0,
	0
)

GO

/*===========================================================================*\
  Drop function before re-creating.
\*===========================================================================*/
IF Object_Id(N'dbo.ufn_GetSampleAdminAreas') IS NOT NULL
	DROP FUNCTION ufn_GetSampleAdminAreas
GO

/*===========================================================================*\
  Description:	
		Gets a concatenated string of Admin Areas for a Sample.

  Parameters:	
		@Sample_Key 		The key of the Sample of interest.

  Created:	Jan 2009

  Last revision information:
    $Revision: 2 $
    $Date: 12/03/09 10:29 $
    $Author: Simonwood $

\*===========================================================================*/
CREATE FUNCTION dbo.ufn_GetSampleAdminAreas
(
	@Sample_Key		CHAR(16)
)
RETURNS	VARCHAR(1000)

AS
BEGIN
	DECLARE @Seperator	VARCHAR(100)
	
	-- Gets the Seperator from the settings table.
	SELECT	@Seperator	=	Data
	FROM	Setting
	WHERE	"Name"		=	'DBListSep' 
	
	DECLARE	@ReturnValue	VARCHAR(1000)
	
	SELECT		@ReturnValue	=	
					-- Blank when this is the first value, otherwise
					-- the previous string plus the seperator
					CASE
						WHEN @ReturnValue IS NULL THEN ''
						ELSE @ReturnValue + @Seperator
					END + AA.Item_Name
	FROM		Sample_Admin_Areas		SAA
	INNER JOIN	Admin_Area				AA
			ON	SAA.Admin_Area_Key		=	AA.Admin_Area_Key
	WHERE		SAA.Sample_Key			=	@Sample_Key
	
	RETURN	@ReturnValue
END
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * 
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[ufn_GetSampleAdminAreas]')
	   AND    Type IN ('FN', 'IF', 'TF'))
	BEGIN
    	PRINT 'Setting up security on function ufn_GetSampleAdminAreas'
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
	        	GRANT EXECUTE ON dbo.ufn_GetSampleAdminAreas TO [R2k_AddOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
			GRANT EXECUTE ON dbo.ufn_GetSampleAdminAreas TO [R2k_Administrator]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
			GRANT EXECUTE ON dbo.ufn_GetSampleAdminAreas TO [R2k_FullEdit]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
			GRANT EXECUTE ON dbo.ufn_GetSampleAdminAreas TO [R2k_ReadOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
			GRANT EXECUTE ON dbo.ufn_GetSampleAdminAreas TO [R2k_RecordCardsOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
	        GRANT EXECUTE ON dbo.ufn_GetSampleAdminAreas TO [Dev - JNCC SQL]
	END
GO

-- Adds the attribute for Sample_Administration_Areas to the report wizard output.
INSERT INTO Report_Attribute (
	Report_Attribute_Key,
	Item_Group,
	Source_Table,
	Item_Name,
	Attribute_SQL,
	Report_Join_Key,
	System_Supplied_Data
)
VALUES (
	'NBNSYS00000000BB',
	'Sample',
	'SAMPLE',
	'Administrative Areas',
	'#REPORT_OUTPUT.[Sample Administrative Areas] = dbo.ufn_GetSampleAdminAreas(SAMPLE.SAMPLE_KEY)',
	'NBNSYS0000000007',
	1
)
GO

-- Adds the field for Sample_Administration_Areas to the report wizard output.
INSERT INTO Report_Field (
	Report_Field_Key,
	Report_Attribute_Key,
	Field_Item_Name,
	Field_Type,
	Field_Size,
	System_Supplied_Data
)
VALUES (
	'NBNSYS00000000CA',
	'NBNSYS00000000BB',
	'Sample Administrative Areas',
	'varchar',
	1000,
	1
)
GO