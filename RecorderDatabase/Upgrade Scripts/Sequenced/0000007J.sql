SET QUOTED_IDENTIFIER ON
SET ANSI_NULLS ON
GO

-- create the new table

IF OBJECT_ID('dbo.Relation_Role_Type') IS NOT NULL
	DROP TABLE dbo.Relation_Role_Type

GO

CREATE TABLE dbo.Relation_Role_Type (
	Relation_Role_Type_Key		CHAR(16)		NOT NULL,
	Short_Name					VARCHAR(30)		NOT NULL,
	Long_Name					VARCHAR(100)	NULL,
	"Description"				TEXT			NULL,
	Entered_By					CHAR(16)		NOT NULL,
	Entry_Date					SMALLDATETIME	NOT NULL,
	Changed_By					CHAR(16)		NULL,
	Changed_Date				SMALLDATETIME	NULL,
	System_Supplied_Data		BIT				NOT NULL,
	Custodian					CHAR(8)			NULL,
	CONSTRAINT PK_Relation_Role_Type_Key 
			PRIMARY KEY NONCLUSTERED (Relation_Role_Type_Key)
)
GO

GRANT INSERT ON dbo.Relation_Role_Type TO R2k_AddOnly
GO
GRANT SELECT ON dbo.Relation_Role_Type TO R2k_AddOnly, R2k_Administrator,
		R2k_FullEdit, R2k_ReadOnly, R2k_RecordCardsOnly
GO
GRANT DELETE ON dbo.Relation_Role_Type TO R2k_Administrator, R2k_FullEdit
GO
GRANT INSERT ON dbo.Relation_Role_Type TO R2k_Administrator, R2k_FullEdit
GO
GRANT UPDATE ON dbo.Relation_Role_Type TO R2k_Administrator, R2k_FullEdit
GO

-- add to the term list
IF NOT EXISTS (SELECT 1
				FROM	Term_List
				WHERE	"Table" = 'Relation_Role_Type')
	INSERT INTO  Term_List (
		"Table",
		Key_Field,
		"Description",
		System_Supplied_Data,
		Additional_Fields,
		Linked_Table
	)
	VALUES (
		'Relation_Role_Type',
		'Relation_Role_Type_Key',
		'Relation Role Type',
		1,
		0,
		NULL
	)
GO

-- create old hardcoded values
IF NOT EXISTS (SELECT 1
				FROM dbo.Relation_Role_Type
				WHERE	Relation_Role_Type_Key = 'NBNSYS0000000000')
BEGIN 
	INSERT INTO dbo.Relation_Role_Type (
		Relation_Role_Type_Key,
		Short_Name,
		Entered_By,
		Entry_Date,
		System_Supplied_Data
	)
	VALUES (
		'NBNSYS0000000000',
		'Employee',
		'TESTDATA00000001',
		GETDATE(),
		1
	)

	INSERT INTO dbo.Relation_Role_Type (
		Relation_Role_Type_Key,
		Short_Name,
		Entered_By,
		Entry_Date,
		System_Supplied_Data
	)
	VALUES (
		'NBNSYS0000000001',
		'Member',
		'TESTDATA00000001',
		GETDATE(),
		1
	)

	INSERT INTO dbo.Relation_Role_Type (
		Relation_Role_Type_Key,
		Short_Name,
		Entered_By,
		Entry_Date,
		System_Supplied_Data
	)
	VALUES (
		'NBNSYS0000000002',
		'Recorder',
		'TESTDATA00000001',
		GETDATE(),
		1
	)

	INSERT INTO dbo.Relation_Role_Type (
		Relation_Role_Type_Key,
		Short_Name,
		Entered_By,
		Entry_Date,
		System_Supplied_Data
	)
	VALUES (
		'NBNSYS0000000004',
		'Volunteer',
		'TESTDATA00000001',
		GETDATE(),
		1
	)
END

GO

IF OBJECT_ID('dbo.usp_RelationRoleType_Select') IS NOT NULL
	DROP PROCEDURE dbo.usp_RelationRoleType_Select
GO

/*===========================================================================
Description: 
	Gets all the relation role type records for display in a dropdown
Parameters: 
	
Created: 
	05/10/2009
Last revision information: 
	$Revision: 3 $ 
	$Date: 5/10/09 12:11 $ 
	$Author: Bonnerearle $ 
===========================================================================*/
CREATE PROCEDURE dbo.usp_RelationRoleType_Select
AS

SET NOCOUNT ON

SELECT	Short_Name
FROM	dbo.Relation_Role_Type
ORDER BY Short_Name

GO

GRANT EXECUTE ON dbo.usp_RelationRoleType_Select TO 
	R2k_AddOnly, 
	R2k_Administrator,
	R2k_FullEdit,
	R2k_ReadOnly,
	R2k_RecordCardsOnly

IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
	GRANT EXECUTE ON dbo.usp_RelationRoleType_Select TO "Dev - JNCC SQL"
GO
