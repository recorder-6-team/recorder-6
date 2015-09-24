SET QUOTED_IDENTIFIER ON
SET ANSI_NULLS ON
GO

/*============================================================================*\
	VI 21086 / CCN364: Insert match rules, column types and output fields
\*============================================================================*/

/*********************************************************************************************
	Match rules
*********************************************************************************************/

-- Create Match Tables for Determiner Roles and Types
CREATE TABLE IW_Matched_Determiner_Roles 
(
	Matched_Value varchar(100) NOT NULL,
	Matched_Key char(16) NOT NULL
)


CREATE TABLE IW_Matched_Determination_Types
(
	Matched_Value varchar(100) NOT NULL,
	Matched_Key char(16) NOT NULL
)

GO

-- Insert Match Rule Records for Determination Types and Determiner Roles
INSERT INTO dbo.IW_Match_Rule
           (IW_Match_Rule_Key,
           Sequence,
           Item_Name,
           Control_Type,
           Remembered_Matches_Procedure,
           Match_Procedure,
           Record_Matches_Procedure,
           New_Entry_Procedure,
           Requires_Checklist,
           Set_Match_Procedure,
           Table_Create_SQL,
           Search_Type,
           Termlist_Select_Procedure,
           Entered_By,
           Entry_Date,
           System_Supplied_Data)
     VALUES
           ('SYSTEM010000000C',
           12,
           'DeterminerRoles',
           1,
           'usp_IWMatchRemembered_DeterminerRoles',
           'usp_IWMatch_DeterminerRoles',
           'usp_IWMatchRecord_DeterminerRoles',
           'usp_IWMatchNewEntry_DeterminerRole',
           0,
           'usp_IWMatchSet_DeterminerRole',
           'CREATE TABLE #DeterminerRoles(     Import_Value VARCHAR(100) COLLATE SQL_Latin1_General_CP1_CI_AS PRIMARY KEY,     
													Match_Count INT,     Match_Value VARCHAR(100) COLLATE SQL_Latin1_General_CP1_CI_AS,     
														Match_Key CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS,     Manual_Match BIT DEFAULT 0,   Remembered BIT DEFAULT 0  )  ',
           -1,
           'usp_DeterminerRoles_Select',
           'NBNSYS0000000004',
           getdate(),
           1)

INSERT INTO dbo.IW_Match_Rule
           (IW_Match_Rule_Key,
           Sequence,
           Item_Name,
           Control_Type,
           Remembered_Matches_Procedure,
           Match_Procedure,
           Record_Matches_Procedure,
           New_Entry_Procedure,
           Requires_Checklist,
           Set_Match_Procedure,
           Table_Create_SQL,
           Search_Type,
           Termlist_Select_Procedure,
           Entered_By,
           Entry_Date,
           System_Supplied_Data)
     VALUES
           ('SYSTEM010000000D',
           12,
           'DeterminationTypes',
           1,
           'usp_IWMatchRemembered_DeterminationTypes',
           'usp_IWMatch_DeterminationTypes',
           'usp_IWMatchRecord_DeterminationTypes',
           'usp_IWMatchNewEntry_DeterminationType',
           0,
           'usp_IWMatchSet_DeterminationType',
           'CREATE TABLE #DeterminationTypes(     Import_Value VARCHAR(100) COLLATE SQL_Latin1_General_CP1_CI_AS PRIMARY KEY,     
													Match_Count INT,     Match_Value VARCHAR(100) COLLATE SQL_Latin1_General_CP1_CI_AS,     
														Match_Key CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS,     Manual_Match BIT DEFAULT 0,   Remembered BIT DEFAULT 0  )  ',
           -1,
           'usp_DeterminationTypes_Select',
           'NBNSYS0000000004',
           getdate(),
           1)

GO

/*********************************************************************************************
	Determiner Role
*********************************************************************************************/

-- Insert Column Type record for Determiner Role
INSERT INTO IW_Column_Type (
		IW_Column_Type_Key,
		Class_Name,
		Item_Name,
		Required,
		Commonly_Used,
		Parser_Class_Name,
		Maximum_Length,
		Entered_By,
		Entry_Date,		
		System_Supplied_Data)
	VALUES (
		'SYSTEM010000000V',
		'TColumnType',
		'Determiner Role',
		0,
		0,
		'TRequiredTextParser',
		100,
		'NBNSYS0000000004',
		GetDate(),
		1)
GO

-- Define this as a matched determiner role column
INSERT INTO IW_Column_Type_Match_Rule (
		IW_Column_Type_Key,
		IW_Match_Rule_Key,
		Field_Index,
		Entered_By,
		Entry_Date,
		System_Supplied_Data
	)
	VALUES (
		'SYSTEM010000000V',
		'SYSTEM010000000C',
		0,
		'NBNSYS0000000004',
		GetDate(),
		1)
GO


-- Insert Pattern matching record for determiner role
INSERT INTO IW_Column_Type_Pattern (
		IW_Column_Type_Key,
		Pattern,
		Exclude_Match,
		Entered_By,
		Entry_Date,
		System_Supplied_Data)
	VALUES (
		'SYSTEM010000000V',
		'det%role%',
		0,
		'NBNSYS0000000004',
		GetDate(),
		1)

--The old Determiner name column type must not match this pattern
INSERT INTO IW_Column_Type_Pattern (
		IW_Column_Type_Key,
		Pattern,
		Exclude_Match,
		Entered_By,
		Entry_Date,
		System_Supplied_Data)
	VALUES (
		'SYSTEM0100000005',
		'det%role%',
		1,
		'NBNSYS0000000004',
		GetDate(),
		1)

GO

/*********************************************************************************************
	Determination Type
*********************************************************************************************/

-- Insert Column Type record for Determiner Type
INSERT INTO IW_Column_Type (
		IW_Column_Type_Key,
		Class_Name,
		Item_Name,
		Required,
		Commonly_Used,
		Parser_Class_Name,
		Maximum_Length,
		Entered_By,
		Entry_Date,
		System_Supplied_Data)
	VALUES (
		'SYSTEM010000000W',
		'TColumnType',
		'Determination Type',
		0,
		0,
		'TRequiredTextParser',
		100,
		'NBNSYS0000000004',
		GetDate(),
		1)
GO

-- Define this as a matched determination type column
INSERT INTO IW_Column_Type_Match_Rule (
		IW_Column_Type_Key,
		IW_Match_Rule_Key,
		Field_Index,
		Entered_By,
		Entry_Date,
		System_Supplied_Data
	)
	VALUES (
		'SYSTEM010000000W',
		'SYSTEM010000000D',
		0,
		'NBNSYS0000000004',
		GetDate(),
		1)
GO

-- Insert Pattern matching record for determination type
INSERT INTO IW_Column_Type_Pattern (
		IW_Column_Type_Key,
		Pattern,
		Exclude_Match,
		Entered_By,
		Entry_Date,
		System_Supplied_Data)
	VALUES (
		'SYSTEM010000000W',
		'det%type%',
		0,
		'NBNSYS0000000004',
		GetDate(),
		1)

INSERT INTO IW_Column_Type_Pattern (
		IW_Column_Type_Key,
		Pattern,
		Exclude_Match,
		Entered_By,
		Entry_Date,
		System_Supplied_Data)
	VALUES (
		'SYSTEM0100000005',
		'det%type%',
		1,
		'NBNSYS0000000004',
		GetDate(),
		1)
GO

/*********************************************************************************************
	Output Fields
*********************************************************************************************/

-- Change Determiner_Role and Determination_Type to use generated fields
-- instead of hard-coded values

UPDATE	IW_OUTPUT_FIELD
SET		Literal_Value			= NULL
WHERE	IW_Output_Field_Key		= 'SYSTEM010000001D'
OR		IW_Output_Field_Key		= 'SYSTEM010000001E'

UPDATE	IW_OUTPUT_FIELD
SET		Generating_Class_name	= 'TDeterminationTypeFieldGenerator'
WHERE	IW_Output_Field_Key		= 'SYSTEM010000001D'

UPDATE	IW_OUTPUT_FIELD
SET		Generating_Class_name	= 'TDeterminerRoleFieldGenerator'
WHERE	IW_Output_Field_Key		= 'SYSTEM010000001E'

GO