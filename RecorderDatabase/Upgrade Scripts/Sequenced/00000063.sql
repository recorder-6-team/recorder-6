/*===========================================================================*\
  Description:	
	-	Adds a new setting to the Settings table, for storing a list of
		designations on which to filter Taxon Lists.
	-	Adds a 'Status_Abbreviation' field to the Taxon_Designation_Type table.
	-	Grants Select, Delete, Update and Insert rights on Report_Field and
		Report_Attribute to R2k_Administrator.
	-	Removes the Taxon Status filters on the report wizard.

  Created:	January 2009

  Last revision information:
    $Revision: 3 $
    $Date: 10/02/09 10:19 $
    $Author: Pauldavies $

\*===========================================================================*/

IF NOT EXISTS (SELECT 1 FROM Setting WHERE Name = 'TaxDesList')
	INSERT INTO Setting (Name, DATA)
	VALUES ('TaxDesList', 'NHMSYS0020424779')

IF NOT EXISTS (SELECT 1 FROM Setting WHERE Name = 'DBListSep')
	INSERT INTO Setting (Name, DATA)
	VALUES ('DBListSep', ',')

GO

IF NOT EXISTS (	SELECT	1
				FROM	SysColumns
				WHERE	ID				=	Object_ID(N'dbo.Taxon_Designation_Type')
					AND	SysColumns.Name	=	'Status_Abbreviation'
					AND	ObjectProperty(ID, N'IsTable') = 1)
	ALTER TABLE	Taxon_Designation_Type
	ADD			Status_Abbreviation VARCHAR(20) NULL

GO

IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
BEGIN
	GRANT DELETE ON Report_Field TO R2k_Administrator
	GRANT UPDATE ON Report_Field TO R2k_Administrator
	GRANT SELECT ON Report_Field TO R2k_Administrator
	GRANT INSERT ON Report_Field TO R2k_Administrator
	GRANT DELETE ON Report_Attribute TO R2k_Administrator
	GRANT UPDATE ON Report_Attribute TO R2k_Administrator
	GRANT SELECT ON Report_Attribute TO R2k_Administrator
	GRANT INSERT ON Report_Attribute TO R2k_Administrator
END

GO

DELETE FROM Usable_Field WHERE Usable_Field_Key IN (
	'ABABABAB00000111',
	'ABABABAB00000112',
	'ABABABAB00000113',
	'ABABABAB00000114',
	'ABABABAB00000115'
)

GO

/*===========================================================================*\
  Description:	
			Adds a new table, Taxon_Designation_Set, which holds a list
			of sets of designations.

  Created:	January 2009

  Last revision information:
    $Revision: 3 $
    $Date: 10/02/09 10:19 $
    $Author: Pauldavies $

\*===========================================================================*/

IF NOT EXISTS (	SELECT *
				FROM   SysObjects 
				WHERE  Id = Object_Id(N'dbo.Taxon_Designation_Set') 
				AND    ObjectProperty(Id, N'IsTable') = 1)
	CREATE TABLE Taxon_Designation_Set (
		Taxon_Designation_Set_Key CHAR(16) 
		CONSTRAINT PK_Taxon_Designation_Set PRIMARY KEY CLUSTERED,
		Title VARCHAR(100) NOT NULL,
		Entered_By CHAR(16) NOT NULL,
		Entry_Date SMALLDATETIME NOT NULL,
		Changed_By CHAR(16) NULL,
		Changed_Date SMALLDATETIME NULL,
		System_Supplied_Data BIT NOT NULL,
		Custodian CHAR(8) NOT NULL
	)

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (	SELECT *
			FROM   SysObjects 
			WHERE  Id = Object_Id(N'dbo.Taxon_Designation_Set') 
			AND    ObjectProperty(Id, N'IsTable') = 1)
BEGIN
	PRINT 'Setting up security on procedure dbo.Taxon_Designation_Set'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
		GRANT SELECT ON dbo.Taxon_Designation_Set TO R2k_AddOnly
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT SELECT ON dbo.Taxon_Designation_Set TO R2k_Administrator
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT SELECT ON dbo.Taxon_Designation_Set TO R2k_FullEdit
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT SELECT ON dbo.Taxon_Designation_Set TO R2k_ReadOnly
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT SELECT ON dbo.Taxon_Designation_Set TO R2k_RecordCardsOnly
END
GO

/*===========================================================================*\
  Description:	
			Adds a new link table, Taxon_Designation_Set_Item, which links the
			Taxon_Designation_Set to the Taxon_Designation_Types it holds.

  Created:	January 2009

  Last revision information:
    $Revision: 3 $
    $Date: 10/02/09 10:19 $
    $Author: Pauldavies $

\*===========================================================================*/

IF NOT EXISTS (	SELECT *
				FROM   SysObjects 
				WHERE  Id = Object_Id(N'dbo.Taxon_Designation_Set_Item') 
				AND    ObjectProperty(Id, N'IsTable') = 1)
	CREATE TABLE Taxon_Designation_Set_Item (
		Taxon_Designation_Set_Item_Key CHAR(16)
		CONSTRAINT PK_Taxon_Designation_Set_Item PRIMARY KEY CLUSTERED,
		Taxon_Designation_Set_Key CHAR(16)
			CONSTRAINT	FK_Taxon_Designation_Set_Item_Taxon_Designation_Set
			FOREIGN KEY	(Taxon_Designation_Set_Key)
			REFERENCES	Taxon_Designation_Set	(Taxon_Designation_Set_Key),
		Taxon_Designation_Type_Key CHAR(16)
			CONSTRAINT	FK_Taxon_Designation_Set_Item_Taxon_Designation_Type
			FOREIGN KEY	(Taxon_Designation_Type_Key)
			REFERENCES	Taxon_Designation_Type	(Taxon_Designation_Type_Key),
		Entered_By CHAR(16) NOT NULL,
		Entry_Date SMALLDATETIME NOT NULL,
		Changed_By CHAR(16) NULL,
		Changed_Date SMALLDATETIME NULL,
		System_Supplied_Data BIT NOT NULL,
		Custodian CHAR(8) NOT NULL
	)

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (	SELECT *
			FROM   SysObjects 
			WHERE  Id = Object_Id(N'dbo.Taxon_Designation_Set_Item') 
			AND    ObjectProperty(Id, N'IsTable') = 1)
BEGIN
	PRINT 'Setting up security on procedure dbo.Taxon_Designation_Set_Item'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
		GRANT SELECT ON dbo.Taxon_Designation_Set_Item TO R2k_AddOnly
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT SELECT ON dbo.Taxon_Designation_Set_Item TO R2k_Administrator
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT SELECT ON dbo.Taxon_Designation_Set_Item TO R2k_FullEdit
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT SELECT ON dbo.Taxon_Designation_Set_Item TO R2k_ReadOnly
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT SELECT ON dbo.Taxon_Designation_Set_Item TO R2k_RecordCardsOnly
END
GO

/*===========================================================================*\
  Drop function before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'dbo.usp_Index_Taxon_Designation_Rebuild') 
	   AND    ObjectProperty(Id, N'IsProcedure') = 1)
    DROP PROCEDURE dbo.usp_Index_Taxon_Designation_Rebuild
GO

SET QUOTED_IDENTIFIER ON 
GO
SET ANSI_NULLS ON 
GO

/*===========================================================================*\
  Description:	
			Procedure which rebuilds the indexed list of Taxon Designations for
			the supplied list of Taxon_List_Keys. 

  Created:	January 2009

  Last revision information:
    $Revision: 3 $
    $Date: 10/02/09 10:19 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE dbo.usp_Index_Taxon_Designation_Rebuild
AS
	DECLARE	@KeyList	VARCHAR(100)
	
	SET		@KeyList	=	''
	
	SELECT	@KeyList	=	Data
	FROM	Setting
	WHERE	Name		=	'TaxDesList'

	DELETE FROM	Index_Taxon_Designation

	INSERT INTO	Index_Taxon_Designation	(
				Taxon_List_Item_Key,
				Taxon_Designation_Type_Key	)
	SELECT		ITN.TAXON_LIST_ITEM_KEY,
				TDES.TAXON_DESIGNATION_TYPE_KEY
	FROM		Index_Taxon_Name						ITN
	INNER JOIN	Index_Taxon_Name						ITN2
			ON	ITN2.Recommended_Taxon_List_Item_Key	=	ITN.Taxon_List_Item_Key
	INNER JOIN	Index_Taxon_Group						ITG
			ON	ITG.Contained_LIst_Item_Key				=	ITN2.Taxon_List_Item_Key
	INNER JOIN	Index_Taxon_Name						ITN3
			ON	ITN3.Taxon_List_Item_Key				=	ITG.Taxon_List_Item_Key
	INNER JOIN	Index_Taxon_Name						ITN4
			ON	ITN4.Recommended_Taxon_List_Item_Key	=	ITN3.Recommended_Taxon_List_Item_Key
	INNER JOIN	Taxon_designation						TDES
			ON	TDES.taxon_List_Item_Key				=	ITN4.Taxon_List_Item_Key
	INNER JOIN	Taxon_List_Version						TLV
			ON	TLV.Taxon_List_Version_Key				=	ITN4.Taxon_List_Version_Key

	WHERE		(@Keylist	LIKE '%' + TLV.Taxon_List_Key + '%'
					OR	@Keylist	=	''
					OR	TDES.System_Supplied_Data	=	0)
			AND	TDES.Date_To IS NULL
	GROUP BY	ITN.TAXON_LIST_ITEM_KEY,
				TDES.TAXON_DESIGNATION_TYPE_KEY
GO

SET QUOTED_IDENTIFIER OFF
GO
SET ANSI_NULLS OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Index_Taxon_Designation_Rebuild') AND SysStat & 0xf = 4)
BEGIN
    PRINT 'Setting up security on procedure usp_Index_Taxon_Designation_Rebuild'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Index_Taxon_Designation_Rebuild TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        GRANT EXECUTE ON dbo.usp_Index_Taxon_Designation_Rebuild TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop function before re-creating.
\*===========================================================================*/
IF Object_Id(N'dbo.ufn_GetDesignations') IS NOT NULL
	DROP FUNCTION ufn_GetDesignations
GO

/*===========================================================================*\
  Description:	
		Gets a concatenated string of the Taxon_Designation_Keys for a
		particular Taxon_List_Item_Key.

  Parameters:	
		@Taxon_List_Item_Key 		The Taxon_List_Item_Key of interest.
		@Output_Format				The field to output:
										1. Short_Name
										2. Long Name
										3. Kind
										4. Status_Abbreviation 
										5. Is designated: Yes/No
		@Taxon_Designation_Set_Key	The primary key of the Taxon_Designation_Set
									to look in.
		@Taxon_Designation_Type_Keys The primary keys of the Taxon_Designation_Types
									being searched for.

  Created:	Jan 2009

  Last revision information:
    $Revision: 3 $
    $Date: 10/02/09 10:19 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE FUNCTION dbo.ufn_GetDesignations
(
	@Taxon_List_Item_Key		CHAR(16),
	@Output_Format				SMALLINT,	-- 1. Short_Name
											-- 2. Long Name
											-- 3. Kind
											-- 4. Status_Abbreviation 
											-- 5. Is Designated: Yes/No
	@Taxon_Designation_Set_Key	CHAR(16) = NULL,
	@Taxon_Designation_Type_Keys VARCHAR(1000) = NULL
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
	
	SELECT	@ReturnValue	=	
				-- Blank when this is the first value, otherwise
				-- the previous string plus the seperator
				CASE
					WHEN @ReturnValue IS NULL THEN ''
					ELSE @ReturnValue + @Seperator
				END +
				-- Adds the required data based on the output format
				CASE @Output_Format
					WHEN 1 THEN TDT.Short_Name
					WHEN 2 THEN TDT.Long_Name
					WHEN 3 THEN TDT.Kind
					WHEN 4 THEN TDT.Status_Abbreviation
					ELSE 'Invalid output format.'
				END
	FROM	Index_Taxon_Designation		ITD
	JOIN	Taxon_Designation_Type		TDT
		ON	TDT.Taxon_Designation_Type_Key	=	ITD.Taxon_Designation_Type_Key
	JOIN	Taxon_Designation_Set_Item	TDSI
		ON	TDSI.Taxon_Designation_Type_Key	=	TDT.Taxon_Designation_Type_Key
	JOIN	Taxon_Designation_Set		TDS
		ON	TDS.Taxon_Designation_Set_Key	=	TDSI.Taxon_Designation_Set_Key
	WHERE	ITD.Taxon_List_Item_Key			=	@Taxon_List_Item_Key
			-- Filters by Taxon_Designation_Set if the Key is not null
		AND (@Taxon_Designation_Set_Key IS NULL
				OR	TDS.Taxon_Designation_Set_Key	=	@Taxon_Designation_Set_Key)
			-- Filters by Taxon_Designation_Set_Items if the Key list is not null
		AND	(@Taxon_Designation_Type_Keys IS NULL
				OR	@Taxon_Designation_Type_Keys LIKE 
						'%' + TDSI.Taxon_Designation_Type_Key + '%')

	-- If the return value is null then there were no matching designations
	IF @Output_Format = 5
		SET @ReturnValue = CASE 
							WHEN @ReturnValue IS NULL THEN 'No'
							ELSE 'Yes'
						   END
	
	RETURN	@ReturnValue
END
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * 
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[ufn_GetDesignations]')
	   AND    Type IN ('FN', 'IF', 'TF'))
	BEGIN
    	PRINT 'Setting up security on function ufn_GetDesignations'
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
	        	GRANT EXECUTE ON dbo.ufn_GetDesignations TO [R2k_AddOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
			GRANT EXECUTE ON dbo.ufn_GetDesignations TO [R2k_Administrator]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
			GRANT EXECUTE ON dbo.ufn_GetDesignations TO [R2k_FullEdit]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
			GRANT EXECUTE ON dbo.ufn_GetDesignations TO [R2k_ReadOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
			GRANT EXECUTE ON dbo.ufn_GetDesignations TO [R2k_RecordCardsOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
	        	GRANT EXECUTE ON dbo.ufn_GetDesignations TO [Dev - JNCC SQL]
	END
GO

/*===========================================================================*\
  Description:	
			Adds a new index table, Index_Taxon_Designation, which can hold
			a list of keys for associated Taxon_List_Items and Taxon_Designations
			for one or more Taxon_Lists.

  Created:	January 2009

  Last revision information:
    $Revision: 3 $
    $Date: 10/02/09 10:19 $
    $Author: Pauldavies $

\*===========================================================================*/

IF NOT EXISTS (	SELECT *
				FROM   SysObjects 
				WHERE  Id = Object_Id(N'dbo.Index_Taxon_Designation') 
				AND    ObjectProperty(Id, N'IsTable') = 1)
BEGIN
	CREATE TABLE Index_Taxon_Designation (
		Taxon_List_Item_Key CHAR(16)
			CONSTRAINT	FK_Index_Taxon_Designation_Taxon_List_Item 
			FOREIGN KEY	(Taxon_List_Item_Key)
			REFERENCES	Taxon_List_Item (Taxon_List_Item_Key),
		Taxon_Designation_Type_Key CHAR(16)
			CONSTRAINT	FK_Index_Taxon_Designation_Taxon_Designation_Type_Key
			FOREIGN KEY	(Taxon_Designation_Type_Key)
			REFERENCES	Taxon_Designation_Type (Taxon_Designation_Type_Key),
		CONSTRAINT PK_Index_Taxon_Designation PRIMARY KEY NONCLUSTERED 
		(
			Taxon_List_Item_Key ASC,
			Taxon_Designation_Type_Key ASC
		)
	)
	
	-- Creates indices for the table
	CREATE INDEX IX_Index_Taxon_Designation_Taxon_List_Item_Key
	ON Index_Taxon_Designation (Taxon_List_Item_Key)

	CREATE INDEX IX_Index_Taxon_Designation_Taxon_Designation_Key
	ON Index_Taxon_Designation (Taxon_Designation_Type_Key)

	-- Fills in the table
	EXECUTE		dbo.usp_Index_Taxon_Designation_Rebuild
END

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (	SELECT *
			FROM   SysObjects 
			WHERE  Id = Object_Id(N'dbo.Index_Taxon_Designation') 
			AND    ObjectProperty(Id, N'IsTable') = 1)
BEGIN
	PRINT 'Setting up security on procedure dbo.Index_Taxon_Designation'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
		GRANT SELECT ON dbo.Index_Taxon_Designation TO R2k_AddOnly
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT SELECT ON dbo.Index_Taxon_Designation TO R2k_Administrator
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT SELECT ON dbo.Index_Taxon_Designation TO R2k_FullEdit
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT SELECT ON dbo.Index_Taxon_Designation TO R2k_ReadOnly
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT SELECT ON dbo.Index_Taxon_Designation TO R2k_RecordCardsOnly
END
GO

/*	Adds a record to the Usable_Table table, adding the Index_Taxon_Designation table as a 
	usable table, and another for the Taxon_Designation it links to. */
IF NOT EXISTS(SELECT 1 FROM dbo.Usable_Table WHERE Usable_Table_Key = 'ABABABAB0000006C')
	INSERT INTO dbo.Usable_Table (
			Usable_Table_Key,
			Table_Name,
			Link_Table,
			Link,
			Apply_To,
			Join_Order )
	VALUES(
			'ABABABAB0000006C',
			'Index_Taxon_Designation',
			'Taxon_List_Item',
			'Taxon_List_Item.Taxon_List_Item_Key = Index_Taxon_Designation.Taxon_List_Item_Key',
			'A',
			4 )