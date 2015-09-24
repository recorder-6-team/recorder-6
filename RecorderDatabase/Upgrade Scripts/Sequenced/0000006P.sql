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
    $Revision: 1 $
    $Date: 27/03/09 12:43 $
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
			ON	ITN.Recommended_Taxon_List_Item_Key		=	ITN2.Taxon_List_Item_Key
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
    $Revision: 1 $
    $Date: 27/03/09 12:43 $
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
	WHERE	Name		=	'DBListSep' 
	
	DECLARE	@ReturnValue	VARCHAR(1000)

	DECLARE @OutputValues	TABLE (
		Item	VARCHAR(100)
	)
	
	INSERT INTO	@OutputValues
	SELECT		CASE @Output_Format
					WHEN 1 THEN TDT.Short_Name
					WHEN 2 THEN TDT.Long_Name
					WHEN 3 THEN TDT.Kind
					WHEN 4 THEN -- Status abbreviation
							CASE -- if no status abbreviation then use short name, otherwise use status abbr.
								WHEN TDT.Status_Abbreviation IS NULL THEN TDT.Short_Name
								ELSE TDT.Status_Abbreviation
							END
					ELSE ''
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
	
	IF @Output_Format = 5
		SET @ReturnValue = CASE -- Were there any matching designations?
							WHEN EXISTS(SELECT 1 FROM @OutputValues) THEN 'Yes'
							ELSE 'No'
						   END
	ELSE
		SELECT	@ReturnValue	=
					-- Blank when this is the first value, otherwise
					-- the previous string plus the seperator
					CASE
						WHEN @ReturnValue IS NULL THEN ''
						ELSE @ReturnValue + @Seperator
					END + Item
		FROM		@OutputValues
		GROUP BY	Item
	
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
