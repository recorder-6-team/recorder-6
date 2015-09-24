SET QUOTED_IDENTIFIER ON
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatch_AssociatedSpecies') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_IWMatch_AssociatedSpecies]
GO

/*===========================================================================*\
  Description:	Set the match for the specified import record in the match table.

  Parameters:	@ChecklistKey

  Created:	June 2004

  Last revision information:
    $Revision: 1 $
    $Date: 31/08/04 14:23 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_IWMatch_AssociatedSpecies]
	@ChecklistKey char(16)
AS
	-- Setup new column with reworked Import_Value, hopefully that will speed things up a bit.
	UPDATE	#AssociatedSpecies
	SET	Species_Name = 
		CASE	WHEN Right(Import_Value, 2) = 'sp' THEN Left(Import_Value, Len(Import_Value) - 2)
			WHEN Right(Import_Value, 3) = 'sp.' THEN Left(Import_Value, Len(Import_Value) - 3)
			WHEN Right(Import_Value, 3) = 'spp' THEN Left(Import_Value, Len(Import_Value) - 3)
			WHEN Right(Import_Value, 4) = 'spp.' THEN Left(Import_Value, Len(Import_Value) - 4)
			ELSE Import_Value
		END

	-- Set Match_Count first.
	UPDATE	#AssociatedSpecies
	SET	Match_Count =  (SELECT	Count(*)
				FROM	Index_Taxon_Name ITN
				JOIN	Taxon_List_Version TLV ON TLV.Taxon_List_Version_Key = ITN.Taxon_List_Version_Key
				JOIN	Taxon_List_Item TLI ON TLI.Taxon_List_Item_Key = ITN.Taxon_List_Item_Key
				WHERE	TLV.Taxon_List_Key = @ChecklistKey
				AND	TLI.Taxon_List_Version_To IS NULL
				AND	(Species_Name = ITN.Actual_Name
				OR	 Species_Name = ITN.Actual_Name + ' ' + ITN.Authority))
	WHERE	Match_Key IS NULL

	-- Now get values and keys for unique matches only.
	UPDATE	#AssociatedSpecies
	SET	Match_Key = ITN.Taxon_List_Item_Key,
		Match_Value = dbo.ufn_GetFormattedSpeciesName(ITN.Taxon_List_Item_Key),
		Checklist = TL.Item_Name,
		Checklist_Key = TL.Taxon_List_Key
	FROM	Index_Taxon_Name ITN
	JOIN	Taxon_List_Version TLV ON TLV.Taxon_List_Version_Key = ITN.Taxon_List_Version_Key
	JOIN	Taxon_List TL ON TL.Taxon_List_Key = TLV.Taxon_List_Key
	JOIN	Taxon_List_Item TLI ON TLI.Taxon_List_Item_Key = ITN.Taxon_List_Item_Key
	WHERE	Match_Count = 1
	AND	Match_Key IS NULL
	AND	TLI.Taxon_List_Version_To IS NULL
	AND	TL.Taxon_List_Key = @ChecklistKey
	AND	(Species_Name = Actual_Name
	OR	 Species_Name = Actual_Name + ' ' + ITN.Authority)

	-- Update the Order field. Key value for rank 'Order' is 'NBNSYS..12'. 
	UPDATE	#AssociatedSpecies
	SET	[Order] = ITN.Actual_Name
	FROM	Index_Taxon_Synonym ITS
	JOIN	Index_Taxon_Group ITG ON ITG.Contained_List_Item_Key = ITS.Synonym_List_Item_Key
	JOIN	Index_Taxon_Name ITN ON ITN.Taxon_List_Item_Key = ITG.Taxon_List_Item_Key
	JOIN	Taxon_List_Item TLI ON TLI.Taxon_List_Item_Key = ITG.Taxon_List_Item_Key 
	WHERE	Match_Count = 1
	AND	[Order] IS NULL
	AND	ITS.Taxon_List_Item_Key = Match_Key
	AND	TLI.Taxon_Rank_Key = 'NBNSYS0000000012' 

	-- Order field can still be null, so deal with that. 'Not available' value handled and regionalised in app.
	UPDATE	#AssociatedSpecies
	SET	[Order] = 'Not available'
	WHERE	Match_Count = 1
	AND	[Order] IS NULL
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatch_AssociatedSpecies') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_IWMatch_AssociatedSpecies'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_IWMatch_AssociatedSpecies TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_IWMatch_AssociatedSpecies TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_IWMatch_AssociatedSpecies TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatch_Species') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_IWMatch_Species]
GO

/*===========================================================================*\
  Description:	Set the match for the specified import record in the match table.

  Parameters:	@ChecklistKey

  Created:	June 2004

  Last revision information:
    $Revision: 1 $
    $Date: 31/08/04 14:23 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_IWMatch_Species]
	@ChecklistKey char(16)
AS
	-- Setup new column with reworked Import_Value, hopefully that will speed things up a bit.
	UPDATE	#Species
	SET	Species_Name = 
		CASE	WHEN Right(Import_Value, 2) = 'sp' THEN Left(Import_Value, Len(Import_Value) - 2)
			WHEN Right(Import_Value, 3) = 'sp.' THEN Left(Import_Value, Len(Import_Value) - 3)
			WHEN Right(Import_Value, 3) = 'spp' THEN Left(Import_Value, Len(Import_Value) - 3)
			WHEN Right(Import_Value, 4) = 'spp.' THEN Left(Import_Value, Len(Import_Value) - 4)
			ELSE Import_Value
		END

	-- Set Match_Count first. Broken down in two separate updates for speed.
	UPDATE	UpdatedSpecies
	SET	Match_Count =  (SELECT	Count(*)
				FROM	Index_Taxon_Name ITN 
				JOIN	Taxon_List_Version TLV ON TLV.Taxon_List_Version_Key = ITN.Taxon_List_Version_Key
				JOIN	Taxon_List_Item TLI ON TLI.Taxon_List_Item_Key = ITN.Taxon_List_Item_Key
				JOIN	#Species S ON S.Species_Name = ITN.Actual_Name 
				WHERE	TLV.Taxon_List_Key = @ChecklistKey
				AND	TLI.Taxon_List_Version_To IS NULL
				AND	S.Import_Value = UpdatedSpecies.Import_Value
				)
	FROM	#Species UpdatedSpecies
	WHERE	Match_Key IS NULL

	UPDATE	UpdatedSpecies
	SET	Match_Count =  Match_Count + (
				SELECT	Count(*)
				FROM	Index_Taxon_Name ITN 
				JOIN	Taxon_List_Version TLV ON TLV.Taxon_List_Version_Key = ITN.Taxon_List_Version_Key
				JOIN	Taxon_List_Item TLI ON TLI.Taxon_List_Item_Key = ITN.Taxon_List_Item_Key
				JOIN	#Species S ON S.Species_Name = ITN.Actual_Name + ' ' + ITN.Authority
				WHERE	TLV.Taxon_List_Key = @ChecklistKey
				AND	TLI.Taxon_List_Version_To IS NULL
				AND	S.Import_Value = UpdatedSpecies.Import_Value
				)
	FROM	#Species UpdatedSpecies
	WHERE	Match_Key IS NULL


	-- Now get values and keys for unique matches only. Broken down in tow separate updates for speed.
	UPDATE	#Species
	SET	Match_Key = ITN.Taxon_List_Item_Key,
		Match_Value = dbo.ufn_GetFormattedSpeciesName(ITN.Taxon_List_Item_Key),
		Checklist = TL.Item_Name,
		Checklist_Key = TL.Taxon_List_Key
	FROM	Index_Taxon_Name ITN
	JOIN	Taxon_List_Version TLV ON TLV.Taxon_List_Version_Key = ITN.Taxon_List_Version_Key
	JOIN	Taxon_List TL ON TL.Taxon_List_Key = TLV.Taxon_List_Key
	JOIN	Taxon_List_Item TLI ON TLI.Taxon_List_Item_Key = ITN.Taxon_List_Item_Key
	WHERE	Match_Count = 1
	AND	Match_Key IS NULL
	AND	TLI.Taxon_List_Version_To IS NULL
	AND	TL.Taxon_List_Key = @ChecklistKey
	AND	Species_Name = Actual_Name

	UPDATE	#Species
	SET	Match_Key = ITN.Taxon_List_Item_Key,
		Match_Value = dbo.ufn_GetFormattedSpeciesName(ITN.Taxon_List_Item_Key),
		Checklist = TL.Item_Name,
		Checklist_Key = TL.Taxon_List_Key
	FROM	Index_Taxon_Name ITN
	JOIN	Taxon_List_Version TLV ON TLV.Taxon_List_Version_Key = ITN.Taxon_List_Version_Key
	JOIN	Taxon_List TL ON TL.Taxon_List_Key = TLV.Taxon_List_Key
	JOIN	Taxon_List_Item TLI ON TLI.Taxon_List_Item_Key = ITN.Taxon_List_Item_Key
	WHERE	Match_Count = 1
	AND	Match_Key IS NULL
	AND	TLI.Taxon_List_Version_To IS NULL
	AND	TL.Taxon_List_Key = @ChecklistKey
	AND	Species_Name = Actual_Name + ' ' + ITN.Authority

	-- Update the Order field. Key value for rank 'Order' is 'NBNSYS..12'. 
	UPDATE	#Species
	SET	[Order] = ITN.Actual_Name
	FROM	Index_Taxon_Synonym ITS
	JOIN	Index_Taxon_Group ITG ON ITG.Contained_List_Item_Key = ITS.Synonym_List_Item_Key
	JOIN	Index_Taxon_Name ITN ON ITN.Taxon_List_Item_Key = ITG.Taxon_List_Item_Key
	JOIN	Taxon_List_Item TLI ON TLI.Taxon_List_Item_Key = ITG.Taxon_List_Item_Key 
	WHERE	Match_Count = 1
	AND	[Order] IS NULL
	AND	ITS.Taxon_List_Item_Key = Match_Key
	AND	TLI.Taxon_Rank_Key = 'NBNSYS0000000012' 

	-- Order field can still be null, so deal with that. 'Not available' value handled and regionalised in app.
	UPDATE	#Species
	SET	[Order] = 'Not available'
	WHERE	Match_Count = 1
	AND	[Order] IS NULL
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatch_Species') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_IWMatch_Species'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_IWMatch_Species TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_IWMatch_Species TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_IWMatch_Species TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Species_Select_ForSearch') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_Species_Select_ForSearch]
GO

/*===========================================================================*\
  Description:	Returns a list of taxa matching a search string.

  Parameters:	@SearchKey
		@SearchText

  Created:	June 2004

  Last revision information:
    $Revision: 1 $
    $Date: 31/08/04 14:23 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Species_Select_ForSearch]
	@SearchKey char(16),
	@SearchText varchar(100)
AS
	IF @SearchKey IS NULL
		SELECT	ITN.Taxon_List_Item_Key AS Item_Key,
			CASE Actual_Name_Italic
				WHEN 1 THEN '<i>' + Actual_Name + '</i>'
				ELSE Actual_Name
			END + ISNULL(' ' + ITN.Authority, '') + ISNULL(' - ' + TL.Item_Name, '') AS DisplayTerm, 
			Actual_Name + ISNULL(' ' + ITN.Authority, '') + ISNULL(' - ' + TL.Item_Name, '') AS SearchTerm

		FROM	Index_Taxon_Name ITN
		JOIN 	Taxon_List_Version TLV ON TLV.Taxon_List_Version_Key = ITN.Taxon_List_Version_Key
		JOIN 	Taxon_List TL ON TL.Taxon_List_Key = TLV.Taxon_List_Key
		JOIN	Taxon_List_Item TLI ON TLI.Taxon_List_Item_Key = ITN.Taxon_List_Item_Key

		WHERE 	(Actual_Name LIKE @SearchText + '%'
		OR	Actual_Name + ' ' + ITN.Authority LIKE @SearchText + '%'
		OR 	ITN.Authority LIKE @SearchText + '%')
		AND	TLI.Taxon_List_Version_To IS NULL
		ORDER BY SearchTerm
	ELSE
		SELECT	ITN.Taxon_List_Item_Key AS Item_Key,
			CASE Actual_Name_Italic
				WHEN 1 THEN '<i>' + Actual_Name + '</i>'
				ELSE Actual_Name
			END + ISNULL(' ' + ITN.Authority, '') AS DisplayTerm, 
			Actual_Name + ISNULL(' ' + ITN.Authority, '') AS SearchTerm
	
		FROM	Index_Taxon_Name ITN
		JOIN 	Taxon_List_Version TLV ON TLV.Taxon_List_Version_Key = ITN.Taxon_List_Version_Key AND TLV.Taxon_List_Key = @SearchKey
		JOIN	Taxon_List_Item TLI ON TLI.Taxon_List_Item_Key = ITN.Taxon_List_Item_Key

		WHERE 	(Actual_Name LIKE @SearchText + '%'
		OR	Actual_Name + ' ' + ITN.Authority LIKE @SearchText + '%'
		OR 	ITN.Authority LIKE @SearchText + '%')
		AND	TLI.Taxon_List_Version_To IS NULL
		ORDER BY SearchTerm
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Species_Select_ForSearch') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Species_Select_ForSearch'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Species_Select_ForSearch TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Species_Select_ForSearch TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Species_Select_ForSearch TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Species_Select_ForSearch TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Species_Select_ForSearch TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Species_Select_ForSearch TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Species_Select_ForSearchByList') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_Species_Select_ForSearchByList]
GO

/*===========================================================================*\
  Description:	Returns a list of taxa matching a search string.

  Parameters:	@SearchText

  Created:	June 2004

  Last revision information:
    $Revision: 1 $
    $Date: 31/08/04 14:23 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Species_Select_ForSearchByList]
	@SearchKey char(16),
	@SearchText varchar(100)

AS
	SELECT	ITN.Taxon_List_Item_Key AS Item_Key,
		CASE Actual_Name_Italic
			WHEN 1 THEN '<i>' + Actual_Name + '</i>'
			ELSE Actual_Name
		END + ' ' + ISNULL(ITN.Authority, '') AS DisplayTerm, 
		Actual_Name + ' ' + ISNULL(ITN.Authority, '') AS SearchTerm
	FROM	Index_Taxon_Name ITN
	JOIN 	Taxon_List_Version TLV ON TLV.Taxon_List_Version_Key = ITN.Taxon_List_Version_Key AND TLV.Taxon_List_Key = @SearchKey
	JOIN	Taxon_List_Item TLI ON TLI.Taxon_List_Item_Key = ITN.Taxon_List_Item_Key
	WHERE 	(Actual_Name LIKE @SearchText + '%'
	OR	Actual_Name + ' ' + ITN.Authority LIKE @SearchText + '%'
	OR 	ITN.Authority LIKE @SearchText + '%')
	AND	TLI.Taxon_List_Version_To IS NULL
	ORDER BY SearchTerm

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Species_Select_ForSearchByList') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Species_Select_ForSearchByList'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Species_Select_ForSearchByList TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Species_Select_ForSearchByList TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Species_Select_ForSearchByList TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Species_Select_ForSearchByList TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Species_Select_ForSearchByList TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Species_Select_ForSearchByList TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_SpecimenTypes_Select]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_SpecimenTypes_Select]
GO

GO
/*===========================================================================*\
  Description:	Returns the Specimen Types for the SpecimenGeneral frame.

  Parameters:	@Mask	The Specimen Unit Mask value

  Created:	September 2003

  Last revision information:
    $Revision: 1 $
    $Date: 31/08/04 14:23 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_SpecimenTypes_Select]
	@Mask int
AS

-- Set of options for better use of vw_ConceptTerm.
SET ANSI_NULLS ON
SET ANSI_PADDING ON
SET ANSI_WARNINGS ON
SET ARITHABORT ON
SET CONCAT_NULL_YIELDS_NULL ON
SET QUOTED_IDENTIFIER ON
SET NO_BROWSETABLE OFF
SET NOCOUNT ON

DECLARE @LocalKey CHAR(16)
SELECT @LocalKey=CG.Concept_Group_Key
FROM concept_group cg 
LEFT JOIN local_domain ld ON ld.local_domain_key = cg.local_domain_Key
LEFT JOIN domain d ON d.domain_key = ld.domain_key
WHERE D.Domain_Mask & @Mask > 0
	AND CG.Item_Name = 'Specimen Type'

SELECT CT.concept_key, CT.plaintext
	FROM VW_ConceptTerm CT 
WHERE CT.concept_group_key IN ('SYSTEM000000000J', @LocalKey)
ORDER BY CT.Plaintext

SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_SpecimenTypes_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_SpecimenTypes_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_SpecimenTypes_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_SpecimenTypes_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_SpecimenTypes_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_SpecimenTypes_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_SpecimenTypes_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_SpecimenTypes_Select TO [Dev - JNCC SQL]
END

GO

