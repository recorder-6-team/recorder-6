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
    $Revision: 2 $
    $Date: 5/04/06 14:12 $
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
				JOIN	#Species S ON S.Species_Name = ITN.Actual_Name 
				WHERE	TLV.Taxon_List_Key = @ChecklistKey
				AND	S.Import_Value = UpdatedSpecies.Import_Value
				)
	FROM	#Species UpdatedSpecies
	WHERE	Match_Key IS NULL

	UPDATE	UpdatedSpecies
	SET	Match_Count =  Match_Count + (
				SELECT	Count(*)
				FROM	Index_Taxon_Name ITN 
				JOIN	Taxon_List_Version TLV ON TLV.Taxon_List_Version_Key = ITN.Taxon_List_Version_Key
				JOIN	#Species S ON S.Species_Name = ITN.Actual_Name + ' ' + ITN.Authority
				WHERE	TLV.Taxon_List_Key = @ChecklistKey
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
	WHERE	Match_Count = 1
	AND	Match_Key IS NULL
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
	WHERE	Match_Count = 1
	AND	Match_Key IS NULL
	AND	TL.Taxon_List_Key = @ChecklistKey
	AND	Species_Name = Actual_Name + ' ' + ITN.Authority

	-- Update the Order field. Key value for rank 'Order' is 'NBNSYS..12'. 
	UPDATE	#Species
	SET	[Order] = ITN.Actual_Name
	FROM	Index_Taxon_Name ITN
	JOIN	Taxon_List_Item TLI 
		ON ITN.Taxon_List_Item_Key = TLI.Taxon_List_Item_Key
		AND TLI.Taxon_Rank_Key = 'NBNSYS0000000012'
	JOIN Index_Taxon_Group ITG ON ITG.Taxon_List_Item_Key=TLI.Taxon_List_Item_Key
	JOIN Index_Taxon_Synonym ITS ON ITS.Taxon_List_Item_Key=ITG.Contained_List_Item_Key
	WHERE Match_Count = 1
	AND	[Order] IS NULL
	AND	ITS.Synonym_List_Item_Key = Match_Key

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
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_SpecimenTypeTermList_Select') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_SpecimenTypeTermList_Select]
GO

/*===========================================================================*\
  Description:	Returns a list of Specimen Types

  Parameters:	<none>

  Created:	April 2006

  Last revision information:
    $Revision: 2 $
    $Date: 5/04/06 14:12 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_SpecimenTypeTermList_Select]
AS
	SELECT		Specimen_Type_Key AS Item_Key, Short_Name AS Item_Name
	FROM		Specimen_Type
	ORDER BY 	Short_Name
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_SpecimenTypeTermList_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_SpecimenTypeTermList_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_SpecimenTypeTermList_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_SpecimenTypeTermList_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_SpecimenTypeTermList_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_SpecimenTypeTermList_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_SpecimenTypeTermList_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_SpecimenTypeTermList_Select TO [Dev - JNCC SQL]
END
GO