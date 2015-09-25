SET QUOTED_IDENTIFIER ON
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
    $Date: 16/05/08 10:57 $
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

	IF @ChecklistKey IS NULL
	BEGIN
		-- Handle searches against the virtual preferred list

		-- Set Match_Count first. Broken down in two separate updates for speed.
		UPDATE	UpdatedSpecies
		SET	Match_Count =  (SELECT	Count(DISTINCT ITN2.Taxon_List_Item_Key)
					FROM	#Species S  
					INNER JOIN Index_Taxon_Name ITN1 ON ITN1.Actual_Name = S.Species_Name
					INNER JOIN Index_Taxon_Name ITN2 ON ITN2.Recommended_Taxon_List_Item_Key=ITN1.Recommended_Taxon_List_Item_Key
					INNER JOIN Taxon_List_Version TLV ON TLV.Taxon_List_Version_Key = ITN2.Taxon_List_Version_Key
					INNER JOIN Taxon_List_Item TLI ON TLI.Taxon_List_Item_Key=ITN2.Taxon_List_Item_Key
					WHERE	TLI.Preferred_Name = TLI.Taxon_List_Item_Key 
					AND ITN2.Preferred_List = 1
					AND	S.Import_Value = UpdatedSpecies.Import_Value
					AND TLI.Taxon_List_Version_To IS NULL
					AND TLV.Version >= (SELECT MAX(Version)
									FROM Taxon_List_Version 
          							WHERE Taxon_List_Key = TLV.Taxon_List_Key
									AND Version_Is_Amendment = 0))
		FROM	#Species UpdatedSpecies
		WHERE	Match_Key IS NULL

		-- Set Match_Count for species name + authority test. Note the strange way of doing
		-- this join - it seems to be faster this way!
		UPDATE	UpdatedSpecies
		SET	Match_Count = Match_Count + (SELECT	Count(DISTINCT ITN2.Taxon_List_Item_Key)
					FROM	#Species S  
					INNER JOIN Index_Taxon_Name ITN1 ON LEFT(S.Species_Name, LEN(ITN1.Actual_Name)) = ITN1.Actual_Name
					INNER JOIN Index_Taxon_Name ITN2 ON ITN2.Recommended_Taxon_List_Item_Key=ITN1.Recommended_Taxon_List_Item_Key
					INNER JOIN Taxon_List_Version TLV ON TLV.Taxon_List_Version_Key = ITN2.Taxon_List_Version_Key
					INNER JOIN Taxon_List_Item TLI ON TLI.Taxon_List_Item_Key=ITN2.Taxon_List_Item_Key
					WHERE	TLI.Preferred_Name = TLI.Taxon_List_Item_Key 
					AND S.Species_Name = ITN1.Actual_Name + ' ' + ITN1.Authority
					AND ITN2.Preferred_List = 1
					AND	S.Import_Value = UpdatedSpecies.Import_Value
					AND TLI.Taxon_List_Version_To IS NULL
					AND TLV.Version >= (SELECT MAX(Version)
									FROM Taxon_List_Version 
          							WHERE Taxon_List_Key = TLV.Taxon_List_Key
									AND Version_Is_Amendment = 0))
		FROM	#Species UpdatedSpecies
		WHERE	Match_Key IS NULL


		-- Now get values and keys for unique matches only. Broken down in two separate updates for speed.
		UPDATE	#Species
		SET	Match_Key = ITN2.Taxon_List_Item_Key,
			Match_Value = dbo.ufn_GetFormattedSpeciesName(ITN2.Taxon_List_Item_Key),
			Checklist = TL.Item_Name,
			Checklist_Key = TL.Taxon_List_Key
		FROM	Index_Taxon_Name ITN1
		JOIN	Index_Taxon_Name ITN2 ON ITN2.Recommended_Taxon_List_Item_Key=ITN1.Recommended_Taxon_List_Item_Key
		JOIN	Taxon_List_Version TLV ON TLV.Taxon_List_Version_Key = ITN2.Taxon_List_Version_Key
		JOIN	Taxon_List TL ON TL.Taxon_List_Key = TLV.Taxon_List_Key
		JOIN	Taxon_List_Item TLI ON TLI.Taxon_List_Item_Key=ITN2.Taxon_List_Item_Key
		WHERE	Match_Count = 1
		AND	Match_Key IS NULL
		AND	TLI.Preferred_Name = TLI.Taxon_List_Item_Key 
		AND ITN2.Preferred_List = 1
		AND	Species_Name = ITN1.Actual_Name
		AND TLI.Taxon_List_Version_To IS NULL

		UPDATE	#Species
		SET	Match_Key = ITN2.Taxon_List_Item_Key,
			Match_Value = dbo.ufn_GetFormattedSpeciesName(ITN2.Taxon_List_Item_Key),
			Checklist = TL.Item_Name,
			Checklist_Key = TL.Taxon_List_Key
		FROM	Index_Taxon_Name ITN1
		JOIN	Index_Taxon_Name ITN2 ON ITN2.Recommended_Taxon_List_Item_Key=ITN1.Recommended_Taxon_List_Item_Key
		JOIN	Taxon_List_Version TLV ON TLV.Taxon_List_Version_Key = ITN2.Taxon_List_Version_Key
		JOIN	Taxon_List TL ON TL.Taxon_List_Key = TLV.Taxon_List_Key
		JOIN	Taxon_List_Item TLI ON TLI.Taxon_List_Item_Key=ITN2.Taxon_List_Item_Key	
		WHERE	Match_Count = 1
		AND	Match_Key IS NULL
		AND	TLI.Preferred_Name = TLI.Taxon_List_Item_Key 
		AND ITN2.Preferred_List = 1
		AND	Species_Name = ITN1.Actual_Name + ' ' + ITN1.Authority
		AND TLI.Taxon_List_Version_To IS NULL

	END ELSE
	BEGIN
		-- Handle searches against a specified list

		-- Set Match_Count first. Broken down in two separate updates for speed.
		UPDATE	UpdatedSpecies
		SET	Match_Count =  (SELECT	Count(*)
					FROM	#Species S 
					INNER JOIN Index_Taxon_Name ITN ON ITN.Actual_Name = S.Species_Name 
					INNER JOIN Taxon_List_Version TLV ON TLV.Taxon_List_Version_Key = ITN.Taxon_List_Version_Key					
					INNER JOIN Taxon_List_Item TLI ON TLI.Taxon_List_Item_Key=ITN.Taxon_List_Item_Key
					WHERE	TLV.Taxon_List_Key = @ChecklistKey
					AND	S.Import_Value = UpdatedSpecies.Import_Value
					AND TLI.Taxon_List_Version_To IS NULL
					AND TLV.Version >= (SELECT MAX(Version)
									FROM Taxon_List_Version 
          							WHERE Taxon_List_Key = TLV.Taxon_List_Key
									AND Version_Is_Amendment = 0))
		FROM	#Species UpdatedSpecies
		WHERE	Match_Key IS NULL

		UPDATE	UpdatedSpecies
		SET	Match_Count = Match_Count + (SELECT	Count(*)
					FROM	#Species S 
					INNER JOIN Index_Taxon_Name ITN ON ITN.Actual_Name + ' ' + ITN.Authority = S.Species_Name 
					INNER JOIN Taxon_List_Version TLV ON TLV.Taxon_List_Version_Key = ITN.Taxon_List_Version_Key					
					INNER JOIN Taxon_List_Item TLI ON TLI.Taxon_List_Item_Key=ITN.Taxon_List_Item_Key
					WHERE	TLV.Taxon_List_Key = @ChecklistKey
					AND	S.Import_Value = UpdatedSpecies.Import_Value
					AND TLI.Taxon_List_Version_To IS NULL
					AND TLV.Version >= (SELECT MAX(Version)
									FROM Taxon_List_Version 
          							WHERE Taxon_List_Key = TLV.Taxon_List_Key
									AND Version_Is_Amendment = 0))
		FROM	#Species UpdatedSpecies
		WHERE	Match_Key IS NULL


		-- Now get values and keys for unique matches only. Broken down in two separate updates for speed.
		UPDATE	#Species
		SET	Match_Key = ITN.Taxon_List_Item_Key,
			Match_Value = dbo.ufn_GetFormattedSpeciesName(ITN.Taxon_List_Item_Key),
			Checklist = TL.Item_Name,
			Checklist_Key = TL.Taxon_List_Key
		FROM	Index_Taxon_Name ITN
		JOIN	Taxon_List_Version TLV ON TLV.Taxon_List_Version_Key = ITN.Taxon_List_Version_Key
		JOIN	Taxon_List TL ON TL.Taxon_List_Key = TLV.Taxon_List_Key
		JOIN	Taxon_List_Item TLI ON TLI.Taxon_List_Item_Key=ITN.Taxon_List_Item_Key
		WHERE	Match_Count = 1
		AND	Match_Key IS NULL
		AND	TL.Taxon_List_Key = @ChecklistKey
		AND	Species_Name = Actual_Name
		AND TLI.Taxon_List_Version_To IS NULL

		UPDATE	#Species
		SET	Match_Key = ITN.Taxon_List_Item_Key,
			Match_Value = dbo.ufn_GetFormattedSpeciesName(ITN.Taxon_List_Item_Key),
			Checklist = TL.Item_Name,
			Checklist_Key = TL.Taxon_List_Key
		FROM	Index_Taxon_Name ITN
		JOIN	Taxon_List_Version TLV ON TLV.Taxon_List_Version_Key = ITN.Taxon_List_Version_Key
		JOIN	Taxon_List TL ON TL.Taxon_List_Key = TLV.Taxon_List_Key
		JOIN	Taxon_List_Item TLI ON TLI.Taxon_List_Item_Key=ITN.Taxon_List_Item_Key	
		WHERE	Match_Count = 1
		AND	Match_Key IS NULL
		AND	TL.Taxon_List_Key = @ChecklistKey
		AND	Species_Name = Actual_Name + ' ' + ITN.Authority
		AND TLI.Taxon_List_Version_To IS NULL
	END

	-- Update the Order field. Key value for rank 'Order' is 'NBNSYS..12'. 
	UPDATE	S
	SET	[Order] = ITN.Actual_Name
	FROM	#Species	S
	JOIN	Index_Taxon_Synonym ITS ON ITS.Synonym_List_Item_Key = S.Match_Key
	JOIN	Index_Taxon_Group ITG ON ITS.Taxon_List_Item_Key = ITG.Contained_List_Item_Key
	JOIN	Taxon_List_Item TLI 
		ON	ITG.Taxon_List_Item_Key = TLI.Taxon_List_Item_Key
		AND TLI.Taxon_Rank_Key = 'NBNSYS0000000012'
	JOIN	Index_Taxon_Name ITN
		ON	ITN.Taxon_List_Item_Key = TLI.Taxon_List_Item_Key
	WHERE Match_Count = 1
	AND	[Order] IS NULL

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
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Rucksack_LoadTaxa]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Rucksack_LoadTaxa]
GO

/*===========================================================================*\
  Description:	Uses a pre-existing temp table (#TempList) to obtain a list
		of Taxa name recursively.

  Parameters:	@ItemKey
		@Output

  Created:	May 2008

  Last revision information:
    $Revision: 1 $
    $Date: 16/05/08 10:57 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Rucksack_LoadTaxa]
AS
  SET NOCOUNT ON

  IF object_id('tempdb..#TempList') IS NULL
	Raiserror('[usp_Rucksack_LoadTaxa]requires #TempList temp table to be created first.', 16, 1)

	SELECT Taxon_List_Item_Key as ItemKey, Actual_Name,Common_Name, Preferred_Name,Preferred_Name_Italic,Common_Name_Italic
	FROM Index_Taxon_Name ITG
	INNER JOIN #TempList T ON T.RecordKey=ITG.Taxon_List_Item_Key
    ORDER BY T.SortOrder

GO
/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Rucksack_LoadTaxa') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Rucksack_LoadTaxa'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Rucksack_LoadTaxa TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Rucksack_LoadTaxa TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Rucksack_LoadTaxa TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Rucksack_LoadTaxa TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Rucksack_LoadTaxa TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Rucksack_LoadTaxa TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_SurveyEvent_Delete]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_SurveyEvent_Delete]
GO

/*===========================================================================*\
  Description:	Delete a record in the Survey_Event table with related tables.

  Parameters:	@Key

  Created:	May 2008

  Last revision information:
    $Revision: 1 $
    $Date: 16/05/08 10:57 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_SurveyEvent_Delete]
	@Key char(16)
AS
	SET NOCOUNT ON

    DELETE  Survey_Event_Recorder Where Survey_Event_Key = @Key
	DELETE	Survey_Event_Owner WHERE	Survey_Event_Key = @Key
    Delete  Survey_Event where Survey_Event_Key = @Key
GO
/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_SurveyEvent_Delete') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure [usp_SurveyEvent_Delete]'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.[usp_SurveyEvent_Delete] TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.[usp_SurveyEvent_Delete]TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.[usp_SurveyEvent_Delete] TO [Dev - JNCC SQL]
END
GO

