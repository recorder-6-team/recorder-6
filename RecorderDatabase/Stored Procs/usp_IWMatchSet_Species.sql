/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatchSet_Species') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_IWMatchSet_Species]
GO

/*===========================================================================*\
  Description:	Set the match for the specified import record in the match table.

  Parameters:	
		@ImportValue	The name of the species.
		@MatchKey		The key of the Taxon_List_Item that the species is
						being matched to.
		@UserID			The ID of the current user.

  Created:	June 2004

  Last revision information:
    $Revision: 8 $
    $Date: 12/01/09 9:49 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_IWMatchSet_Species]
	@ImportValue varchar(100),
	@MatchKey CHAR(16),
	@UserID CHAR(16),
	@ChecklistKey CHAR(16)
AS
	IF @MatchKey IS NOT NULL BEGIN
		DECLARE	@Order varchar(100), 
			@Checklist varchar(100),
			@MatchValue varchar(100)

		-- Key value for rank 'Order' is 'NBNSYS..12'. 
		SELECT	@Order = ITN.Actual_Name
		FROM	Index_Taxon_Synonym ITS 
		JOIN	Index_Taxon_Group ITG ON ITG.Contained_List_Item_Key = ITS.Synonym_List_Item_Key
		JOIN	Index_Taxon_Name ITN ON ITN.Taxon_List_Item_Key = ITG.Taxon_List_Item_Key
		JOIN	Taxon_List_Item TLI ON TLI.Taxon_List_Item_Key = ITG.Taxon_List_Item_Key 
		WHERE	ITS.Taxon_List_Item_Key = @MatchKey
		AND	TLI.Taxon_Rank_Key = 'NBNSYS0000000012' 
	
		-- 'Not available' value handled and regionalised in app.
		IF @Order IS NULL SET @Order = 'Not available'
	
		-- Get the associated checklist.
		SELECT	@Checklist = TL.Item_Name
		FROM	Index_Taxon_Name ITN
		JOIN	Taxon_List_Version TLV ON TLV.Taxon_List_Version_Key = ITN.Taxon_List_Version_Key
		JOIN	Taxon_List TL ON TL.Taxon_List_Key = TLV.Taxon_List_Key
		WHERE	ITN.Taxon_List_Item_Key = @MatchKey

		SET	@MatchValue =	dbo.ufn_GetFormattedSpeciesName(@MatchKey)

		-- And update match table.
		UPDATE	#Species
		SET	Match_Value = @MatchValue,
			Match_Key = @MatchKey,
			Match_Count = 1,
			Manual_Match = 1,
			Remembered = 0,
			[Order] = @Order,
			Checklist = @Checklist,
			Checklist_Key = @ChecklistKey
		WHERE	Import_Value = @ImportValue

		-- Remove any previous temporary matches this user made of this value.
		DELETE FROM IW_Matched_Species
		WHERE	Matched_Value		=	@ImportValue
			AND	Temp_User_ID		=	@UserID
		
		-- Inserts the temporary match.
		INSERT INTO IW_Matched_Species (
			Matched_Value,
			Matched_Key,
			Match_Checklist_Key,
			Temp_User_ID
		) VALUES (
			@ImportValue,
			@MatchKey,
			@ChecklistKey,
			@UserID
		)
	END ELSE BEGIN
		UPDATE	#Species
		SET	Match_Value = NULL,
			Match_Key = NULL,
			Match_Count = NULL,
			Manual_Match = 0,
			Remembered = 0,
			[Order] = NULL,
			Checklist = NULL,
			Checklist_Key = NULL
		WHERE	Import_Value = @ImportValue
		
		-- Remove any previous temporary matches this user made of this value.
		DELETE FROM IW_Matched_Species
		WHERE	Matched_Value		=	@ImportValue
			AND	Temp_User_ID		=	@UserID
	END
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatchSet_Species') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_IWMatchSet_Species'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_IWMatchSet_Species TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_IWMatchSet_Species TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_IWMatchSet_Species TO [Dev - JNCC SQL]
END
GO