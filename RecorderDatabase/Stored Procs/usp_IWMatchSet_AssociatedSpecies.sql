/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatchSet_AssociatedSpecies') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_IWMatchSet_AssociatedSpecies]
GO

/*===========================================================================*\
  Description:	Set the match for the specified import record in the match table.

  Parameters:	@ImportValue
		@MatchKey

  Created:	June 2004

  Last revision information:
    $Revision: 4 $
    $Date: 23/07/04 14:37 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_IWMatchSet_AssociatedSpecies]
	@ImportValue varchar(100),
	@MatchKey char(16)
AS
	IF @MatchKey IS NOT NULL BEGIN
		DECLARE	@Order varchar(100), 
			@Checklist varchar(100),
			@ChecklistKey char(16)

		-- Key value for rank 'Order' is 'NBNSYS..12'. 
		SELECT	@Order = ITN.Actual_Name
		FROM	Index_Taxon_Group ITG
		JOIN	Index_Taxon_Name ITN ON ITN.Taxon_List_Item_Key = ITG.Taxon_List_Item_Key
		JOIN	Taxon_List_Item TLI ON TLI.Taxon_List_Item_Key = ITG.Taxon_List_Item_Key 
		WHERE	ITG.Contained_List_Item_Key = @MatchKey
		AND	TLI.Taxon_Rank_Key = 'NBNSYS0000000012' 
	
		-- 'Not available' value handled and regionalised in app.
		IF @Order IS NULL SET @Order = 'Not available'
	
		-- Get the associated checklist.
		SELECT	@ChecklistKey = TL.Taxon_List_Key, @Checklist = TL.Item_Name
		FROM	Index_Taxon_Name ITN
		JOIN	Taxon_List_Version TLV ON TLV.Taxon_List_Version_Key = ITN.Taxon_List_Version_Key
		JOIN	Taxon_List TL ON TL.Taxon_List_Key = TLV.Taxon_List_Key
		WHERE	ITN.Taxon_List_Item_Key = @MatchKey

		-- And update match table.
		UPDATE	#AssociatedSpecies
		SET	Match_Value = dbo.ufn_GetFormattedSpeciesName(@MatchKey),
			Match_Key = @MatchKey,
			Match_Count = 1,
			Manual_Match = 1,
			Remembered = 0,
			[Order] = @Order,
			Checklist = @Checklist,
			Checklist_Key = @ChecklistKey
		WHERE	Import_Value = @ImportValue
	END ELSE
		UPDATE	#AssociatedSpecies
		SET	Match_Value = NULL,
			Match_Key = NULL,
			Match_Count = NULL,
			Manual_Match = 0,
			Remembered = 0,
			[Order] = NULL,
			Checklist = NULL,
			Checklist_Key = NULL
		WHERE	Import_Value = @ImportValue

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatchSet_AssociatedSpecies') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_IWMatchSet_AssociatedSpecies'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_IWMatchSet_AssociatedSpecies TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_IWMatchSet_AssociatedSpecies TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_IWMatchSet_AssociatedSpecies TO [Dev - JNCC SQL]
END
GO