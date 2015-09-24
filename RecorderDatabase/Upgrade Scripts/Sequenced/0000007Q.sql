SET QUOTED_IDENTIFIER ON
SET ANSI_NULLS ON
GO

/*============================================================================*\
CCN359 (VI 21085) -- Import Wizard - 'Abundance Data' truncation,
					 'Record Type' trunction, and matching associated species
\*============================================================================*/

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
    $Revision: 2 $
    $Date: 2/04/10 13:41 $
    $Author: Robertjohnson $

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

	IF @ChecklistKey IS NULL
	BEGIN
		-- Set Match_Count first.
		UPDATE	#AssociatedSpecies
		SET	Match_Count =  (SELECT	Count(DISTINCT ITN2.Taxon_List_Item_Key)
					FROM	Index_Taxon_Name ITN1
					JOIN	Index_Taxon_Name ITN2 ON ITN2.Recommended_Taxon_List_Item_Key=ITN1.Recommended_Taxon_List_Item_Key
					JOIN	Taxon_List_Version TLV ON TLV.Taxon_List_Version_Key = ITN2.Taxon_List_Version_Key
					JOIN	Taxon_List_Item TLI ON TLI.Taxon_List_Item_Key = ITN2.Taxon_List_Item_Key
					WHERE	ITN2.Preferred_List = 1
					AND		TLI.Taxon_List_Version_To IS NULL
					AND		(Species_Name = ITN2.Actual_Name
					OR		Species_Name = ITN2.Actual_Name + ' ' + ITN2.Authority))
		WHERE	Match_Key IS NULL

		-- Now get values and keys for unique matches only.
		UPDATE	#AssociatedSpecies
		SET	Match_Key = ITN1.Taxon_List_Item_Key,
			Match_Value = dbo.ufn_GetFormattedSpeciesName(ITN1.Taxon_List_Item_Key),
			Checklist = TL.Item_Name,
			Checklist_Key = TL.Taxon_List_Key
		FROM	Index_Taxon_Name ITN1
		JOIN	Index_Taxon_Name ITN2 ON ITN2.Recommended_Taxon_List_Item_Key=ITN1.Recommended_Taxon_List_Item_Key
		JOIN	Taxon_List_Version TLV ON TLV.Taxon_List_Version_Key = ITN2.Taxon_List_Version_Key
		JOIN	Taxon_List TL ON TL.Taxon_List_Key = TLV.Taxon_List_Key
		JOIN	Taxon_List_Item TLI ON TLI.Taxon_List_Item_Key = ITN2.Taxon_List_Item_Key
		WHERE	Match_Count = 1
		AND		Match_Key IS NULL
		AND		TLI.Taxon_List_Version_To IS NULL
		AND		ITN2.Preferred_List = 1
		AND		(Species_Name = ITN2.Actual_Name
		OR		Species_Name = ITN2.Actual_Name + ' ' + ITN2.Authority)
	END ELSE
	BEGIN
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
	END

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
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatchNewEntry_AbundanceQualifier') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_IWMatchNewEntry_AbundanceQualifier]
GO

/*===========================================================================*\
  Description:	Create a new item in a term list from an import value.

  Parameters:	
	@ImportValue	The name of the new item.
	@EnteredBy

  Created:	June 2004

  Last revision information:
    $Revision: 2 $
    $Date: 2/04/10 13:41 $
    $Author: Robertjohnson $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_IWMatchNewEntry_AbundanceQualifier]
	@ImportValue varchar(100),
	@EnteredBy char(16)
AS
	DECLARE @Key char(16)
	EXECUTE spNextKey 'Measurement_Qualifier', @Key OUTPUT

	BEGIN TRANSACTION
		INSERT INTO Measurement_Qualifier(
			Measurement_Qualifier_Key, Short_Name, Long_Name, Measurement_Type_Key, Entered_By
		) VALUES (
			@Key, CASE WHEN LEN(@ImportValue)>40 THEN LEFT(@ImportValue, 37) + '...' ELSE @ImportValue END, 
			@ImportValue, 'NBNSYS0000000004', @EnteredBy
		)

		IF @@Error <> 0 GOTO RollbackAndExit

		-- And update import table with new data.
		UPDATE	#AbundanceQualifiers
		SET	Match_Value = Import_Value,
			Match_Key = @Key,
			Match_Count = 1,
			Manual_Match = 1,
			Remembered = 0
		WHERE	Import_Value = @ImportValue
	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatchNewEntry_AbundanceQualifier') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_IWMatchNewEntry_AbundanceQualifier'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_IWMatchNewEntry_AbundanceQualifier TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_IWMatchNewEntry_AbundanceQualifier TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_IWMatchNewEntry_AbundanceQualifier TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatchNewEntry_RecordType') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_IWMatchNewEntry_RecordType]
GO

/*===========================================================================*\
  Description:	Create a new item in a term list from an import value.

  Parameters:	
	@ImportValue	The name of the new item.
	@EnteredBy

  Created:	June 2004

  Last revision information:
    $Revision: 2 $
    $Date: 2/04/10 13:41 $
    $Author: Robertjohnson $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_IWMatchNewEntry_RecordType]
	@ImportValue varchar(100),
	@EnteredBy char(16)
AS
	DECLARE @Key char(16)
	EXECUTE spNextKey 'Record_Type', @Key OUTPUT

	BEGIN TRANSACTION
		INSERT INTO Record_Type(
			Record_Type_Key, Short_Name, Long_Name, Entered_By
		) VALUES (
			@Key, 
			CASE WHEN LEN(@ImportValue)>40 THEN LEFT(@ImportValue, 37) + '...' ELSE @ImportValue END, 
			@ImportValue, 
			@EnteredBy
		)

		IF @@Error <> 0 GOTO RollbackAndExit

		-- And update import table with new data.
		UPDATE	#RecordTypes
		SET	Match_Value = Import_Value,
			Match_Key = @Key,
			Match_Count = 1,
			Manual_Match = 1,
			Remembered = 0
		WHERE	Import_Value = @ImportValue
	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatchNewEntry_RecordType') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_IWMatchNewEntry_RecordType'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_IWMatchNewEntry_RecordType TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_IWMatchNewEntry_RecordType TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_IWMatchNewEntry_RecordType TO [Dev - JNCC SQL]
END
GO

UPDATE	dbo.IW_Column_Type
SET		Maximum_Length		= 100
WHERE	IW_Column_Type_Key	= 'SYSTEM010000000I'
GO