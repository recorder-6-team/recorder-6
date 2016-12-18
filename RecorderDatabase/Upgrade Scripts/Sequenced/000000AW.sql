/****** Changes to allow Virtual Organism table to be used by Import Wizard  ******/

/****** Object:  StoredProcedure [dbo].[usp_IWMatch_Species]    Script Date: 12/17/2016 11:02:17 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

/*===========================================================================*\
  Description:	Set the match for the specified import record in the match table.

  Parameters:	@ChecklistKey

  Created:	June 2004

  Last revision information:
    17/12/2016 
    MikeWeideli
    changed to allow Virtual Organism table to be used by Import wizard  

\*===========================================================================*/


ALTER PROCEDURE [dbo].[usp_IWMatch_Species]
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
    --  remove the sub genus from import_value 
    UPDATE #Species Set Species_name = dbo.LCRemoveSubGenusText(Species_name)
    -- Match TV Key
       UPDATE	#Species
		SET	Match_Key = ITN2.Taxon_List_Item_Key,
			Match_Value =  TLI.Taxon_Version_Key,
			Checklist = TL.Item_Name,
			Checklist_Key = @ChecklistKey,
			Match_Count = 1
		FROM	
		Taxon_List_Item TLI INNER JOIN Index_Taxon_Name ITN ON ITN.Taxon_List_Item_Key = TLi.Taxon_List_Item_Key
		INNER JOIN Index_Taxon_Name ITN2 ON ITN.Recommended_Taxon_List_Item_Key = ITN2.Taxon_List_Item_Key
		JOIN	Taxon_List_Version TLV ON TLV.Taxon_List_Version_Key = ITN2.Taxon_List_Version_Key
		JOIN	Taxon_List TL ON TL.Taxon_List_Key = TLV.Taxon_List_Key
		WHERE	Species_Name = TLI.Taxon_Version_Key
  
 IF @ChecklistKey IS NULL
   BEGIN
   -- Handle searches against the virtual preferred list - no change here 
   UPDATE	UpdatedSpecies
      SET	Match_Count =  (SELECT	Count(DISTINCT ITN2.Taxon_List_Item_Key)
	  FROM	#Species S  
	  INNER JOIN Index_Taxon_Name ITN1 ON dbo.LCRemoveSubGenusText(ITN1.Actual_Name) = S.Species_Name
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
	  -- this join - it seems to be faster this way !
   UPDATE	UpdatedSpecies
      SET	Match_Count = Match_Count + (SELECT	Count(DISTINCT ITN2.Taxon_List_Item_Key)
	  FROM	#Species S  
      INNER JOIN Index_Taxon_Name ITN1 ON LEFT(S.Species_Name, LEN(dbo.LCRemoveSubGenusText(ITN1.Actual_Name))) = dbo.LCRemoveSubGenusText(ITN1.Actual_Name)
	  INNER JOIN Index_Taxon_Name ITN2 ON ITN2.Recommended_Taxon_List_Item_Key=ITN1.Recommended_Taxon_List_Item_Key
	  INNER JOIN Taxon_List_Version TLV ON TLV.Taxon_List_Version_Key = ITN2.Taxon_List_Version_Key
	  INNER JOIN Taxon_List_Item TLI ON TLI.Taxon_List_Item_Key=ITN2.Taxon_List_Item_Key
	  WHERE	TLI.Preferred_Name = TLI.Taxon_List_Item_Key 
	  AND S.Species_Name = dbo.LCRemoveSubGenusText(ITN1.Actual_Name) + ' ' + ITN1.Authority
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
	  Checklist_Key = NULL
	  FROM	Index_Taxon_Name ITN1
	  JOIN	Index_Taxon_Name ITN2 ON ITN2.Recommended_Taxon_List_Item_Key=ITN1.Recommended_Taxon_List_Item_Key
	  JOIN	Taxon_List_Version TLV ON TLV.Taxon_List_Version_Key = ITN2.Taxon_List_Version_Key
	  JOIN	Taxon_List TL ON TL.Taxon_List_Key = TLV.Taxon_List_Key
	  JOIN	Taxon_List_Item TLI ON TLI.Taxon_List_Item_Key=ITN2.Taxon_List_Item_Key
	  WHERE	Match_Count = 1
	  AND	Match_Key IS NULL
	  AND	TLI.Preferred_Name = TLI.Taxon_List_Item_Key 
	  AND ITN2.Preferred_List = 1
	  AND	Species_Name = dbo.LCRemoveSubGenusText(ITN1.Actual_Name)
	 AND TLI.Taxon_List_Version_To IS NULL

   UPDATE	#Species
     SET	Match_Key = ITN2.Taxon_List_Item_Key,
	 Match_Value = dbo.ufn_GetFormattedSpeciesName(ITN2.Taxon_List_Item_Key),
	Checklist = TL.Item_Name,
	Checklist_Key = NULL
	FROM	Index_Taxon_Name ITN1
	JOIN	Index_Taxon_Name ITN2 ON ITN2.Recommended_Taxon_List_Item_Key=ITN1.Recommended_Taxon_List_Item_Key
	JOIN	Taxon_List_Version TLV ON TLV.Taxon_List_Version_Key = ITN2.Taxon_List_Version_Key
	JOIN	Taxon_List TL ON TL.Taxon_List_Key = TLV.Taxon_List_Key
	JOIN	Taxon_List_Item TLI ON TLI.Taxon_List_Item_Key=ITN2.Taxon_List_Item_Key	
	WHERE	Match_Count = 1
	AND	Match_Key IS NULL
	AND	TLI.Preferred_Name = TLI.Taxon_List_Item_Key 
	AND ITN2.Preferred_List = 1
	AND	Species_Name = dbo.LCRemoveSubGenusText(ITN1.Actual_Name) + ' ' + ITN1.Authority
	AND TLI.Taxon_List_Version_To IS NULL

    END ELSE
	BEGIN
	  If LEFT(@checkListkey,7) = 'VIRTUAL'
	  -- Deal with Virtual Organism table 
	  BEGIN 
	    UPDATE	UpdatedSpecies
		SET	Match_Count =  (SELECT	Count(*)
		    FROM	#Species S 
			INNER JOIN Index_Taxon_Name ITN ON dbo.LCRemoveSubGenusText(ITN.Actual_Name) = S.Species_Name
			INNER JOIN VIRTUAL_ORGANISM TLI ON TLI.Taxon_List_Item_Key=ITN.Taxon_List_Item_Key
			WHERE	S.Import_Value = UpdatedSpecies.Import_Value)
		    FROM	#Species UpdatedSpecies
		    WHERE	Match_Key IS NULL

		UPDATE	UpdatedSpecies
		    SET	Match_Count = Match_Count + (SELECT	Count(*)
			FROM	#Species S 
			INNER JOIN Index_Taxon_Name ITN ON dbo.LCRemoveSubGenusText(ITN.Actual_Name)  + ' ' + ITN.Authority = S.Species_Name 
			INNER JOIN VIRTUAL_ORGANISM TLI ON TLI.Taxon_List_Item_Key=ITN.Taxon_List_Item_Key
			WHERE S.Import_Value = UpdatedSpecies.Import_Value)
			FROM	#Species UpdatedSpecies
		    WHERE	Match_Key IS NULL
		 
		-- Virtal Organism get values and keys for unique matches only. Broken down in two separate updates for speed.
		UPDATE	#Species
		    SET	Match_Key = ITN.Taxon_List_Item_Key,
		    Match_Value = dbo.ufn_GetFormattedSpeciesName(ITN.Taxon_List_Item_Key),
			Checklist = 'VIRTUAL_ORGANISM',
			Checklist_Key = 'VIRTUAL_ORGANISM'
		    FROM	Index_Taxon_Name ITN
		    INNER JOIN	VIRTUAL_ORGANISM TLI ON TLI.Taxon_List_Item_Key=ITN.Taxon_List_Item_Key
		    WHERE	Match_Count = 1
		    AND	Match_Key IS NULL
		    AND	Species_Name = dbo.LCRemoveSubGenusText(Actual_Name)
		
		UPDATE	#Species
		    SET	Match_Key = ITN.Taxon_List_Item_Key,
			Match_Value = dbo.ufn_GetFormattedSpeciesName(ITN.Taxon_List_Item_Key),
			Checklist = 'VIRTUAL_ORGANISM',
			Checklist_Key = 'VIRTUAL_ORGANISM'
		    FROM	Index_Taxon_Name ITN
		    INNER JOIN	VIRTUAL_ORGANISM TLI ON TLI.Taxon_List_Item_Key=ITN.Taxon_List_Item_Key	
		    WHERE	Match_Count = 1
		    AND	Match_Key IS NULL
		    AND	Species_Name = dbo.LCRemoveSubGenusText(Actual_Name) + ' ' + ITN.Authority
		
		
	  END ELSE
	  BEGIN
	  -- Handle searches against a normal specified list No change here.
      -- Set Match_Count first. Broken down in two separate updates for speed.
		UPDATE	UpdatedSpecies
		SET	Match_Count =  (SELECT	Count(*)
					FROM	#Species S 
					INNER JOIN Index_Taxon_Name ITN ON dbo.LCRemoveSubGenusText(ITN.Actual_Name) = S.Species_Name
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
					INNER JOIN Index_Taxon_Name ITN ON dbo.LCRemoveSubGenusText(ITN.Actual_Name)  + ' ' + ITN.Authority = S.Species_Name 
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
		AND	Species_Name = dbo.LCRemoveSubGenusText(Actual_Name)
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
		AND	Species_Name = dbo.LCRemoveSubGenusText(Actual_Name) + ' ' + ITN.Authority
		AND TLI.Taxon_List_Version_To IS NULL
	  END
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
