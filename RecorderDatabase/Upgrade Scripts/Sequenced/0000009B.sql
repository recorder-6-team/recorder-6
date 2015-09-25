/****** Metadata: Fix lower case in grid refs  ******/

Update Sample set Spatial_ref = UPPER(Spatial_ref)
WHERE SPATIAL_REF_SYSTEM IN ('OSGB','OSNI')

Update Survey_event  set Spatial_ref = UPPER(Spatial_ref)
WHERE SPATIAL_REF_SYSTEM IN ('OSGB','OSNI')

Update Location set Spatial_ref = UPPER(Spatial_ref)
WHERE SPATIAL_REF_SYSTEM IN ('OSGB','OSNI')
 
GO
/****** Metadata: Add names to Setting for temporary data  ******/

IF NOT EXISTS (SELECT * FROM NAME WHERE NAME_KEY = 'LCA0002400000001')
INSERT INTO NAME (NAME_KEY, ORGANISATION,ENTERED_BY,ENTRY_DATE,SYSTEM_SUPPLIED_DATA,CUSTODIAN)
VALUES ('LCA0002400000001', 0, 'NBNSYS0000000001', GETDATE(), 1,'NBNSYS00'); 

IF NOT EXISTS (SELECT * FROM INDIVIDUAL WHERE NAME_KEY = 'LCA0002400000001')
INSERT INTO INDIVIDUAL (NAME_KEY,SURNAME,ENTERED_BY,ENTRY_DATE,SYSTEM_SUPPLIED_DATA)
VALUES ('LCA0002400000001','Withheld','NBNSYS0000000001', GETDATE(), 1);

DELETE FROM SETTING WHERE [NAME] = 'TempName';

INSERT INTO SETTING VALUES ('TempName', 'LCA0002400000001');

DELETE FROM SETTING WHERE [NAME] = 'SortMethod';

INSERT INTO SETTING VALUES ('SortMethod', 'Organism');


GO


/****** Metadata: Allows matching of taxon version keys ******/


/****** Object:  StoredProcedure [dbo].[usp_IWMatch_Species]    Script Date: 09/26/2013 17:48:47 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

/*===========================================================================*\
  Description:	Set the match for the specified import record in the match table.

  Parameters:	@ChecklistKey

  Created:	June 2004

  Last revision information:
    $Revision: 1 $
    $Date: 12/02/13 10:37 $
    $Author: MikeWeideli $

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
		-- Handle searches against the virtual preferred list
	
		
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
		-- Handle searches against a specified list
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

/****** Object:  StoredProcedure [dbo].[usp_IWMatch_AssociatedSpecies]    Script Date: 09/26/2013 17:48:47 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

/*===========================================================================*\
  Description:	Set the match for the specified import record in the match table.

  Parameters:	@ChecklistKey

  Created:	June 2004

  Last revision information:
    $Revision: 1 $
    $Date: 12/2/2013 10:37 $
    $Author: Mikeweideli $

\*===========================================================================*/
ALTER PROCEDURE [dbo].[usp_IWMatch_AssociatedSpecies]
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

  --  remove the sub genus from import_value 
     
     UPDATE #AssociatedSpecies Set Species_name = dbo.LCRemoveSubGenusText(Species_name)


	IF @ChecklistKey IS NULL
	BEGIN
		-- Handle searches against the virtual preferred list

		-- Set Match_Count first. Broken down in two separate updates for speed.
		UPDATE	UpdatedSpecies
		SET	Match_Count =  (SELECT	Count(DISTINCT ITN2.Taxon_List_Item_Key)
					FROM	#AssociatedSpecies S  
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
		FROM	#AssociatedSpecies UpdatedSpecies
		WHERE	Match_Key IS NULL

		-- Set Match_Count for species name + authority test. Note the strange way of doing
		-- this join - it seems to be faster this way!
		UPDATE	UpdatedSpecies
		SET	Match_Count = Match_Count + (SELECT	Count(DISTINCT ITN2.Taxon_List_Item_Key)
					FROM	#AssociatedSpecies S  
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
		FROM	#AssociatedSpecies UpdatedSpecies
		WHERE	Match_Key IS NULL


		-- Now get values and keys for unique matches only. Broken down in two separate updates for speed.
		UPDATE	#AssociatedSpecies
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
		AND	Species_Name = ITN1.Actual_Name
		AND TLI.Taxon_List_Version_To IS NULL

		UPDATE	#AssociatedSpecies
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
		AND	Species_Name = ITN1.Actual_Name + ' ' + ITN1.Authority
		AND TLI.Taxon_List_Version_To IS NULL

	END ELSE
	BEGIN
		-- Handle searches against a specified list

		-- Set Match_Count first. Broken down in two separate updates for speed.
		UPDATE	UpdatedSpecies
		SET	Match_Count =  (SELECT	Count(*)
					FROM	#AssociatedSpecies S 
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
		FROM	#AssociatedSpecies UpdatedSpecies
		WHERE	Match_Key IS NULL

		UPDATE	UpdatedSpecies
		SET	Match_Count = Match_Count + (SELECT	Count(*)
					FROM	#AssociatedSpecies S 
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
		FROM	#AssociatedSpecies UpdatedSpecies
		WHERE	Match_Key IS NULL


		-- Now get values and keys for unique matches only. Broken down in two separate updates for speed.
		UPDATE	#AssociatedSpecies
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

		UPDATE	#AssociatedSpecies
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
	FROM	#AssociatedSpecies	S
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
	UPDATE	#AssociatedSpecies
	SET	[Order] = 'Not available'
	WHERE	Match_Count = 1
	AND	[Order] IS NULL

GO


/****** Metadata: Procedure to removes entries from Taxon_Occurrence_Ext_Ref where Taxon Occurrence no longer Exists  ******/

Update Report_field set Field_size = 300 Where Report_Field_Key = 'JNCCDEV100000006'

IF EXISTS(SELECT 1 FROM sysobjects WHERE NAME='usp_ImportWizard_Unwanted_Imported')
  DROP PROCEDURE [usp_ImportWizard_Unwanted_Imported];
GO

/****** Object:  StoredProcedure [dbo].[usp_ImportWizard_Unwanted_Imported]    Script Date: 02/17/2013 17:34:46 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

/*===========================================================================*\
  Description:  Removes entries in Imported_Data_Keys which no longer have Taxon_Version_Keys
         
  Parameters:   None

  Created:      Feb 2013

  Last revision information:
       $Author: Mikeweideli$

\*===========================================================================*/

CREATE PROCEDURE [dbo].[usp_ImportWizard_Unwanted_Imported]
AS
   DELETE FROM Taxon_Occurrence_Ext_Ref 
   WHERE NOT EXISTS (SELECT * FROM Taxon_Occurrence WHERE 
   Taxon_Occurrence_Key = Taxon_Occurrence_Ext_Ref.Taxon_Occurrence_Key)
GO

GRANT EXECUTE ON  [dbo].[usp_ImportWizard_Unwanted_Imported] TO PUBLIC  

GO

/****** Metadata: Creates new table 'Taxon_Occurrence_Ext_Ref' ******/

IF EXISTS(SELECT 1 FROM SYSOBJECTS WHERE NAME='External_GUI_Link')
	DROP TABLE External_GUI_Link


GO
IF EXISTS(SELECT 1 FROM SYSOBJECTS WHERE NAME='Taxon_Occurrence_Ext_Ref')
	DROP TABLE Taxon_Occurrence_Ext_Ref
GO


/****** Object:  Table [dbo].[Taxon_Occurrence_Ext_Ref]    Script Date: 02/17/2013 17:41:25 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

SET ANSI_PADDING ON
GO

CREATE TABLE [dbo].[Taxon_Occurrence_Ext_Ref] (
	[Taxon_Occurrence_Ext_Ref_Key] [char](16) CONSTRAINT PK_TAXon_Occurrence_Ref PRIMARY KEY NOT NULL,
	[Taxon_Occurrence_Key] [char](16) NOT NULL,
	[External_Key] [varchar](30) NOT NULL,
	[Date_Imported] [datetime] NOT NULL)

GO

SET ANSI_PADDING OFF
GO

ALTER TABLE [dbo].[Taxon_Occurrence_Ext_Ref] ADD  CONSTRAINT [DF_Taxon_Occurrence_Ext_Ref_Date_Imported]  DEFAULT (getdate()) FOR [Date_Imported]
GO


GRANT SELECT ON Taxon_Occurrence_Ext_Ref TO R2k_Administrator 
GRANT UPDATE ON Taxon_Occurrence_Ext_Ref TO R2k_Administrator 
GRANT DELETE ON Taxon_Occurrence_Ext_Ref TO R2k_Administrator 
GRANT INSERT ON Taxon_Occurrence_Ext_Ref TO R2k_Administrator 
GRANT SELECT ON Taxon_Occurrence_Ext_Ref TO R2k_FullEdit
GRANT SELECT ON Taxon_Occurrence_Ext_Ref TO R2k_ReadOnly
GRANT SELECT ON Taxon_Occurrence_Ext_Ref TO R2k_AddOnly
GRANT SELECT ON Taxon_Occurrence_Ext_Ref TO R2k_RecordCardsOnly


GO

/* Add Iw entries for populating the  Taxon_Occurrence_Ext_Ref */

/* Delete all the entries in the tables. in reverse order */

DELETE FROM IW_Post_Processing_Procedure WHERE IW_Post_Processing_Procedure_Key  = 'LCA00023000000R8'
GO
DELETE FROM IW_Table_Rule_Related_Field WHERE IW_Table_Rule_Key  = 'LCA00023000000R8'



GO
DELETE FROM IW_Table_Rule_Related_Table WHERE IW_Table_Rule_Key = 'LCA00023000000R8'
GO
DELETE FROM IW_Table_Rule_OUTPUT_FIELD WHERE IW_Table_Rule_Key = 'LCA00023000000R8'
GO
DELETE FROM IW_Output_field  WHERE IW_Output_Field_Key  = 'LCA00023000000R8'
GO
DELETE FROM IW_Output_field  WHERE IW_Output_Field_Key  = 'LCA00023000000R9'
GO
DELETE FROM IW_Table_Rule WHERE IW_Table_Rule_Key = 'LCA00023000000R8'
GO
DELETE FROM IW_Column_Type_Pattern WHERE IW_Column_Type_Key = 'LCA00023000000R8'
GO
DELETE FROM IW_Column_Type_Match_Rule WHERE IW_Column_Type_Key = 'LCA00023000000R8'
GO
DELETE FROM IW_Match_Rule WHERE IW_Match_Rule_Key = 'LCA00023000000R8'
GO
DELETE FROM IW_Column_Type WHERE IW_Column_Type_Key  = 'LCA00023000000R8'
GO

DELETE FROM DataBase_RelationShip WHERE Relationship_Key   = 'LCA00023000000R8'
GO

INSERT INTO IW_Column_Type (IW_Column_Type_Key,Class_Name,Item_Name,Required,
Commonly_Used, Sequence,Parser_Class_Name,Maximum_Length,Entered_By,
Entry_Date,System_Supplied_Data) 
VALUES ('LCA00023000000R8','TColumnType','External Key',0,0,9,'TTextParser',
30,'TESTDATA00000001',GetDate(),1)

GO

INSERT INTO IW_TABLE_RULE
(IW_Table_Rule_Key,Sequence,Table_Name,Filter_Expression,Entered_By,
Entry_Date,System_Supplied_Data)
VALUES ('LCA00023000000R8',17,'Taxon_Occurrence_Ext_Ref','#master.LCA00023000000R8_data <> ''''',
'TESTDATA00000001',GetDate(),1)

GO


INSERT INTO IW_OUTPUT_FIELD
(IW_outPut_Field_Key,Name,Data_Type,IW_Column_Type_Key,Source_Field_Name,Entered_by,
Entry_date,System_Supplied_data) 
VALUES ('LCA00023000000R8','External_Key','VARCHAR(30)','LCA00023000000R8','data',
'TESTDATA00000001',GetDate(),1)

GO

INSERT INTO IW_OUTPUT_FIELD
(IW_outPut_Field_Key,Name,Data_Type,Generating_Class_Name,Generator_Field_Index, Entered_by,
Entry_date,System_Supplied_data) 
VALUES ('LCA00023000000R9','Taxon_Occurrence_Ext_Ref_Key','CHAR(16)','TKeyFieldGenerator',0,
'TESTDATA00000001',GetDate(),1)

GO


INSERT IW_Table_Rule_OUTPUT_FIELD (IW_table_Rule_Key,IW_Output_Field_Key,Entered_By,
Entry_Date,System_supplied_data)
VALUES ('LCA00023000000R8','LCA00023000000R8','TESTDATA00000001',GetDate(),1)


GO

INSERT IW_Table_Rule_OUTPUT_FIELD (IW_table_Rule_Key,IW_Output_Field_Key,Entered_By,
Entry_Date,System_supplied_data)
VALUES ('LCA00023000000R8','SYSTEM010000001A','TESTDATA00000001',GetDate(),1)

GO

INSERT IW_Table_Rule_OUTPUT_FIELD (IW_table_Rule_Key,IW_Output_Field_Key,Entered_By,
Entry_Date,System_supplied_data)
VALUES ('LCA00023000000R8','LCA00023000000R9','TESTDATA00000001',GetDate(),1)


GO

INSERT INTO IW_Table_Rule_Related_Field (IW_Table_Rule_Key,IW_Column_Type_Key,Relationship,
Entered_By,Entry_date,System_supplied_data)
VALUES ('LCA00023000000R8','LCA00023000000R8',2,'TESTDATA00000001',GetDate(),1)

GO


INSERT INTO IW_Table_Rule_Related_Table (IW_Table_Rule_Key,Table_name,Relationship,
Entered_By,Entry_date,System_supplied_data)
VALUES ('LCA00023000000R8','Taxon_Occurrence',1,'TESTDATA00000001',GetDate(),1)

GO

INSERT INTO IW_Post_Processing_procedure (IW_Post_Processing_Procedure_Key,Sequence,Procedure_name,
Entered_By,Entry_date,System_supplied_data)
VALUES ('LCA00023000000R8',2 ,'usp_ImportWizard_Unwanted_Imported','TESTDATA00000001',GetDate(),1)

GO

INSERT INTO DATABASE_RELATIONSHIP (Relationship_Key, Relationship_Name,Master_table,Master_Field,Detail_Table,Detail_Field,
Follow_Up,Follow_Down,One_to_One)
VALUES ('LCA00023000000R8','TAXON_OCCURRENCEIMPORTEDDATAKEYS','TAXON_OCCURRENCE','TAXON_OCCURRENCE_KEY',
'Taxon_Occurrence_Ext_Ref', 'TAXON_OCCURRENCE_KEY',0,1,0)

GO

INSERT INTO IW_Column_type_Pattern  (IW_Column_Type_Key,Pattern,Exclude_Match,
Entered_By,Entry_date,System_supplied_data)
VALUES ('LCA00023000000R8','ext%' ,0,'TESTDATA00000001',GetDate(),1) 

INSERT INTO IW_Column_type_Pattern  (IW_Column_Type_Key,Pattern,Exclude_Match,
Entered_By,Entry_date,System_supplied_data)
VALUES ('LCA00023000000R8','%gui%' ,0,'TESTDATA00000001',GetDate(),1) 
GO

/* UDF for consolidating the normal and temporary Recorder in the report wizard*/


/****** Object:  UserDefinedFunction [dbo].[ConsolidatedRecorder]    Script Date: 09/26/2013 19:04:39 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
/****** Object:  UserDefinedFunction [dbo].[ConsolidateRecorder]    Script Date: 10/08/2012 18:04:43 ******/


IF  EXISTS (SELECT * FROM sysobjects WHERE id = OBJECT_ID(N'[dbo].[ConsolidateRecorder]') AND type in (N'FN', N'TF'))
DROP FUNCTION [dbo].[ConsolidateRecorder]

GO
/*===========================================================================*\
  Description:	Returns the Recorder name from the Recorders column in sample if the field is not null
  Otherwise returns the Recorder in the normal way  

  Parameters:
  @SampleKey - Sample key 
 		  
  Created:	September 2013
  Author: MikeWeideli 

\*=========================================================================== */

CREATE FUNCTION [dbo].[ConsolidateRecorder]
(@SampleKey char(16) )

RETURNS varchar(8000)
AS
BEGIN

--****************************************************************************************************
DECLARE @ReturnString varchar(8000)


Select @ReturnString = Sample.Recorders
FROM Sample where Sample_Key = @SampleKey

If  @ReturnString IS NULL SET @ReturnString = [dbo].[FormatEventRecorders](@SampleKey)
    

RETURN @ReturnString

END

GO

GRANT EXECUTE ON [dbo].[ConsolidateRecorder] TO PUBLIC

GO

/****** Including consolidated recorder in the report wizard: ******/

Delete From REPORT_FIELD 
Where Exists (Select * From REPORT_FIELD Where REPORT_FIELD_KEY = 'LCA0002200000009') and REPORT_FIELD_KEY = 'LCA0002200000009'
Delete From REPORT_ATTRIBUTE
Where Exists (Select * From REPORT_ATTRIBUTE Where REPORT_ATTRIBUTE_KEY = 'LCA0002200000009') and REPORT_ATTRIBUTE_KEY = 'LCA0002200000009'

GO

Insert Into REPORT_ATTRIBUTE
Values ('LCA0002200000009', 'Sample', 'SAMPLE', 'Consolidated Recorders', 
'#REPORT_OUTPUT.[Consolidated Recorders] = dbo.ConsolidateRecorder(SAMPLE_KEY)', 
'NBNSYS0000000035', NULL, 'TESTDATA00000001', GetDate(), NULL, NULL, 1)
Insert Into REPORT_FIELD
Values ('LCA0002200000009', 'LCA0002200000009', 'Consolidated Recorders', 'varchar', 100, 
'TESTDATA00000001', GetDate(), NULL, NULL, 1)

GO


IF  EXISTS (SELECT * FROM sysobjects WHERE id = OBJECT_ID(N'[dbo].[GetExternalRef]') AND type in (N'FN', N'TF'))
DROP FUNCTION [dbo].[GetExternalRef]

GO
/****** Object:  UserDefinedFunction [dbo].[GetExternalRef]    Script Date: 09/27/2013 21:00:55 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

/*===========================================================================*\
  Description:	Returns the External Key for a given Taxon Occurrence Key

  Parameters:
  @ToccKey - Taxon occurrence key 
 		  
  Created:	September 2013
  Author: MikeWeideli 

\*=========================================================================== */

CREATE FUNCTION [dbo].[GetExternalRef]
(@ToccKey char(16) )

RETURNS varchar(40)
AS
BEGIN

--****************************************************************************************************
DECLARE @ReturnString varchar(40)


Select @ReturnString = Taxon_Occurrence_Ext_Ref.External_key
FROM Taxon_Occurrence_Ext_Ref where Taxon_Occurrence_key = @TOCCKey

If  @ReturnString IS NULL SET @ReturnString = 'Not recorded'
    

RETURN @ReturnString

END

GO



GRANT EXECUTE ON [dbo].[GetExternalRef] TO Public

GO

/****** Including external key in the report wizard: ******/

Delete From REPORT_FIELD 
Where Exists (Select * From REPORT_FIELD Where REPORT_FIELD_KEY = 'LCA0002200000010') and REPORT_FIELD_KEY = 'LCA0002200000010'

Delete From REPORT_ATTRIBUTE
Where Exists (Select * From REPORT_ATTRIBUTE Where REPORT_ATTRIBUTE_KEY = 'LCA0002200000010') and REPORT_ATTRIBUTE_KEY = 'LCA0002200000010'

GO
Insert Into REPORT_ATTRIBUTE
Values ('LCA0002200000010', 'Taxon\Observations', 'OBSERVATION', 'External Key', 
'#REPORT_OUTPUT.[External Key] =[dbo].[GetExternalRef](TAXON_OCCURRENCE_KEY)', 
'NBNSYS0000000016', 'NBNSYS0000000000', 'TESTDATA00000001', GetDate(), NULL, NULL, 1)

Go

Insert Into REPORT_FIELD
Values ('LCA0002200000010', 'LCA0002200000010', 'External Key', 'varchar', 40, 
'TESTDATA00000001', GetDate(), NULL, NULL, 1)

GO


/****** Modify script to allow the import of Name keys in the Recorders column of Iw ******/


/****** Object:  StoredProcedure [dbo].[usp_IWMatchRemembered_Names]    Script Date: 09/28/2013 21:25:08 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

/*===========================================================================*\
  Description:	Populate import table with matched values from previous imports.

  Parameters:	<none>

  Created:	July 2004

  Last revision information:
    $Revision: 8 $
    $Date: 28/09/13 15:52 $
    $Author: mikeweideli $

\*===========================================================================*/
ALTER PROCEDURE [dbo].[usp_IWMatchRemembered_Names]
AS
	-- Update temp table with relevant data.
	UPDATE 	#Names
	SET 	Match_Count = 1, 
		Match_Key = Matched_Key, 
		Match_Value = dbo.ufn_GetFormattedName(Matched_Key), 
		Remembered = 1
	FROM 	IW_Matched_Names 
	JOIN	[Name] ON Name_Key = Matched_Key
	WHERE 	Import_Value = Matched_Value 
	AND 	Match_Key IS NULL AND NAME_KEY <> Import_Value     
	
	
	UPDATE 	#Names
	SET 	Match_Count = 1, 
		Match_Key = I.Name_key,
		Match_Value =  I.NAME_KEY,
		Remembered = 1
	FROM 	Individual I 
	JOIN  #Names On I.Name_Key = #names.import_value 
  	WHERE 	Match_Key IS NULL      


/****** Scripts and table entries to allow the import of location keys in the Location column of the IW. ******/

/****** Object:  StoredProcedure [dbo].[usp_IWMatchRemembered_Locations]    Script Date: 09/28/2013 21:28:04 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

/*===========================================================================*\
  Description:	Populate import table with matched values from previous imports.

  Parameters:	<none>

  Created:	July 2004

  Last revision information:
    $Revision: 8 $
    $Date: 28 Sep 2013 15:52 $
    $Author: mikeweideli $

\*===========================================================================*/
ALTER PROCEDURE [dbo].[usp_IWMatchRemembered_Locations]
AS
	-- Update temp table with relevant data.
	UPDATE 	#Locations
	SET 	Match_Count = 1, 
		Match_Key = L.Location_Key, 
		Match_Value = L.Location_Key,
		Remembered = 1,
		Spatial_Ref = L.Spatial_Ref,
		Spatial_Ref_System = L.Spatial_Ref_System,
		Lat = L.Lat,
		Long = L.Long
	FROM 	IW_Matched_Locations 
	JOIN	Location L ON L.Location_Key = Matched_Key
	JOIN	Location_Name LN ON LN.Location_Key = L.Location_Key AND LN.Preferred = 1
	WHERE 	Import_Value = Matched_Value 
	AND 	Match_Key IS NULL AND Import_Value <> L.LOCATION_KEY
	     
    UPDATE 	#Locations
	SET 	Match_Count = 1, 
		Match_Key = L.Location_key, 
		Match_Value = L.Location_key,
		Remembered = 1,
		Spatial_Ref = L.Spatial_Ref,
		Spatial_Ref_System = L.Spatial_Ref_System,
		Lat = L.Lat,
		Long = L.Long
		
	
	FROM 	Location L 
	JOIN #Locations ON L.Location_Key = Import_value
	JOIN	Location_Name LN ON LN.Location_Key = L.Location_Key AND LN.Preferred = 1
	WHERE 	Match_Key IS NULL 
  
Go



IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ImportWizard_CalcSampleSELocationName') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_ImportWizard_CalcSampleSELocationName] 
GO



/****** Object:  StoredProcedure [dbo].[usp_ImportWizard_CalcSampleSELocationName]    Script Date: 09/28/2013 21:32:52 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

/*===========================================================================*\
  Description:  Sets the Location Name in #sample and #survey event to null
  in situations where the Location Name is populated with the Location key  
  Parameters:   None

  Created:      Sept 2013

  Last revision information:
    $Revision: 1 $
    $Date: 28/09/13 15:52 $
    $Author: MikeWeideli $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ImportWizard_CalcSampleSELocationName]
AS
       UPDATE      #Survey_Event
    SET      LOCATION_NAME =  Null 
               WHERE Location_Name = Location_Key 
  
    UPDATE #SURVEY_EVENT
    SET  Location_Name = RIGHT(Location_Name,Len(Location_Name)-18) 
          WHERE  Location_Name  Like(LOCATION_Key + '%') AND  len(LOCATION_NAME) > 17         

   UPDATE      #Sample
    SET      LOCATION_NAME =  Null 
               WHERE Location_Name = Location_Key 
  
    UPDATE #Sample
    SET  Location_Name = RIGHT(Location_Name,Len(Location_Name)-18) 
          WHERE  Location_Name  Like(LOCATION_Key + '%') AND  len(LOCATION_NAME) > 17  


GO

GRANT EXECUTE ON [dbo].[usp_ImportWizard_CalcSampleSELocationName] to Public
               
GO

Delete FROM IW_Post_Processing_Procedure WHERE IW_Post_Processing_Procedure_Key = 'SYSTEM0100000004'

GO

INSERT INTO IW_Post_Processing_procedure (IW_Post_Processing_Procedure_Key,Sequence,Procedure_name,
Entered_By,Entry_date,System_supplied_data)
VALUES ('SYSTEM0100000004',3 ,'usp_ImportWizard_CalcSampleSELocationName','TESTDATA00000001',GetDate(),1)

GO

IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ImportWizard_TidyGridRef') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_ImportWizard_TidyGridRef] 
GO


/****** Scripts and table entries to fix lower case errors in UK grid refs ******/

/****** Object:  StoredProcedure [dbo].[usp_ImportWizard_TidyGridRef]    Script Date: 09/28/2013 21:58:49 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

/*===========================================================================*\
  Description:  Makes sure the spatial ref on OSGB and OSNI is upper case for sample and events
  Parameters:   None

  Created:      Sept 2013

  Last revision information:
    $Revision: 1 $
    $Date: 28/09/13 15:52 $
    $Author: MikeWeideli $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ImportWizard_TidyGridRef]
AS
    UPDATE      #Survey_Event
    SET         spatial_ref = UPPER(spatial_ref)
                where Spatial_Ref_System IN ('OSGB', 'OSNI')

    UPDATE      #Sample
    SET         spatial_ref = UPPER(spatial_ref)
                where Spatial_Ref_System IN ('OSGB', 'OSNI')
                    

GO

GRANT EXECUTE ON [dbo].[usp_ImportWizard_TidyGridRef] to Public

GO

Delete FROM IW_Post_Processing_Procedure WHERE IW_Post_Processing_Procedure_Key = 'SYSTEM0100000005'

GO

INSERT INTO IW_Post_Processing_procedure (IW_Post_Processing_Procedure_Key,Sequence,Procedure_name,
Entered_By,Entry_date,System_supplied_data)
VALUES ('SYSTEM0100000005',4 ,'usp_ImportWizard_TidyGridRef','TESTDATA00000001',GetDate(),1)
                   
GO


IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ImportWizard_TidyLocationGridRef') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_ImportWizard_TidyLocationGridRef]
GO

/****** Scripts and table entries to fix lower case errors in UK grid refs ******/

/****** Object:  StoredProcedure [dbo].[usp_ImportWizard_TidyLocationGridRef]    Script Date: 09/28/2013 21:58:49 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

/*===========================================================================*\
  Description:  Makes sure the spatial ref on OSGB and OSNI is upper case for locations
  Parameters:   None

  Created:      Sept 2013

  Last revision information:
    $Revision: 1 $
    $Date: 28/09/13 15:52 $
    $Author: MikeWeideli $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ImportWizard_TidyLocationGridRef]
AS
    UPDATE      #Location
    SET         spatial_ref = UPPER(spatial_ref)
                where Spatial_Ref_System IN ('OSGB', 'OSNI')

  
GO

GRANT EXECUTE ON [dbo].[usp_ImportWizard_TidyLocationGridRef] to Public

GO

Delete FROM IW_Post_Processing_Procedure WHERE IW_Post_Processing_Procedure_Key = 'SYSTEM0100000006'

GO

INSERT INTO IW_Post_Processing_procedure (IW_Post_Processing_Procedure_Key,Sequence,Required_Table_Name,Procedure_Name,
Entered_By,Entry_date,System_supplied_data)
VALUES ('SYSTEM0100000006',4 ,'Location','usp_ImportWizard_TidyLocationGridRef','TESTDATA00000001',GetDate(),1)
                   

/*==========================================================================*/
GO

/****** Fixes constrainst on Survey_Media_Type ******/

DELETE FROM Usable_Table WHERE Usable_table_Key = 'ABABABAB00000100'

GO

INSERT INTO Usable_Table (Usable_Table_Key, Table_Name, Link_Table,Link,Apply_To,Join_Order)
VALUES ('ABABABAB00000100','Survey_Media','Survey','Survey_Media.Survey_Media_Key = Survey.Survey_Media_Key','A',5)

GO
/****** Fixes errors in date filters  ******/

/****** Object:  View [dbo].[LC_DATE_TOCC_CHANGED]    Script Date: 11/14/2012 21:30:24 ******/

IF  EXISTS (SELECT * FROM sysobjects WHERE id = OBJECT_ID(N'[dbo].[LC_DATE_TOCC_CHANGED]') AND type in (N'V'))
DROP VIEW [dbo].[LC_DATE_TOCC_CHANGED]

GO

SET ANSI_NULLS ON

GO

SET QUOTED_IDENTIFIER ON
GO

/*===========================================================================*\
  Description:	VIEW which returns the maximum data changed for a Taxon_Occurrence key taking into 
   account Survey Event and Sample Changes. Format retuned is yyymmdd
              
  Created:	November 2012
  Author: MikeWeideli
  Last Changes: Thomas Schneider, 02.10.2013

\*=========================================================================== */
CREATE VIEW [dbo].[LC_DATE_TOCC_CHANGED]
AS
SELECT ToccKEY AS Taxon_Occurrence_key, MAX(DATESTRING) as DateString 
FROM
(Select TOCC.taxon_occurrence_key as TOccKey,
ISNULL( CAST(CONVERT(char(10), S.CHANGED_DATE, 112) AS INT), CAST(CONVERT(char(10), S.ENTRY_DATE, 112) AS INT) ) 
  AS DATESTRING
FROM sample S
INNER JOIN taxon_occurrence Tocc On Tocc.sample_key = S.sample_key

UNION 

SELECT
TOCC.taxon_occurrence_key AS TOccKey,
ISNULL( CAST(CONVERT(char(10), SE.CHANGED_DATE, 112) AS INT), CAST(CONVERT(char(10), SE.ENTRY_DATE, 112) AS INT) ) 
  AS DATESTRING
FROM sample S
INNER JOIN taxon_occurrence Tocc On Tocc.sample_key = S.sample_key
INNER JOIN Survey_event SE ON SE.Survey_Event_key = S.Survey_Event_key

UNION 

SELECT
TOCC.taxon_occurrence_key as TOccKey,
ISNULL( CAST(CONVERT(char(10), Tocc.CHANGED_DATE, 112) AS INT), CAST(CONVERT(char(10), Tocc.ENTRY_DATE, 112) AS INT) ) 
  AS DATESTRING
FROM taxon_occurrence Tocc) AS D

GROUP BY D.TOCCKey


GO

GRANT SELECT ON [dbo].[LC_DATE_TOCC_CHANGED] TO PUBLIC

GO

/****** Object:  View [dbo].[LC_Date_Filter_Changed]    Script Date: 08/27/2012 21:30:24 ******/


IF  EXISTS (SELECT * FROM sysobjects WHERE id = OBJECT_ID(N'[dbo].[LC_DATE_FILTER_CHANGED]') AND type in (N'V'))
DROP VIEW [dbo].[LC_DATE_FILTER_CHANGED]

GO

SET ANSI_NULLS ON

GO

SET QUOTED_IDENTIFIER ON
GO

/*===========================================================================*\
  Description:	VIEW which changes the changed date into yyymmdd format 
              
  Created:	August  2012
  Author: MikeWeideli 
  Last Changes: Thomas Schneider, 02.10.2013

\*=========================================================================== */
CREATE VIEW [dbo].[LC_DATE_FILTER_CHANGED]
AS
SELECT     TAXON_OCCURRENCE_KEY, 
ISNULL(CAST(CONVERT(char(10), CHANGED_DATE, 112) AS INT), CAST(CONVERT(char(10), ENTRY_DATE, 112) AS INT)) 
AS DATESTRING

FROM dbo.TAXON_OCCURRENCE


GO

GRANT SELECT ON [dbo].[LC_DATE_FILTER_CHANGED] TO PUBLIC

GO


/****** Metadata: Creates new table 'Duplicate_Control' ******/

IF EXISTS(SELECT 1 FROM SYSOBJECTS WHERE NAME='Duplicate_Control')
	DROP TABLE Duplicate_control

GO

/****** Object:  Table [dbo].[Duplicate_Control]    Script Date: 10/19/2013 20:29:54 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

SET ANSI_PADDING ON
GO

CREATE TABLE [dbo].[Duplicate_Control](
	[Taxon_Occurrence_Key1] [char](16) NOT NULL,
	[Taxon_Occurrence_Key2] [char](16) NOT NULL,
	[Indicator] [int] NULL
) ON [PRIMARY]

GO

SET ANSI_PADDING OFF
GO

ALTER TABLE [dbo].[Duplicate_Control] ADD  CONSTRAINT [DF_Duplicate_Control_Indicator]  DEFAULT (0) FOR [Indicator]
GO


GRANT SELECT ON Duplicate_Control TO R2k_Administrator 
GRANT UPDATE ON Duplicate_Control TO R2k_Administrator 
GRANT DELETE ON Duplicate_Control TO R2k_Administrator 
GRANT INSERT ON Duplicate_Control TO R2k_Administrator 
GRANT SELECT ON Duplicate_Control TO R2k_FullEdit
GRANT SELECT ON Duplicate_Control TO R2k_ReadOnly
GRANT SELECT ON Duplicate_Control TO R2k_AddOnly
GRANT SELECT ON Duplicate_Control TO R2k_RecordCardsOnly


GO
IF  EXISTS (SELECT * FROM sysobjects WHERE id = OBJECT_ID(N'[dbo].[GetLegacySortOrders]') AND type in (N'FN', N'TF'))
DROP FUNCTION [dbo].[GetLegacySortOrders] 

GO
/****** Object:  UserDefinedFunction [dbo].[GetLegacySortOrders]    Script Date: 10/21/2013 16:46:30 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

/*===========================================================================*\
  Description:	Returns the Sort Order used prior to the introduction of the Organism table 
  Plus the sort order based on the preferred name. This is for use by users who use only 
  one Taxon List for their input.  
  Parameters:
  @TLIKey - Taxon List Item key 
  @Method - 0 for sort based on the Recommended Taxon List Item key
		 1 for sort order based on the Preferred Name (TLI key)   		  
  Created:	September 2013
  Author: MikeWeideli 

\*=========================================================================== */

CREATE FUNCTION [dbo].[GetLegacySortOrders]
(@TLIKey char(16) , @Method int)

RETURNS varchar(30)
AS
BEGIN

--****************************************************************************************************
DECLARE @ReturnString varchar(30)

If @Method = 0
   Select @ReturnString = RIGHT('000'+CAST(ISNULL(TG.SORT_ORDER,'0') AS VARCHAR(3)),3) + RIGHT('000000000000000000000000000'+CAST(ISNULL(TLI.SORT_CODE,'0') AS VARCHAR(27)),27)
FROM INDEX_TAXON_NAME ITN INNER JOIN TAXON_LIST_ITEM TLI ON TLI.TAXON_LIST_ITEM_KEY = ITN.RECOMMENDED_TAXON_LIST_ITEM_KEY  INNER JOIN TAXON_VERSION TV ON TV.TAXON_VERSION_KEY = TLI.TAXON_VERSION_KEY 
INNER JOIN TAXON_GROUP TG ON TG.TAXON_GROUP_KEY = TV.OUTPUT_GROUP_KEY WHERE ITN.Taxon_List_Item_Key = @TLIKey

ELSE
  Select @ReturnString = RIGHT('000'+CAST(ISNULL(TG.SORT_ORDER,'0') AS VARCHAR(3)),3) + RIGHT('000000000000000000000000000'+CAST(ISNULL(TLI2.SORT_CODE,'0') AS VARCHAR(27)),27)
  FROM  TAXON_LIST_ITEM TLI  
  INNER JOIN TAXON_LIST_ITEM TLI2 ON TLI2.TAXON_LIST_ITEM_KEY  = TLI.PREFERRED_NAME 
  INNER JOIN TAXON_VERSION TV ON TV.TAXON_VERSION_KEY = TLI2.TAXON_VERSION_KEY 
  INNER JOIN TAXON_GROUP TG ON TG.TAXON_GROUP_KEY = TV.OUTPUT_GROUP_KEY WHERE TLI.Taxon_List_Item_Key = @TLIKey


RETURN @ReturnString

END

GO

GRANT EXECUTE ON [dbo].[GetLegacySortOrders] TO Public

GO

/****** Including legacy sort orders the report wizard: ******/

Delete From REPORT_FIELD 
Where Exists (Select * From REPORT_FIELD Where REPORT_FIELD_KEY = 'LCA0002200000011') and REPORT_FIELD_KEY = 'LCA0002200000011'

Delete From REPORT_ATTRIBUTE
Where Exists (Select * From REPORT_ATTRIBUTE Where REPORT_ATTRIBUTE_KEY = 'LCA0002200000011') and REPORT_ATTRIBUTE_KEY = 'LCA0002200000011'

GO

Insert Into REPORT_ATTRIBUTE
Values ('LCA0002200000011', 'Taxon', 'TAXON_LIST', 'Taxon List Sort Order', 
'#REPORT_OUTPUT.[Taxon List Sort Order] = dbo.GetLegacySortOrders(TAXON_LIST_ITEM.TAXON_LIST_ITEM_KEY,0)', 
'NBNSYS0000000030', 'NBNSYS0000000000', 'TESTDATA00000001', GetDate(), NULL, NULL, 1)
GO
Insert Into REPORT_FIELD
Values ('LCA0002200000011', 'LCA0002200000011', 'Taxon List Sort Order', 'varchar', 30, 
'TESTDATA00000001', GetDate(), NULL, NULL, 1)

GO

Delete From REPORT_FIELD 
Where Exists (Select * From REPORT_FIELD Where REPORT_FIELD_KEY = 'LCA0002200000012') and REPORT_FIELD_KEY = 'LCA0002200000012'
GO
Delete From REPORT_ATTRIBUTE
Where Exists (Select * From REPORT_ATTRIBUTE Where REPORT_ATTRIBUTE_KEY = 'LCA0002200000012') and REPORT_ATTRIBUTE_KEY = 'LCA0002200000012'

GO

Insert Into REPORT_ATTRIBUTE
Values ('LCA0002200000012', 'Taxon', 'TAXON_LIST', 'Preferred Sort Order', 
'#REPORT_OUTPUT.[Preferred Sort Order] = dbo.GetLegacySortOrders(TAXON_LIST_ITEM.TAXON_LIST_ITEM_KEY,1)', 
'NBNSYS0000000030', 'NBNSYS0000000000', 'TESTDATA00000001', GetDate(), NULL, NULL, 1)
GO
Insert Into REPORT_FIELD
Values ('LCA0002200000012', 'LCA0002200000012', 'Preferred Sort Order', 'varchar', 30, 
'TESTDATA00000001', GetDate(), NULL, NULL, 1)

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[nbn_exchange_add_comment]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[nbn_exchange_add_comment]
GO

/*===========================================================================*\
  Author  Stuart Ball, JNCC
  Date    22 May 2008

  Description	
	Add TAXON_OCCURRENCE.COMMENT to the Comment field in nbn_exchange_obs

  Parameters
	
  Revision information
  SGB  16 Jan 2009 - convert RTF to plain text

\*===========================================================================*/
create procedure [dbo].[nbn_exchange_add_comment]
as
-- First do the ones where we have RTF and/or line feeds
UPDATE ##nbn_exchange_obs 
SET Comment = dbo.ufn_RtfToPlaintext(TAXON_OCCURRENCE.[COMMENT])
FROM ##nbn_exchange_obs INNER JOIN
TAXON_OCCURRENCE ON ##nbn_exchange_obs.RecordKey = TAXON_OCCURRENCE.TAXON_OCCURRENCE_KEY 
WHERE (TAXON_OCCURRENCE.[COMMENT] LIKE '{\rtf%')

-- Next check for any with LF/CR
UPDATE ##nbn_exchange_obs 
SET Comment = dbo.nbn_exchange_strip_LFCR(TAXON_OCCURRENCE.[COMMENT])
FROM ##nbn_exchange_obs INNER JOIN
TAXON_OCCURRENCE ON ##nbn_exchange_obs.RecordKey = TAXON_OCCURRENCE.TAXON_OCCURRENCE_KEY 
WHERE ((TAXON_OCCURRENCE.[COMMENT] LIKE '%' + CHAR(13) + '%') OR (TAXON_OCCURRENCE.[COMMENT] LIKE '%' + CHAR(10) + '%')) 
AND (##nbn_exchange_obs.[Comment] is null)

-- then simply transfer the rest
UPDATE ##nbn_exchange_obs 
SET Comment = TAXON_OCCURRENCE.[COMMENT]
FROM ##nbn_exchange_obs INNER JOIN
TAXON_OCCURRENCE ON ##nbn_exchange_obs.RecordKey = TAXON_OCCURRENCE.TAXON_OCCURRENCE_KEY 
WHERE (TAXON_OCCURRENCE.[COMMENT] Is Not Null) AND (##nbn_exchange_obs.[Comment] is null)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.nbn_exchange_add_comment') AND SysStat & 0xf = 4)
BEGIN
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.nbn_exchange_add_comment TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.nbn_exchange_add_comment TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.nbn_exchange_add_comment TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.nbn_exchange_add_comment TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.nbn_exchange_add_comment TO [R2k_RecordCardsOnly]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[nbn_exchange_add_determiner]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[nbn_exchange_add_determiner]
GO

/*===========================================================================*\
  Author  Stuart Ball, JNCC
  Date    22 May 2008

  Description	
	Add Determiner's name to the Determiner field in nbn_exchange_obs

  Parameters
	
  Revision information

\*===========================================================================*/
create procedure [dbo].[nbn_exchange_add_determiner]
as
UPDATE ##nbn_exchange_obs 
SET Determiner = dbo.ufn_GetFormattedName(TAXON_DETERMINATION.DETERMINER)
FROM ##nbn_exchange_obs INNER JOIN
TAXON_DETERMINATION ON ##nbn_exchange_obs.RecordKey = TAXON_DETERMINATION.TAXON_OCCURRENCE_KEY AND TAXON_DETERMINATION.PREFERRED = 1
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.nbn_exchange_add_determiner') AND SysStat & 0xf = 4)
BEGIN
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.nbn_exchange_add_determiner TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.nbn_exchange_add_determiner TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.nbn_exchange_add_determiner TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.nbn_exchange_add_determiner TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.nbn_exchange_add_determiner TO [R2k_RecordCardsOnly]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[nbn_exchange_add_recorders]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[nbn_exchange_add_recorders]
GO

/*===========================================================================*\
  Author  Stuart Ball, JNCC
  Date    22 May 2008

  Description	
	Add (list of) recorder's name(s) to the Recorder field in nbn_exchange_obs

  Parameters
	
  Revision information

\*===========================================================================*/
create procedure [dbo].[nbn_exchange_add_recorders]
as
UPDATE ##nbn_exchange_obs 
SET Recorder = dbo.FormatEventRecorders(SampleKey)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.nbn_exchange_add_recorders') AND SysStat & 0xf = 4)
BEGIN
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.nbn_exchange_add_recorders TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.nbn_exchange_add_recorders TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.nbn_exchange_add_recorders TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.nbn_exchange_add_recorders TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.nbn_exchange_add_recorders TO [R2k_RecordCardsOnly]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[nbn_exchange_add_sample_type]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[nbn_exchange_add_sample_type]
GO

/*===========================================================================*\
  Author  Stuart Ball, JNCC
  Date    22 May 2008

  Description	
	Add Sample type to the SampleMethod field in nbn_exchange_obs

  Parameters
	
  Revision information

\*===========================================================================*/
create procedure [dbo].[nbn_exchange_add_sample_type]
as
UPDATE ##nbn_exchange_obs 
SET SampleMethod = SAMPLE_TYPE.SHORT_NAME
FROM ##nbn_exchange_obs INNER JOIN 
SAMPLE ON ##nbn_exchange_obs.SampleKey = SAMPLE.SAMPLE_KEY INNER JOIN
SAMPLE_TYPE ON SAMPLE.SAMPLE_TYPE_KEY = SAMPLE_TYPE.SAMPLE_TYPE_KEY
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.nbn_exchange_add_sample_type') AND SysStat & 0xf = 4)
BEGIN
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.nbn_exchange_add_sample_type TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.nbn_exchange_add_sample_type TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.nbn_exchange_add_sample_type TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.nbn_exchange_add_sample_type TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.nbn_exchange_add_sample_type TO [R2k_RecordCardsOnly]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[nbn_exchange_add_substrate]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[nbn_exchange_add_substrate]
GO

/*===========================================================================*\
  Author  Stuart Ball, JNCC
  Date    22 May 2008

  Description	
	Add substrate to the Substrate field in nbn_exchange_obs

  Parameters
	
  Revision information

\*===========================================================================*/
create procedure [dbo].[nbn_exchange_add_substrate]
as
UPDATE ##nbn_exchange_obs 
SET Substrate = SUBSTRATE.SHORT_NAME
FROM ##nbn_exchange_obs INNER JOIN 
TAXON_OCCURRENCE ON ##nbn_exchange_obs.RecordKey = TAXON_OCCURRENCE.TAXON_OCCURRENCE_KEY
INNER JOIN
SUBSTRATE ON TAXON_OCCURRENCE.SUBSTRATE_KEY = SUBSTRATE.SUBSTRATE_KEY
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.nbn_exchange_add_substrate') AND SysStat & 0xf = 4)
BEGIN
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.nbn_exchange_add_substrate TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.nbn_exchange_add_substrate TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.nbn_exchange_add_substrate TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.nbn_exchange_add_substrate TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.nbn_exchange_add_substrate TO [R2k_RecordCardsOnly]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[nbn_exchange_basic_update]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[nbn_exchange_basic_update]
GO
                      
/*===========================================================================*\
  Author  Stuart Ball, JNCC
  Date    20 May 2008

  Description	
     Update nbn_exchange_obs with basic information about the occurrences
     sample and survey. All of this information should always be present in
     some form, so we don't need to worry about joins.

     Add a location name to SiteName

     Add grid Precision and fix cases where the Spatial ref system isn't OSNI or OSGB
     in which case we store the Lat/Long

     Fix dates to ensure Nulls anr handled correctly

  Parameters
     None.
	
  Revision information
  Updated SGB 16 Jan 2009

\*===========================================================================*/
CREATE procedure [dbo].[nbn_exchange_basic_update]
AS

-- Populate columns with basic information
UPDATE ##nbn_exchange_obs
SET  SurveyKey = SURVEY_EVENT.SURVEY_KEY,
     SampleKey = SAMPLE.SAMPLE_KEY,
	 TaxonVersionKey = TAXON_LIST_ITEM.TAXON_VERSION_KEY,   
     DateType = SAMPLE.VAGUE_DATE_TYPE, 
     GridReference = SAMPLE.SPATIAL_REF, 
     Projection = SAMPLE.SPATIAL_REF_SYSTEM, 
     ZeroAbundance = CASE TAXON_OCCURRENCE.ZERO_ABUNDANCE WHEN 1 THEN 'T' ELSE 'F' END, 
     Sensitive = CASE TAXON_OCCURRENCE.CONFIDENTIAL WHEN 1 THEN 'T' ELSE 'F' END
FROM         TAXON_OCCURRENCE INNER JOIN
                      TAXON_DETERMINATION ON TAXON_OCCURRENCE.TAXON_OCCURRENCE_KEY = TAXON_DETERMINATION.TAXON_OCCURRENCE_KEY INNER JOIN
                      SAMPLE ON TAXON_OCCURRENCE.SAMPLE_KEY = SAMPLE.SAMPLE_KEY INNER JOIN
                      SURVEY_EVENT ON SAMPLE.SURVEY_EVENT_KEY = SURVEY_EVENT.SURVEY_EVENT_KEY INNER JOIN
                      TAXON_LIST_ITEM ON TAXON_DETERMINATION.TAXON_LIST_ITEM_KEY = TAXON_LIST_ITEM.TAXON_LIST_ITEM_KEY
WHERE     (TAXON_DETERMINATION.PREFERRED = 1) AND (##nbn_exchange_obs.RecordKey=TAXON_OCCURRENCE.TAXON_OCCURRENCE_KEY);

-- *********
-- Site Name
-- *********
-- Get the SiteName from the LOCATION_NAME where we can
UPDATE ##nbn_exchange_obs 
SET SiteName = SAMPLE.LOCATION_NAME
FROM ##nbn_exchange_obs INNER JOIN
SAMPLE ON ##nbn_exchange_obs.SampleKey = SAMPLE.SAMPLE_KEY;

-- If it is still null, try getting it from the SAMPLE.LOCATION_KEY
UPDATE ##nbn_exchange_obs 
SET SiteName = LOCATION_NAME.ITEM_NAME,
    SiteKey = SAMPLE.LOCATION_KEY 
FROM ##nbn_exchange_obs INNER JOIN
SAMPLE ON ##nbn_exchange_obs.SampleKey = SAMPLE.SAMPLE_KEY
INNER JOIN LOCATION_NAME ON SAMPLE.LOCATION_KEY = LOCATION_NAME.LOCATION_KEY AND LOCATION_NAME.PREFERRED = 1
WHERE SiteName Is Null OR LEN(SiteName)=0;

-- Finally, try getting it from the SURVEY_EVENT.LOCATION_KEY
UPDATE ##nbn_exchange_obs 
SET SiteName = LOCATION_NAME.ITEM_NAME,
    SiteKey = SURVEY_EVENT.LOCATION_KEY
FROM ##nbn_exchange_obs INNER JOIN
SAMPLE ON ##nbn_exchange_obs.SampleKey = SAMPLE.SAMPLE_KEY
INNER JOIN SURVEY_EVENT ON SAMPLE.SURVEY_EVENT_KEY = SURVEY_EVENT.SURVEY_EVENT_KEY
INNER JOIN LOCATION_NAME ON SURVEY_EVENT.LOCATION_KEY = LOCATION_NAME.LOCATION_KEY AND LOCATION_NAME.PREFERRED = 1
WHERE SiteName Is Null OR LEN(SiteName)=0;

-- ***************
-- Grid references
-- ***************
-- Set the Precision field
UPDATE ##nbn_exchange_obs
SET [Precision] = CASE Len(GridReference)
	WHEN 4 THEN 10000
    WHEN 5 THEN  2000
    WHEN 6 THEN  1000
    WHEN 8 THEN   100
    WHEN 10 THEN   10
    WHEN 12 THEN    1
    ELSE 0
END
WHERE Projection='OSGB';

UPDATE ##nbn_exchange_obs
SET [Precision] = CASE Len(GridReference)
	WHEN 3 THEN 10000
    WHEN 4 THEN  2000
    WHEN 5 THEN  1000
    WHEN 7 THEN   100
    WHEN 9 THEN   10
    WHEN 11 THEN    1
    ELSE 0
END
WHERE Projection='OSNI';

-- In case where the Projection is not OSNI or OSGB, store the lat/long
UPDATE ##nbn_exchange_obs 
SET Projection = 'OSGB36', 
	GridReference = Null, 
	East = SAMPLE.[LONG], 
	North = SAMPLE.[LAT], 
	[Precision] = 100
FROM ##nbn_exchange_obs INNER JOIN SAMPLE 
ON ##nbn_exchange_obs.SampleKey = SAMPLE.SAMPLE_KEY 
WHERE not (Projection='OSNI' Or Projection='OSGB');

-- =======
-- Dates
-- =======
--
-- Update StartDate and EndDate with dates in string format
-- in rows where the dates are not null
--
-- *********
-- StartDate
-- *********
UPDATE ##nbn_exchange_obs 
SET StartDate = dbo.nbn_exchange_date_to_string(SAMPLE.VAGUE_DATE_START)
FROM ##nbn_exchange_obs INNER JOIN
SAMPLE ON ##nbn_exchange_obs.SampleKey = SAMPLE.SAMPLE_KEY
WHERE SAMPLE.VAGUE_DATE_START is not null;

-- *********
-- EndDate
-- *********
UPDATE ##nbn_exchange_obs 
SET EndDate = dbo.nbn_exchange_date_to_string(SAMPLE.VAGUE_DATE_END)
FROM ##nbn_exchange_obs INNER JOIN
SAMPLE ON ##nbn_exchange_obs.SampleKey = SAMPLE.SAMPLE_KEY
WHERE SAMPLE.VAGUE_DATE_END is not null;

-- Make sure nulls are set correctly according to DateType
UPDATE ##nbn_exchange_obs
SET	EndDate = Null
WHERE DateType='U' Or DateType='Y-';

UPDATE ##nbn_exchange_obs
SET	StartDate = Null
WHERE DateType='U' Or DateType='-Y';

UPDATE ##nbn_exchange_obs
SET	EndDate = Null,
    StartDate = Null,
	DateType = 'U'
WHERE DateType Is Null;
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.nbn_exchange_basic_update') AND SysStat & 0xf = 4)
BEGIN
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.nbn_exchange_basic_update TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.nbn_exchange_basic_update TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.nbn_exchange_basic_update TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.nbn_exchange_basic_update TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.nbn_exchange_basic_update TO [R2k_RecordCardsOnly]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[nbn_exchange_create_tables]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[nbn_exchange_create_tables]
GO

/*===========================================================================*\
  Author  Stuart Ball, JNCC
  Date    16 May 2008

  Description	
     Create the tables needed to support export to NBN exchange format.

  Parameters
     None.
	
  Revision information

\*===========================================================================*/
CREATE PROCEDURE [dbo].[nbn_exchange_create_tables]
AS

-- Table in which to assemble the rows to export
CREATE TABLE [##nbn_exchange_obs]
(
	[RecordKey] [varchar](16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL,
	[SurveyKey] [varchar](16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
	[SampleKey] [varchar](16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
	[StartDate] [varchar](12) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
	[EndDate] [varchar](12) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
	[DateType] [char](2) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
	[TaxonVersionKey] [varchar](16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
	[ZeroAbundance] [char](1) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
	[Sensitive] [char](1) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
	[SiteKey] [varchar](16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
	[SiteName] [varchar](255) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
	[Projection] [varchar](10) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
	[GridReference] [varchar](30) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
	[Precision] [int] NULL,
	[East] [float] NULL,
	[North] [float] NULL,
	[Recorder] [varchar](255) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
	[Determiner] [varchar](255) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
	[SampleMethod] [varchar](50) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
	[Comment] [varchar](1024) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
	[Substrate] [varchar](20) COLLATE SQL_Latin1_General_CP1_CI_AS NULL
)
-- make RecordKey the primary key
ALTER TABLE [##nbn_exchange_obs] 
ADD CONSTRAINT pk_RecordKey PRIMARY KEY (RecordKey)

-- table containing the key(s) selected by the user in Rec6
CREATE TABLE [##nbn_exchange_export]
(
	[table_name] [varchar](30) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL,
	[nbn_key] [varchar](16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL
) 

-- results of the validation check before the export
-- rows identified here should be excluded from the export
CREATE TABLE [##nbn_exchange_invalid]
(
	[table_name] [varchar](30) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL,
	[nbn_key] [varchar](16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL
) 
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.nbn_exchange_create_tables') AND SysStat & 0xf = 4)
BEGIN
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.nbn_exchange_create_tables TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.nbn_exchange_create_tables TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.nbn_exchange_create_tables TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.nbn_exchange_create_tables TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.nbn_exchange_create_tables TO [R2k_RecordCardsOnly]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[nbn_exchange_drop_tables]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[nbn_exchange_drop_tables]
GO

/*===========================================================================*\
  Author  Stuart Ball, JNCC
  Date    16 May 2008

  Description	
     Drop the tables used to support export to NBN exchange format.

  Parameters
     None.
	
  Revision information

\*===========================================================================*/
CREATE PROCEDURE [dbo].[nbn_exchange_drop_tables]
AS

DROP TABLE [##nbn_exchange_obs]

DROP TABLE [##nbn_exchange_export]

DROP TABLE [##nbn_exchange_invalid]
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.nbn_exchange_drop_tables') AND SysStat & 0xf = 4)
BEGIN
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.nbn_exchange_drop_tables TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.nbn_exchange_drop_tables TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.nbn_exchange_drop_tables TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.nbn_exchange_drop_tables TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.nbn_exchange_drop_tables TO [R2k_RecordCardsOnly]
END
GO


/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[nbn_exchange_get_obs]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[nbn_exchange_get_obs]
GO

/*===========================================================================*\
  Author  Stuart Ball, JNCC
  Date    20 May 2008

  Description	
     Populate the nbn_exchange_obs with TAXON_OCCURRENCE_KEYs
     This will depend upon the type of data that the user selected to report
     so we need to do a series of queries for each table type in nbn_exchange_export.
     We then need to remove any invalid rows linked to the entries in nbn_exchange_invalid.
     Again, the way we do this depends upon the type of record that failed validation.

  Parameters
     ZeroAbundance - user want's to include ZERO_ABUNDANCE records 0/1.
     Sensitive - user wants ti include CONFIDENTIAL records 0/1.
	
  Revision information
  SGB 16/01/2009  Added DELETE query at the end to remove rows where the
                  TAXON_VERSION entry  is not system supplied

\*===========================================================================*/
CREATE PROCEDURE [dbo].[nbn_exchange_get_obs]
	@ZeroAbundance int =0,
	@Sensitive int =0
AS
DECLARE @aTable varchar(30)
DECLARE @MyCursor CURSOR

-- get the tables from nbn_exchange_export
SET @MyCursor = CURSOR FAST_FORWARD
FOR Select table_name From ##nbn_exchange_export GROUP BY table_name

-- we go through each table in turn adding TAXON_OCCURRENCE_KEYs to nbn_exchange_obs
OPEN @MyCursor
FETCH NEXT FROM @MyCursor
INTO @aTable

WHILE @@FETCH_STATUS = 0
BEGIN
	IF @aTable = 'SURVEY'
    BEGIN
		INSERT INTO ##nbn_exchange_obs (RecordKey)
		SELECT TAXON_OCCURRENCE.TAXON_OCCURRENCE_KEY
		FROM   SURVEY_EVENT INNER JOIN
               SURVEY ON SURVEY_EVENT.SURVEY_KEY = SURVEY.SURVEY_KEY INNER JOIN
               SAMPLE ON SURVEY_EVENT.SURVEY_EVENT_KEY = SAMPLE.SURVEY_EVENT_KEY INNER JOIN
               TAXON_OCCURRENCE ON SAMPLE.SAMPLE_KEY = TAXON_OCCURRENCE.SAMPLE_KEY INNER JOIN
               ##nbn_exchange_export ON SURVEY.SURVEY_KEY = ##nbn_exchange_export.nbn_key
		WHERE  (##nbn_exchange_export.table_name = 'SURVEY') AND (TAXON_OCCURRENCE.ZERO_ABUNDANCE = 0 or TAXON_OCCURRENCE.ZERO_ABUNDANCE = @ZeroAbundance) AND 
               (TAXON_OCCURRENCE.CONFIDENTIAL = 0 or TAXON_OCCURRENCE.CONFIDENTIAL = @Sensitive) AND (TAXON_OCCURRENCE.VERIFIED <> 1) AND (TAXON_OCCURRENCE.CHECKED = 1)	
    END
	ELSE
	IF @aTable = 'SURVEY_EVENT'
	BEGIN
		INSERT INTO ##nbn_exchange_obs (RecordKey)
		SELECT TAXON_OCCURRENCE.TAXON_OCCURRENCE_KEY
		FROM   SURVEY_EVENT INNER JOIN
               SAMPLE ON SURVEY_EVENT.SURVEY_EVENT_KEY = SAMPLE.SURVEY_EVENT_KEY INNER JOIN
               TAXON_OCCURRENCE ON SAMPLE.SAMPLE_KEY = TAXON_OCCURRENCE.SAMPLE_KEY INNER JOIN
               ##nbn_exchange_export ON SURVEY_EVENT.SURVEY_EVENT_KEY = ##nbn_exchange_export.nbn_key
		WHERE  (##nbn_exchange_export.table_name = 'SURVEY_EVENT') AND (TAXON_OCCURRENCE.ZERO_ABUNDANCE = 0 or TAXON_OCCURRENCE.ZERO_ABUNDANCE = @ZeroAbundance) AND 
               (TAXON_OCCURRENCE.CONFIDENTIAL = 0 or TAXON_OCCURRENCE.CONFIDENTIAL = @Sensitive) AND (TAXON_OCCURRENCE.VERIFIED <> 1) AND (TAXON_OCCURRENCE.CHECKED = 1)	
	END
	ELSE
	IF @aTable = 'SAMPLE'
	BEGIN
		INSERT INTO ##nbn_exchange_obs (RecordKey)
		SELECT TAXON_OCCURRENCE.TAXON_OCCURRENCE_KEY
		FROM   SAMPLE INNER JOIN
               TAXON_OCCURRENCE ON SAMPLE.SAMPLE_KEY = TAXON_OCCURRENCE.SAMPLE_KEY INNER JOIN
               ##nbn_exchange_export ON SAMPLE.SAMPLE_KEY = ##nbn_exchange_export.nbn_key
		WHERE  (##nbn_exchange_export.table_name = 'SAMPLE') AND (TAXON_OCCURRENCE.ZERO_ABUNDANCE = 0 or TAXON_OCCURRENCE.ZERO_ABUNDANCE = @ZeroAbundance) AND 
               (TAXON_OCCURRENCE.CONFIDENTIAL = 0 or TAXON_OCCURRENCE.CONFIDENTIAL = @Sensitive) AND (TAXON_OCCURRENCE.VERIFIED <> 1) AND (TAXON_OCCURRENCE.CHECKED = 1)
	END
	ELSE
	IF @aTable = 'TAXON_OCCURRENCE'
	BEGIN
		INSERT INTO ##nbn_exchange_obs (RecordKey)
		SELECT TAXON_OCCURRENCE.TAXON_OCCURRENCE_KEY
		FROM   TAXON_OCCURRENCE INNER JOIN
               ##nbn_exchange_export ON 
               TAXON_OCCURRENCE.TAXON_OCCURRENCE_KEY = ##nbn_exchange_export.nbn_key
		WHERE  (##nbn_exchange_export.table_name = 'TAXON_OCCURRENCE') AND (TAXON_OCCURRENCE.ZERO_ABUNDANCE = 0 or TAXON_OCCURRENCE.ZERO_ABUNDANCE = @ZeroAbundance) AND 
               (TAXON_OCCURRENCE.CONFIDENTIAL = 0 or TAXON_OCCURRENCE.CONFIDENTIAL = @Sensitive) AND (TAXON_OCCURRENCE.VERIFIED <> 1) AND (TAXON_OCCURRENCE.CHECKED = 1)
	END
	ELSE
	IF @aTable = 'TAXON_LIST_ITEM'
	BEGIN
		INSERT INTO ##nbn_exchange_obs (RecordKey)
		SELECT TAXON_OCCURRENCE.TAXON_OCCURRENCE_KEY
		FROM   TAXON_OCCURRENCE INNER JOIN
               TAXON_DETERMINATION ON TAXON_OCCURRENCE.TAXON_OCCURRENCE_KEY = TAXON_DETERMINATION.TAXON_OCCURRENCE_KEY INNER JOIN
               TAXON_LIST_ITEM AS TAXON_LIST_ITEM_1 ON 
               TAXON_DETERMINATION.TAXON_LIST_ITEM_KEY = TAXON_LIST_ITEM_1.TAXON_LIST_ITEM_KEY AND 
               TAXON_DETERMINATION.TAXON_LIST_ITEM_KEY = TAXON_LIST_ITEM_1.TAXON_LIST_ITEM_KEY INNER JOIN
               ##nbn_exchange_export INNER JOIN
               TAXON_LIST_ITEM ON ##nbn_exchange_export.nbn_key = TAXON_LIST_ITEM.TAXON_LIST_ITEM_KEY INNER JOIN
               NAMESERVER ON TAXON_LIST_ITEM.TAXON_VERSION_KEY = NAMESERVER.RECOMMENDED_TAXON_VERSION_KEY ON 
               TAXON_LIST_ITEM_1.TAXON_VERSION_KEY = NAMESERVER.INPUT_TAXON_VERSION_KEY
		WHERE  (##nbn_exchange_export.table_name = 'TAXON_LIST_ITEM') AND (TAXON_OCCURRENCE.ZERO_ABUNDANCE = 0 or TAXON_OCCURRENCE.ZERO_ABUNDANCE = @ZeroAbundance) AND 
               (TAXON_OCCURRENCE.CONFIDENTIAL = 0 or TAXON_OCCURRENCE.CONFIDENTIAL = @Sensitive) AND (TAXON_OCCURRENCE.VERIFIED <> 1) AND (TAXON_OCCURRENCE.CHECKED = 1)
	END
	ELSE
	IF @aTable = 'NAME'
	BEGIN
		INSERT INTO ##nbn_exchange_obs (RecordKey)
		SELECT TAXON_OCCURRENCE.TAXON_OCCURRENCE_KEY
		FROM   SAMPLE INNER JOIN
               TAXON_OCCURRENCE ON SAMPLE.SAMPLE_KEY = TAXON_OCCURRENCE.SAMPLE_KEY INNER JOIN
               SAMPLE_RECORDER ON SAMPLE.SAMPLE_KEY = SAMPLE_RECORDER.SAMPLE_KEY INNER JOIN
               SURVEY_EVENT_RECORDER ON SAMPLE_RECORDER.SE_RECORDER_KEY = SURVEY_EVENT_RECORDER.SE_RECORDER_KEY AND 
               SAMPLE_RECORDER.SE_RECORDER_KEY = SURVEY_EVENT_RECORDER.SE_RECORDER_KEY INNER JOIN
               ##nbn_exchange_export ON SURVEY_EVENT_RECORDER.NAME_KEY = ##nbn_exchange_export.nbn_key
		WHERE  (##nbn_exchange_export.table_name = 'NAME') AND (TAXON_OCCURRENCE.ZERO_ABUNDANCE = 0 or TAXON_OCCURRENCE.ZERO_ABUNDANCE = @ZeroAbundance) AND 
               (TAXON_OCCURRENCE.CONFIDENTIAL = 0 or TAXON_OCCURRENCE.CONFIDENTIAL = @Sensitive) AND (TAXON_OCCURRENCE.VERIFIED <> 1) AND (TAXON_OCCURRENCE.CHECKED = 1)
	END
	ELSE
	IF @aTable = 'LOCATION'
	BEGIN
		INSERT INTO ##nbn_exchange_obs (RecordKey)
		SELECT TAXON_OCCURRENCE.TAXON_OCCURRENCE_KEY
		FROM   ##nbn_exchange_export INNER JOIN
               SAMPLE INNER JOIN
               TAXON_OCCURRENCE ON SAMPLE.SAMPLE_KEY = TAXON_OCCURRENCE.SAMPLE_KEY ON 
               ##nbn_exchange_export.nbn_key = SAMPLE.LOCATION_KEY
		WHERE  (##nbn_exchange_export.table_name = 'LOCATION') AND (TAXON_OCCURRENCE.ZERO_ABUNDANCE = 0 or TAXON_OCCURRENCE.ZERO_ABUNDANCE = @ZeroAbundance) AND 
               (TAXON_OCCURRENCE.CONFIDENTIAL = 0 or TAXON_OCCURRENCE.CONFIDENTIAL = @Sensitive) AND (TAXON_OCCURRENCE.VERIFIED <> 1) AND (TAXON_OCCURRENCE.CHECKED = 1)
	END
	ELSE
	IF @aTable = 'REFERENCE'
	BEGIN
		INSERT INTO ##nbn_exchange_obs (RecordKey)
		SELECT TAXON_OCCURRENCE.TAXON_OCCURRENCE_KEY
		FROM   TAXON_OCCURRENCE_SOURCES INNER JOIN
               TAXON_OCCURRENCE ON 
               TAXON_OCCURRENCE_SOURCES.TAXON_OCCURRENCE_KEY = TAXON_OCCURRENCE.TAXON_OCCURRENCE_KEY INNER JOIN
               ##nbn_exchange_export ON 
               TAXON_OCCURRENCE_SOURCES.SOURCE_KEY = ##nbn_exchange_export.nbn_key
		WHERE  (##nbn_exchange_export.table_name = 'REFERENCE') AND (TAXON_OCCURRENCE.ZERO_ABUNDANCE = 0 or TAXON_OCCURRENCE.ZERO_ABUNDANCE = @ZeroAbundance) AND 
               (TAXON_OCCURRENCE.CONFIDENTIAL = 0 or TAXON_OCCURRENCE.CONFIDENTIAL = @Sensitive) AND (TAXON_OCCURRENCE.VERIFIED <> 1) AND (TAXON_OCCURRENCE.CHECKED = 1)
	END

FETCH NEXT FROM @MyCursor
INTO @aTable
END

CLOSE @MyCursor
DEALLOCATE @MyCursor

-- *******************
-- DELETE INVALID ROWS
-- *******************
-- Now we need to do more or less the same thing again to delete anything that
-- was flagged as invalid before the export, i.e. rows in nbn_exchange_invalid

-- get the tables from nbn_exchange_invalid
SET @MyCursor = CURSOR FAST_FORWARD
FOR Select table_name From ##nbn_exchange_invalid GROUP BY table_name

-- we go through each table in turn deleting corresponding TAXON_OCCURRENCE_KEYs 
-- from nbn_exchange_obs
OPEN @MyCursor
FETCH NEXT FROM @MyCursor
INTO @aTable

WHILE @@FETCH_STATUS = 0
BEGIN
	IF @aTable = 'TAXON_OCCURRENCE'
	BEGIN
		DELETE FROM ##nbn_exchange_obs
		FROM   ##nbn_exchange_obs INNER JOIN
               ##nbn_exchange_invalid ON ##nbn_exchange_obs.RecordKey = ##nbn_exchange_invalid.nbn_key
		WHERE  ##nbn_exchange_invalid.table_name = 'TAXON_OCCURRENCE'
	END

	IF @aTable = 'SAMPLE'
	BEGIN
		DELETE FROM ##nbn_exchange_obs
		FROM   ##nbn_exchange_obs INNER JOIN
               TAXON_OCCURRENCE ON TAXON_OCCURRENCE.TAXON_OCCURRENCE_KEY = ##nbn_exchange_obs.RecordKey INNER JOIN
               ##nbn_exchange_invalid ON  TAXON_OCCURRENCE.SAMPLE_KEY = ##nbn_exchange_invalid.nbn_key
		WHERE  ##nbn_exchange_invalid.table_name = 'SAMPLE'
	END

	IF @aTable = 'SURVEY_EVENT'
	BEGIN
		DELETE FROM ##nbn_exchange_obs
		FROM   ##nbn_exchange_obs INNER JOIN
               TAXON_OCCURRENCE ON TAXON_OCCURRENCE.TAXON_OCCURRENCE_KEY = ##nbn_exchange_obs.RecordKey INNER JOIN
               SAMPLE ON SAMPLE.SAMPLE_KEY = TAXON_OCCURRENCE.SAMPLE_KEY INNER JOIN
               ##nbn_exchange_invalid ON SAMPLE.SURVEY_EVENT_KEY  = ##nbn_exchange_invalid.nbn_key
		WHERE  ##nbn_exchange_invalid.table_name = 'SURVEY_EVENT'
	END

FETCH NEXT FROM @MyCursor
INTO @aTable
END

CLOSE @MyCursor
DEALLOCATE @MyCursor

-- finally, delete keys where the TAXON_VERSION is not system supplied
DELETE FROM ##nbn_exchange_obs
FROM   ##nbn_exchange_obs INNER JOIN
       TAXON_OCCURRENCE ON TAXON_OCCURRENCE.TAXON_OCCURRENCE_KEY = ##nbn_exchange_obs.RecordKey INNER JOIN
       TAXON_DETERMINATION ON TAXON_OCCURRENCE.TAXON_OCCURRENCE_KEY = TAXON_DETERMINATION.TAXON_OCCURRENCE_KEY INNER JOIN
       TAXON_LIST_ITEM ON TAXON_DETERMINATION.TAXON_LIST_ITEM_KEY = TAXON_LIST_ITEM.TAXON_LIST_ITEM_KEY INNER JOIN
       TAXON_VERSION ON TAXON_LIST_ITEM.TAXON_VERSION_KEY = TAXON_VERSION.TAXON_VERSION_KEY
WHERE (TAXON_VERSION.SYSTEM_SUPPLIED_DATA = 0)

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.nbn_exchange_get_obs') AND SysStat & 0xf = 4)
BEGIN
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.nbn_exchange_get_obs TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.nbn_exchange_get_obs TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.nbn_exchange_get_obs TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.nbn_exchange_get_obs TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.nbn_exchange_get_obs TO [R2k_RecordCardsOnly]
END
GO

/*===========================================================================*\
  Drop function before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[nbn_exchange_date_to_string]')
	   AND 	  Type = 'FN')
    DROP FUNCTION [dbo].[nbn_exchange_date_to_string]
GO


CREATE FUNCTION [dbo].[nbn_exchange_date_to_string](@IntDate int)
RETURNS varchar(11)
AS
BEGIN

--****************************************************************************************************
--constants
declare @D1 int
set @D1=365 							-- days in 1 year
declare @D4 int
set @D4 = @D1 * 4 + 1			-- days in 4 years, inc leap year
declare @D100 int
set @D100 = @D4 * 25 - 1 	-- days in 100 years (no leap year on 100th year)
declare @D400 int
set @D400 = @D100 * 4 + 1	-- days in 400 years - every 400 years you do  get a leap year
--variables
declare @T int
declare @Y int
declare @M int
declare @D int
declare @I int
declare @L int
declare @ds as varchar(11)
-- Leap year
set @L = 0
-- get number of days since 1/1/01 
set @T = @IntDate+693593
-- find number of whole 400 year blocks
set @Y = @T / @D400
set @T = @T - @Y * @D400
set @Y = @Y * 400 + 1
set @I = @T / @D100
set @D = @T - @I * @D100
if @I=4 begin
  set @I = @I - 1
  set @D = @D + @D100
end
set @Y = @Y + @I * 100
set @I = @D / @D4
set @D = @D - @I * @D4
set @Y = @Y + @I * 4
set @I = @D / @D1
set @D = @D - @I * @D1
if @I = 4 begin
  set @I = @I - 1
  set @D = @D + @D1
  set @L = 1
end
set @Y=@Y + @I
set @M=1
while 1=1 begin
	set @I = case @M
		when 1 then 31
		when 2 then 28 + @L
		when 3 then 31
		when 4 then 30
  		when 5 then 31
		when 6 then 30
		when 7 then 31
		when 8 then 31
		when 9 then 30
		when 10 then 31
		when 11 then 30
		when 12 then 31
	end
	if @D<@I break
	set @D = @D - @I
	set @M = @M + 1
end

set @ds = Right(Replicate('0',3)+Convert(varchar(11),@D+1),2) + '/' + Right(Replicate('0',3)+Convert(varchar(11),@M),2) + '/' + Right(Replicate('0',4)+Convert(varchar(11),@Y),4)
return @ds

END
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.nbn_exchange_date_to_string') AND Type = 'FN')
BEGIN
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.nbn_exchange_date_to_string TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.nbn_exchange_date_to_string TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.nbn_exchange_date_to_string TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.nbn_exchange_date_to_string TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.nbn_exchange_date_to_string TO [R2k_RecordCardsOnly]
END
GO

/*===========================================================================*\
  Drop function before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[nbn_exchange_strip_LFCR]')
	   AND 	  Type = 'FN')
    DROP FUNCTION [dbo].[nbn_exchange_strip_LFCR]
GO


CREATE FUNCTION [dbo].[nbn_exchange_strip_LFCR](@text varchar(8000))
RETURNS varchar(8000)
AS
BEGIN
	DECLARE @stuff varchar(8000)
	SET @stuff = ''
	DECLARE @i int
	SET @i = 1

	IF (LEN(@text) > 0)
	BEGIN
        WHILE @i <= LEN(@text)
		BEGIN
			IF NOT ((SUBSTRING(@text, @i, 1) = CHAR(13)) OR (SUBSTRING(@text, @i, 1) = CHAR(10)))
				SET @stuff = @stuff + SUBSTRING(@text, @i, 1)
			SET @i = @i + 1
		END
		SET @stuff = RTRIM(LTRIM(@stuff))
	END
	RETURN @stuff

END
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.nbn_exchange_strip_LFCR') AND Type = 'FN')
BEGIN
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.nbn_exchange_strip_LFCR TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.nbn_exchange_strip_LFCR TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.nbn_exchange_strip_LFCR TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.nbn_exchange_strip_LFCR TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.nbn_exchange_strip_LFCR TO [R2k_RecordCardsOnly]
END





GO
/****** Object:  StoredProcedure [dbo].[usp_IndexTaxonName_ApplyNameServer]    Script Date: 11/02/2013 21:44:04 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
/*===========================================================================*\
  Description: Applies the NameServer information to the Index_Taxon_Name
		Recommended_Taxon_List_Item_Key table.  Updates all records where this value
		is null.

  Parameters:	None

  Created:	November 2004

  Last revision information:
    $Revision: 4 $
    $Date: 1/11/13 11:35 $
    $Author: Johnvanbreda $

\*=========================================================================== */
ALTER PROCEDURE [dbo].[usp_IndexTaxonName_ApplyNameServer]
AS
/* Remove any disconnected index_taxon_name records */
DELETE ITN 
FROM Index_Taxon_Name ITN
LEFT JOIN Taxon_List_Item TLI ON TLI.Taxon_List_Item_Key=ITN.Taxon_List_Item_Key
WHERE TLI.Taxon_List_Item_Key IS NULL

UPDATE ITN
SET ITN.RECOMMENDED_TAXON_LIST_ITEM_KEY = NS.RECOMMENDED_TAXON_LIST_ITEM_KEY
FROM NAMESERVER NS
INNER JOIN TAXON_LIST_ITEM TLI ON NS.INPUT_TAXON_VERSION_KEY = TLI.TAXON_VERSION_KEY
INNER JOIN INDEX_TAXON_NAME ITN ON ITN.TAXON_LIST_ITEM_KEY = TLI.TAXON_LIST_ITEM_KEY
WHERE ITN.RECOMMENDED_TAXON_LIST_ITEM_KEY IS NULL 

UPDATE ITN
SET ITN.RECOMMENDED_TAXON_LIST_ITEM_KEY = TLI.PREFERRED_NAME
FROM TAXON_LIST TL 
INNER JOIN TAXON_LIST_VERSION TLV ON TL.TAXON_LIST_KEY = TLV.TAXON_LIST_KEY
INNER JOIN TAXON_LIST_ITEM TLI ON TLV.TAXON_LIST_VERSION_KEY = TLI.TAXON_LIST_VERSION_KEY
INNER JOIN TAXON_LIST_ITEM TLI1 ON TLI.TAXON_VERSION_KEY = TLI1.TAXON_VERSION_KEY
INNER JOIN INDEX_TAXON_NAME ITN ON TLI1.TAXON_LIST_ITEM_KEY = ITN.TAXON_LIST_ITEM_KEY
WHERE TL.PREFERRED=1 AND ITN.RECOMMENDED_TAXON_LIST_ITEM_KEY IS NULL

UPDATE ITN
SET ITN.RECOMMENDED_TAXON_LIST_ITEM_KEY = TLI.PREFERRED_NAME
FROM TAXON_VERSION TV
INNER JOIN TAXON_LIST_ITEM TLI ON TV.TAXON_VERSION_KEY = TLI.TAXON_VERSION_KEY
INNER JOIN TAXON_GROUP TG ON TV.Output_group_key = TG.TAXON_GROUP_KEY
INNER JOIN TAXON_LIST_VERSION TLV ON TLV.TAXON_LIST_KEY=TG.USE_TAXON_LIST_KEY
		AND TLI.TAXON_LIST_VERSION_KEY=TLV.TAXON_LIST_VERSION_KEY
INNER JOIN TAXON_LIST_ITEM AS TLI1 
		ON TLI.TAXON_VERSION_KEY = TLI1.TAXON_VERSION_KEY
INNER JOIN INDEX_TAXON_NAME ITN ON TLI1.TAXON_LIST_ITEM_KEY = ITN.TAXON_LIST_ITEM_KEY
WHERE ITN.RECOMMENDED_TAXON_LIST_ITEM_KEY IS NULL

UPDATE ITN
SET ITN.RECOMMENDED_TAXON_LIST_ITEM_KEY = TLI.PREFERRED_NAME
FROM INDEX_TAXON_NAME ITN
INNER JOIN TAXON_LIST_ITEM TLI ON TLI.TAXON_LIST_ITEM_KEY=ITN.TAXON_LIST_ITEM_KEY
INNER JOIN TAXON_LIST_ITEM TLI2 on TLI2.TAXON_LIST_ITEM_KEY=TLI.PREFERRED_NAME
WHERE ITN.RECOMMENDED_TAXON_LIST_ITEM_KEY IS NULL

--Now set up the recommended sort orders, which depend on the recommended names

UPDATE ITN
SET ITN.Sort_Order=
	LEFT('000', 3 - LEN(CONVERT(VARCHAR(3), ISNULL(TG.Sort_Order,0))))
  + CONVERT(VARCHAR(3), ISNULL(TG.Sort_Order,0)) 
	+ LEFT('00000000', 8 - LEN(CONVERT(VARCHAR(8), ISNULL(TLI.Sort_Code,0))))
  + CONVERT(VARCHAR(8), ISNULL(TLI.Sort_Code,0)) 
FROM Index_Taxon_Name ITN 
INNER JOIN Taxon_List_Item TLI ON TLI.Taxon_List_Item_Key=ITN.Recommended_Taxon_List_Item_Key
INNER JOIN Taxon_Version TV ON TV.Taxon_Version_Key=TLI.Taxon_Version_Key
LEFT JOIN Taxon_Group TG ON TG.Taxon_Group_Key=TV.Output_Group_Key


-- Rebuild the lineage and sort order on the Organism table

EXECUTE [dbo].[spPopulateOrganismLineage]


-- If there is anything in the Organism table and the Sort_Method not there or set to Recommended.

UPDATE Index_Taxon_Name 
  SET SORT_ORDER = ORG.SORT_ORDER
  FROM INDEX_TAXON_NAME ITN 
  INNER JOIN TAXON_LIST_ITEM TLI ON TLI.TAXON_LIST_ITEM_KEY = ITN.RECOMMENDED_TAXON_LIST_ITEM_KEY
  INNER JOIN ORGANISM ORG ON ORG.TAXON_VERSION_KEY = TLI.TAXON_VERSION_KEY
  WHERE NOT EXISTS (SELECT * FROM SETTING WHERE [NAME]  = 'SortMethod' AND [DATA] = 'Recommended')

-- If there is anything in the Organism table, then populate Index_Taxon_Hierarchy
 
EXECUTE [dbo].[usp_Populate_Index_Taxon_Hierarchy]
