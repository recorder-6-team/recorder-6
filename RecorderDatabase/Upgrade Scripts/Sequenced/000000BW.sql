/**********Implements a number of minor changes identified during 
testing in live situation. Also fixes issues identified with SQL Server 2016
With matching stops the notes column being updated during matching. This 
stops the list being displayed ina differm=nt order each time there is change
and allowing the user to work down the list. 
*/

/****** Object:  StoredProcedure [dbo].[usp_IndexTaxonName_ApplySorts]    Script Date: 01/08/2019 19:09:05 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
/*===========================================================================*\
  Description: Applies the sorting and other information which is related 
        to the Organism table.

  Parameters:	None

  Created:	September 2016
  
  Changed January 2019 to allow for TV's with deleted dates. 
  

\*=========================================================================== */
ALTER PROCEDURE [dbo].[usp_IndexTaxonName_ApplySorts]
AS

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


-- Rebuild the lineage and sort order on the Organism table - do this first because can_exapnd uses the Redundant Flag
-- which is upated in populate Organism lineage 

EXECUTE [dbo].[spPopulateOrganismLineage]

-- Set Can_Expand to True

Update Index_Taxon_Name set Can_Expand = 1


-- Poulate Can_Expand in ITN

EXECUTE [dbo].[usp_Populate_Can_Expand]


-- If there is anything in the Organism table and the Sort_Method not there or set to Recommended.

UPDATE Index_Taxon_Name 
  SET SORT_ORDER = ORG.SORT_ORDER
  FROM INDEX_TAXON_NAME ITN 
  INNER JOIN TAXON_LIST_ITEM TLI ON TLI.TAXON_LIST_ITEM_KEY = ITN.RECOMMENDED_TAXON_LIST_ITEM_KEY
  INNER JOIN ORGANISM ORG ON ORG.TAXON_VERSION_KEY = TLI.TAXON_VERSION_KEY
  WHERE NOT EXISTS (SELECT * FROM SETTING WHERE [NAME]  = 'SortMethod' AND [DATA] = 'Recommended')

-- If there is anything in the Organism table then set the allow data entry based on redundant flag 

UPDATE Index_Taxon_Name 
Set Allow_Data_Entry = 1  
FROM INDEX_TAXON_NAME ITN 
INNER JOIN TAXON_LIST_ITEM TLI ON TLI.TAXON_LIST_ITEM_KEY = ITN.RECOMMENDED_TAXON_LIST_ITEM_KEY
INNER JOIN ORGANISM ORG ON ORG.TAXON_VERSION_KEY = TLI.TAXON_VERSION_KEY
WHERE ITN.Allow_Data_Entry = 0 

UPDATE Index_Taxon_Name 
Set Allow_Data_Entry = 0  
FROM INDEX_TAXON_NAME ITN 
INNER JOIN TAXON_LIST_ITEM TLI ON TLI.TAXON_LIST_ITEM_KEY = ITN.RECOMMENDED_TAXON_LIST_ITEM_KEY
INNER JOIN ORGANISM ORG ON ORG.TAXON_VERSION_KEY = TLI.TAXON_VERSION_KEY
WHERE ORG.REDUNDANT_FLAG = 'Y' 

UPDATE Index_Taxon_Name 
Set Allow_Data_Entry = 0  
FROM INDEX_TAXON_NAME ITN 
INNER JOIN TAXON_VERSION TV ON 
TV.TAXON_VERSION_KEY = ITN.TAXON_VERSION_KEY  
WHERE TV.DATE_TO IS NOT NULL

UPDATE ITN
Set Allow_Data_Entry = 0  
FROM INDEX_TAXON_NAME ITN
INNER JOIN TAXON_LIST_ITEM TLI ON 
TLI.TAXON_LIST_ITEM_KEY = ITN.TAXON_LIST_ITEM_KEY
INNER JOIN  INDEX_TAXON_NAME ITN2
ON TLI.PREFERRED_NAME = ITN2.TAXON_LIST_ITEM_KEY
WHERE ITN2.RECOMMENDED_TAXON_VERSION_KEY <>
ITN.RECOMMENDED_TAXON_VERSION_KEY

DELETE FROM IW_MATCHED_SPECIES 
FROM IW_Matched_Species IMS 
INNER JOIN INDEX_TAXON_NAME ITN
ON ITN.TAXON_LIST_ITEM_KEY = IMS.Matched_Key
WHERE ITN.Allow_Data_Entry = 0  

GO
/****** Object:  StoredProcedure [dbo].[usp_IndexTaxonName_ApplyNameServer]    Script Date: 01/14/2019 19:43:39 ******/
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

  Last revision Jnauary 2019 
  following work with NHM to deal with deleted TVK and TLIK
  which changed the allocation of RTLIK's  in the 
  external R6 dictionary processing  

\*=========================================================================== */
ALTER PROCEDURE [dbo].[usp_IndexTaxonName_ApplyNameServer]
AS
/* Remove any disconnected index_taxon_name records */
DELETE ITN 
FROM Index_Taxon_Name ITN
LEFT JOIN Taxon_List_Item TLI ON TLI.Taxon_List_Item_Key=ITN.Taxon_List_Item_Key
WHERE TLI.Taxon_List_Item_Key IS NULL

/* Deal with any where Names Server has no entry. Should just be user
added taxa, but may be more if user dictionary is out of step. 
*/
UPDATE INDEX_TAXON_NAME SET RECOMMENDED_TAXON_LIST_ITEM_KEY
= TLI.PREFERRED_NAME, RECOMMENDED_TAXON_VERSION_KEY = 
INDEX_TAXON_NAME.TAXON_VERSION_KEY 
FROM  TAXON_LIST_ITEM TLI 
INNER JOIN INDEX_TAXON_NAME ON INDEX_TAXON_NAME.TAXON_LIST_ITEM_KEY 
= TLI.TAXON_LIST_ITEM_KEY 
WHERE  TLI.TAXON_LIST_ITEM_KEY = TLI.PREFERRED_NAME
AND NOT EXISTS (SELECT * FROM NAMESERVER NS WHERE 
NS.INPUT_TAXON_VERSION_KEY = INDEX_TAXON_NAME.TAXON_VERSION_KEY)

/* Deal with any where Names Server has an entry
*/


UPDATE INDEX_TAXON_NAME SET RECOMMENDED_TAXON_LIST_ITEM_KEY
= NS.RECOMMENDED_TAXON_LIST_ITEM_KEY,
RECOMMENDED_TAXON_VERSION_KEY = NS.RECOMMENDED_TAXON_VERSION_KEY
FROM NAMESERVER NS 
INNER JOIN INDEX_TAXON_NAME ON NS.INPUT_TAXON_VERSION_KEY 
= INDEX_TAXON_NAME.TAXON_VERSION_KEY 

/* Catch all just in case of user dictionary errors
*/
UPDATE INDEX_TAXON_NAME SET RECOMMENDED_TAXON_LIST_ITEM_KEY
= TAXON_LIST_ITEM_KEY, RECOMMENDED_TAXON_VERSION_KEY = TAXON_VERSION_KEY
FROM INDEX_TAXON_NAME 
WHERE RECOMMENDED_TAXON_LIST_ITEM_KEY IS NULL


GO
/****** Object:  StoredProcedure [dbo].[usp_IndexTaxonName_ApplyNameServer_SingleList]    Script Date: 01/14/2019 19:44:14 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

/*===========================================================================*\
  Description: Applies the NameServer information to the Index_Taxon_Name
		Recommended_Taxon_List_Item_Key table for just the supplied list key.

  Parameters:	None

  Created:	November 2004

  Last revision information:
    $Revision: 1 $
    $Date: 20/03/07 8:54 $
    $Author: Johnvanbreda $
    January 2019 to bring in line with [dbo].[usp_IndexTaxonName_ApplyNameServer
    Note that Name Server will not have an entry if this is user added 
    
\*=========================================================================== */
ALTER PROCEDURE [dbo].[usp_IndexTaxonName_ApplyNameServer_SingleList]
	@TLKey CHAR(16)
AS
/* Deal with any where Names Server has no entry
*/
UPDATE INDEX_TAXON_NAME SET RECOMMENDED_TAXON_LIST_ITEM_KEY
= TLI.PREFERRED_NAME, RECOMMENDED_TAXON_VERSION_KEY = 
TLI.TAXON_VERSION_KEY, PREFERRED_TAXA = 1, DEPRECATED = 0,
OUTPUT_TAXON_NAME = dbo.LCRemoveSubGenusText(INDEX_TAXON_NAME.ACTUAL_NAME),
TAXON_VERSION_KEY = TLI.TAXON_VERSION_KEY
FROM  TAXON_LIST_ITEM TLI 
INNER JOIN INDEX_TAXON_NAME ON INDEX_TAXON_NAME.TAXON_LIST_ITEM_KEY 
= TLI.TAXON_LIST_ITEM_KEY 
INNER JOIN TAXON_LIST_VERSION TLV
ON TLV.TAXON_LIST_VERSION_KEY =  INDEX_TAXON_NAME.TAXON_LIST_VERSION_KEY 
AND TLV.TAXON_LIST_VERSION_KEY = @TLKey
WHERE  TLI.TAXON_LIST_ITEM_KEY = TLI.PREFERRED_NAME
AND NOT EXISTS (SELECT * FROM NAMESERVER NS WHERE 
NS.INPUT_TAXON_VERSION_KEY = TLI.TAXON_VERSION_KEY)

/* Deal with any where Names Server has an entry 
*/

UPDATE INDEX_TAXON_NAME SET RECOMMENDED_TAXON_LIST_ITEM_KEY
= NS.RECOMMENDED_TAXON_LIST_ITEM_KEY 
FROM NAMESERVER NS 
INNER JOIN INDEX_TAXON_NAME ON NS.INPUT_TAXON_VERSION_KEY 
= INDEX_TAXON_NAME.TAXON_VERSION_KEY INNER JOIN TAXON_LIST_VERSION TLV
ON TLV.TAXON_LIST_VERSION_KEY =  INDEX_TAXON_NAME.TAXON_LIST_VERSION_KEY 
AND TLV.TAXON_LIST_VERSION_KEY = @TLKey


/* Catch All 
*/

UPDATE INDEX_TAXON_NAME SET RECOMMENDED_TAXON_LIST_ITEM_KEY
= TAXON_LIST_ITEM_KEY 
FROM INDEX_TAXON_NAME INNER JOIN TAXON_LIST_VERSION TLV
ON TLV.TAXON_LIST_VERSION_KEY =  INDEX_TAXON_NAME.TAXON_LIST_VERSION_KEY 
AND TLV.TAXON_LIST_VERSION_KEY = @TLKey
WHERE RECOMMENDED_TAXON_LIST_ITEM_KEY IS NULL 


--Now set up the recommended sort orders, which depend on the recommended names


UPDATE ITN
SET ITN.Sort_Order=
	LEFT('000', 3 - LEN(CONVERT(VARCHAR(3), ISNULL(TG.Sort_Order,0))))
  + CONVERT(VARCHAR(3), ISNULL(TG.Sort_Order,0)) 
	+ LEFT('00000000', 8 - LEN(CONVERT(VARCHAR(8), ISNULL(TLI.Sort_Code,0))))
  + CONVERT(VARCHAR(8), ISNULL(TLI.Sort_Code,0)) 
FROM Index_Taxon_Name ITN 
INNER JOIN Taxon_List_Item TLI 
	ON TLI.Taxon_List_Item_Key=ITN.Recommended_Taxon_List_Item_Key
INNER JOIN TAXON_LIST_VERSION TLV ON TLV.TAXON_LIST_VERSION_KEY=TLI.TAXON_LIST_VERSION_KEY
INNER JOIN Taxon_Version TV 
	ON TV.Taxon_Version_Key=TLI.Taxon_Version_Key
	AND TLV.Taxon_List_Key=@TLKey
LEFT JOIN Taxon_Group TG ON TG.Taxon_Group_Key=TV.Output_Group_Key

-- If there is anything in the Organism table, then it overwrites sort orders.

UPDATE Index_Taxon_Name 
  SET SORT_ORDER = ORG.SORT_ORDER
  FROM INDEX_TAXON_NAME ITN 
  INNER JOIN TAXON_LIST_ITEM TLI ON TLI.TAXON_LIST_ITEM_KEY = ITN.RECOMMENDED_TAXON_LIST_ITEM_KEY
  INNER JOIN TAXON_LIST_VERSION TLV 
    ON TLV.TAXON_LIST_VERSION_KEY=TLI.TAXON_LIST_VERSION_KEY
	AND TLV.Taxon_List_Key=@TLKey
  INNER JOIN ORGANISM ORG ON ORG.TAXON_VERSION_KEY = TLI.TAXON_VERSION_KEY


GO
/****** Object:  StoredProcedure [dbo].[usp_IndexTaxonName_ApplyNameServer_SingleRecord]    Script Date: 01/14/2019 19:44:58 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

/*===========================================================================*\
  Description: Applies the NameServer information to the Index_Taxon_Name
		Recommended_Taxon_List_Item_Key table and 
		other fields for for just the supplied key.

  Parameters:	None

  Created:	November 2004
  Updated November 2018 
  Januart 2019 to bring into line with usp_IndexTaxonName_ApplyNameServer
  Note that the information available when a single entry is amde is different 
  from that
  when the Index is fully rebuilt. 

\*=========================================================================== */
ALTER PROCEDURE [dbo].[usp_IndexTaxonName_ApplyNameServer_SingleRecord]
	@TLIKey CHAR(16)
AS
/* Deal with any where Names Server has no entry
*/
UPDATE INDEX_TAXON_NAME SET RECOMMENDED_TAXON_LIST_ITEM_KEY
= TLI.PREFERRED_NAME, RECOMMENDED_TAXON_VERSION_KEY = 
TLI.TAXON_VERSION_KEY, PREFERRED_TAXA = 1, DEPRECATED = 0,
OUTPUT_TAXON_NAME = dbo.LCRemoveSubGenusText(INDEX_TAXON_NAME.ACTUAL_NAME),
TAXON_VERSION_KEY = TLI.TAXON_VERSION_KEY
FROM  TAXON_LIST_ITEM TLI 
INNER JOIN INDEX_TAXON_NAME ON INDEX_TAXON_NAME.TAXON_LIST_ITEM_KEY 
= TLI.TAXON_LIST_ITEM_KEY 
WHERE  TLI.TAXON_LIST_ITEM_KEY = TLI.PREFERRED_NAME
AND NOT EXISTS (SELECT * FROM NAMESERVER NS WHERE 
NS.INPUT_TAXON_VERSION_KEY = TLI.TAXON_VERSION_KEY)
AND  INDEX_TAXON_NAME.TAXON_LIST_ITEM_KEY  = @TLIKey


/* Deal with any where Names Server has an entry 
*/

UPDATE INDEX_TAXON_NAME SET RECOMMENDED_TAXON_LIST_ITEM_KEY
= NS.RECOMMENDED_TAXON_LIST_ITEM_KEY 
FROM NAMESERVER NS 
INNER JOIN INDEX_TAXON_NAME ON NS.INPUT_TAXON_VERSION_KEY 
= INDEX_TAXON_NAME.TAXON_VERSION_KEY 
WHERE INDEX_TAXON_NAME.TAXON_LIST_ITEM_KEY  = @TLIKey

/* Catch All 
*/

UPDATE INDEX_TAXON_NAME SET RECOMMENDED_TAXON_LIST_ITEM_KEY
= TAXON_LIST_ITEM_KEY 
FROM INDEX_TAXON_NAME 
WHERE RECOMMENDED_TAXON_LIST_ITEM_KEY IS NULL
AND INDEX_TAXON_NAME.TAXON_LIST_ITEM_KEY  = @TLIKey

--Now set up the recommended sort orders, which depend on the recommended names

UPDATE ITN
SET ITN.Sort_Order=
	LEFT('000', 3 - LEN(CONVERT(VARCHAR(3), ISNULL(TG.Sort_Order,0))))
  + CONVERT(VARCHAR(3), ISNULL(TG.Sort_Order,0)) 
	+ LEFT('00000000', 8 - LEN(CONVERT(VARCHAR(8), ISNULL(TLI.Sort_Code,0))))
  + CONVERT(VARCHAR(8), ISNULL(TLI.Sort_Code,0)) 
FROM Index_Taxon_Name ITN 
INNER JOIN Taxon_List_Item TLI 
	ON TLI.Taxon_List_Item_Key=ITN.Recommended_Taxon_List_Item_Key
INNER JOIN TAXON_LIST_VERSION TLV ON TLV.TAXON_LIST_VERSION_KEY=TLI.TAXON_LIST_VERSION_KEY
INNER JOIN Taxon_Version TV 
	ON TV.Taxon_Version_Key=TLI.Taxon_Version_Key
LEFT JOIN Taxon_Group TG ON TG.Taxon_Group_Key=TV.Output_Group_Key
WHERE ITN.TAXON_LIST_ITEM_KEY = @TLIKey

-- If there is anything in the Organism table, then it overwrites sort orders.

UPDATE Index_Taxon_Name 
  SET SORT_ORDER = ORG.SORT_ORDER
  FROM INDEX_TAXON_NAME ITN 
  INNER JOIN TAXON_LIST_ITEM TLI ON TLI.TAXON_LIST_ITEM_KEY = ITN.RECOMMENDED_TAXON_LIST_ITEM_KEY
  INNER JOIN TAXON_LIST_VERSION TLV 
    ON TLV.TAXON_LIST_VERSION_KEY=TLI.TAXON_LIST_VERSION_KEY
  INNER JOIN ORGANISM ORG ON ORG.TAXON_VERSION_KEY = TLI.TAXON_VERSION_KEY
  WHERE ITN.TAXON_LIST_ITEM_KEY = @TLIKey

GO

UPDATE IW_COLUMN_TYPE SET ITEM_NAME = 'Reviewers Comment' WHERE 
IW_COLUMN_TYPE_KEY = 'SYSTEM01000000E0'

GO
/****** Object:  StoredProcedure [dbo].[usp_IWMatch_Species]    Script Date: 01/17/2019 19:16:32 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

/*===========================================================================*\
  Description:	Set the match for the specified import record in the match table.

  Parameters:	@ChecklistKey

  Created:	June 2004

  Last revision information:
    Nov 2018 
    MikeWeideli
    Preferred List changed to work on Preferred taxa. The count is based on all the preferred taxa
    (ie all possibilities), because we need to make sure that we do not match 
    erroneously, however we only actual do a match if the TLI is not deprecated 
    and allows data entry - preferred taxa = 1)  
    
    This procedure now uses ITN Output_Taxon_Name instead of the udf
    Also uses ITN.Deprecated instead of working this out each time based on the 
    version in TLV. There is no reason why Deprecated taxa can not be used for input/matching as they may have 
    been recorded using an earlier version of the list.  
      
    A new notes field is now being used to reflect the reason for rejection  

\*===========================================================================*/


ALTER PROCEDURE [dbo].[usp_IWMatch_Species]
	@ChecklistKey char(16)
AS
    
UPDATE	#Species
SET	Species_Name = 
    CASE	WHEN Right(Import_Value, 2) = 'sp' THEN Left(Import_Value, Len(Import_Value) - 2)
    WHEN Right(Import_Value, 3) = 'sp.' THEN Left(Import_Value, Len(Import_Value) - 3)
	WHEN Right(Import_Value, 3) = 'spp' THEN Left(Import_Value, Len(Import_Value) - 3)
	WHEN Right(Import_Value, 4) = 'spp.' THEN Left(Import_Value, Len(Import_Value) - 4)
	  ELSE Import_Value
	END
    --  remove the sub genus from import_value 
    UPDATE #Species Set Species_name = dbo.LCRemoveSubGenusText(Species_name),
    Match_Count = 0, Status = 0 
    
   
    -- Match TV Key
       UPDATE	#Species
		SET	Match_Key = ITN2.Taxon_List_Item_Key,
			Match_Value =  TLI.Taxon_Version_Key,
			Checklist = TL.Item_Name,
			Checklist_Key = @ChecklistKey,
			Match_Count = 1,
			Status = 1
		FROM	
		Taxon_List_Item TLI INNER JOIN Index_Taxon_Name ITN ON ITN.Taxon_List_Item_Key = TLi.Taxon_List_Item_Key
		INNER JOIN Index_Taxon_Name ITN2 ON ITN.Recommended_Taxon_List_Item_Key = ITN2.Taxon_List_Item_Key
		JOIN	Taxon_List_Version TLV ON TLV.Taxon_List_Version_Key = ITN2.Taxon_List_Version_Key
		JOIN	Taxon_List TL ON TL.Taxon_List_Key = TLV.Taxon_List_Key
		WHERE	Species_Name = TLI.Taxon_Version_Key
  
 IF @ChecklistKey IS NULL
   BEGIN
   -- Handle searches against the preferrred taxa (Types above 0)     
   UPDATE	UpdatedSpecies
      SET	Match_Count =  (SELECT	Count(DISTINCT ITN.Taxon_List_Item_Key)
	  FROM	#Species S  
	  INNER JOIN Index_Taxon_Name ITN ON ITN.Output_Taxon_Name
	   = S.Species_Name
	  WHERE	ITN.Preferred_Taxa > 0 
	  AND	S.Import_Value = UpdatedSpecies.Import_Value)
	  FROM	#Species UpdatedSpecies
	  WHERE	Match_Key IS NULL

   -- Now get values and keys for unique matches only. Broken down in two separate updates for speed.
   UPDATE	#Species
      SET	Match_Key = TLI.PREFERRED_NAME,
	  Match_Value = dbo.ufn_GetFormattedSpeciesName(TLI.PREFERRED_NAME), 
	  Checklist = TL.Item_Name,
      Checklist_Key = NULL, Status = 1
	  FROM	Index_Taxon_Name ITN
	  JOIN	Taxon_List_Version TLV ON TLV.Taxon_List_Version_Key = ITN.Taxon_List_Version_Key
	  JOIN	Taxon_List TL ON TL.Taxon_List_Key = TLV.Taxon_List_Key
	  INNER JOIN Taxon_List_Item TLI ON TLI.Taxon_List_Item_Key
	  = ITN.Taxon_List_Item_Key 
	  WHERE	Match_Count = 1
	  AND	Match_Key IS NULL
	   AND  ITN.Preferred_Taxa = 1 -- This is correct will auto match only 1
	  AND	Species_Name = ITN.Output_Taxon_Name
	 
     -- Now set the value for the preferred taxa where not set
      UPDATE #Species
      SET	Status = ITN.PREFERRED_TAXA
	  FROM	Index_Taxon_Name ITN
	  WHERE Species_Name = ITN.Output_Taxon_Name   
      AND Status = 0 AND PREFERRED_TAXA > 0
    END ELSE
    BEGIN
	  If LEFT(@checkListkey,7) = 'VIRTUAL'
	  -- Deal with Virtual Organism table 
	  BEGIN 
	    UPDATE	UpdatedSpecies
        SET	Match_Count =  (SELECT	Count(DISTINCT ITN.Taxon_List_Item_Key)
	    FROM	#Species S  
	    INNER JOIN Index_Taxon_Name ITN ON ITN.Output_Taxon_Name
	    = S.Species_Name
	    
	   WHERE	ITN.Preferred_Taxa > 0 
	   AND	S.Import_Value = UpdatedSpecies.Import_Value)
	   FROM	#Species UpdatedSpecies
	   WHERE	Match_Key IS NULL
	   
	    -- Virtal Organism get values and keys for unique matches only. Broken down in two separate updates for speed.
		-- where the taxa is a non deprecated and allow data entry taxa.
		-- the only difference with this that this will always return the
		-- recommended name.       
	   UPDATE	#Species
       SET	Match_Key = ITN.Recommended_Taxon_List_Item_Key,
	   Match_Value = dbo.ufn_GetFormattedSpeciesName(ITN.Taxon_List_Item_Key),
	   Checklist = 'VIRTUAL_ORGANISM',
	   Checklist_Key = 'VIRTUAL_ORGANISM', Status = 1
	   FROM	Index_Taxon_Name ITN
	   JOIN	Taxon_List_Version TLV ON TLV.Taxon_List_Version_Key = ITN.Taxon_List_Version_Key
	   JOIN	Taxon_List TL ON TL.Taxon_List_Key = TLV.Taxon_List_Key
	   WHERE	Match_Count = 1
	   AND	Match_Key IS NULL
	   AND  ITN.Preferred_Taxa = 1 -- This is correct will auto match only 1
	   AND	Species_Name = ITN.Output_Taxon_Name
		
	   UPDATE #Species
       SET	Status = ITN.PREFERRED_TAXA
	   FROM	Index_Taxon_Name ITN
	   WHERE Species_Name = ITN.Output_Taxon_Name   
       AND Status = 0 AND PREFERRED_TAXA > 0		
		
	  END ELSE
	  BEGIN
	  -- Handle searches against a normal specified list No change here.
      -- Set Match_Count first. Broken down in two separate updates for speed.
		UPDATE	UpdatedSpecies
		SET	Match_Count =  (SELECT	Count(*)
					FROM	#Species S 
					INNER JOIN Index_Taxon_Name ITN ON ITN.OUTPUT_TAXON_NAME = S.Species_Name
					INNER JOIN Taxon_List_Version TLV ON TLV.Taxon_List_Version_Key = ITN.Taxon_List_Version_Key					
					INNER JOIN Taxon_List_Item TLI ON TLI.Taxon_List_Item_Key=ITN.Taxon_List_Item_Key
					WHERE	TLV.Taxon_List_Key = @ChecklistKey
					AND	S.Import_Value = UpdatedSpecies.Import_Value
					AND ITN.DEPRECATED = 0) 
		FROM	#Species UpdatedSpecies
		WHERE	Match_Key IS NULL

		UPDATE	UpdatedSpecies
		SET	Match_Count = Match_Count + (SELECT	Count(*)
					FROM	#Species S 
					INNER JOIN Index_Taxon_Name ITN ON ITN.Output_Taxon_Name  + ' ' + ITN.Authority = S.Species_Name 
					INNER JOIN Taxon_List_Version TLV ON TLV.Taxon_List_Version_Key = ITN.Taxon_List_Version_Key					
					INNER JOIN Taxon_List_Item TLI ON TLI.Taxon_List_Item_Key=ITN.Taxon_List_Item_Key
					WHERE	TLV.Taxon_List_Key = @ChecklistKey
					AND	S.Import_Value = UpdatedSpecies.Import_Value
					AND ITN.DEPRECATED = 0)
		FROM	#Species UpdatedSpecies
		WHERE	Match_Key IS NULL
		
		 
		-- Now get values and keys for unique matches only. Broken down in two separate updates for speed.
		UPDATE	#Species
		SET	Match_Key = ITN.Taxon_List_Item_Key,
			Match_Value = dbo.ufn_GetFormattedSpeciesName(ITN.Taxon_List_Item_Key),
			Checklist = TL.Item_Name,
			Checklist_Key = TL.Taxon_List_Key,
			Status = 1
		FROM	Index_Taxon_Name ITN
		JOIN	Taxon_List_Version TLV ON TLV.Taxon_List_Version_Key = ITN.Taxon_List_Version_Key
		JOIN	Taxon_List TL ON TL.Taxon_List_Key = TLV.Taxon_List_Key
		WHERE	Match_Count = 1
		AND	Match_Key IS NULL
		AND	TL.Taxon_List_Key = @ChecklistKey
		AND	Species_Name = ITN.OUTPUT_TAXON_NAME
		AND ITN.DEPRECATED = 0

		UPDATE	#Species
		SET	Match_Key = ITN.Taxon_List_Item_Key,
			Match_Value = dbo.ufn_GetFormattedSpeciesName(ITN.Taxon_List_Item_Key),
			Checklist = TL.Item_Name,
			Checklist_Key = TL.Taxon_List_Key,
			Status = 1
		    
		FROM	Index_Taxon_Name ITN
		JOIN	Taxon_List_Version TLV ON TLV.Taxon_List_Version_Key = ITN.Taxon_List_Version_Key
		JOIN	Taxon_List TL ON TL.Taxon_List_Key = TLV.Taxon_List_Key
		WHERE	Match_Count = 1
		AND	Match_Key IS NULL
		AND	TL.Taxon_List_Key = @ChecklistKey
		AND	Species_Name = ITN.OUTPUT_TAXON_NAME + ' ' + ITN.Authority
		AND ITN.DEPRECATED IS NULL
	  END
    END
    -- Use Index_Taxon_Hierarchy to get the the Order 
    EXEC  [dbo].[usp_IWMatch_Species_Order]
	
	
    -- Update the notes
    EXEC [dbo].[usp_IWMatch_Species_Notes]

GO
/****** Object:  StoredProcedure [dbo].[usp_IW_Species_Multi_Update]    Script Date: 01/17/2019 19:18:31 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

/*===========================================================================*\
  Description:		Updates the #Species table following 
  selction from the notes column 
  Parameters @ImportValue 
			 @MatchCount
			 @MatchKey
             @ManualMatch
             @Remembered
          

  Created:	November 2018 
 

\*===========================================================================*/
ALTER PROCEDURE [dbo].[usp_IW_Species_Multi_Update] 
@ImportValue VARCHAR(100),
@MatchCount INT,
@MatchKey CHAR(16),
@ManualMatch BIT,
@Remembered BIT
AS
  DECLARE @PreferredKey CHAR(16)

   -- Matches are saved with the preferred key
  SELECT	@PreferredKey = TLI.Preferred_Name
  FROM	Taxon_List_Item TLI 
  WHERE	TLI.Taxon_List_Item_Key = @MatchKey

		
 
  UPDATE #SPECIES 
  SET Match_Count = @MatchCount,
  Match_Key = @PreferredKey,
  Manual_Match = @ManualMatch,
  Remembered = @Remembered,
  Match_Value = dbo.ufn_GetFormattedSpeciesName(@PreferredKey),
  CheckList = TL.ITEM_NAME,
  CheckList_Key = '' 
  FROM #SPECIES S 
  INNER JOIN INDEX_TAXON_NAME ITN ON ITN.TAXON_LIST_ITEM_KEY = @MatchKey 
  INNER JOIN TAXON_LIST_VERSION TLV 
  ON TLV.TAXON_LIST_VERSION_KEY = ITN.TAXON_LIST_VERSION_KEY
  INNER JOIN TAXON_LIST TL
  ON TL.TAXON_LIST_KEY = TLV.TAXON_LIST_KEY
  WHERE
  S.Import_Value = @ImportValue
  AND ITN.Allow_Data_Entry = 1
  AND @PreferredKey <> ''
  
  DELETE FROM IW_Matched_Species
  WHERE Matched_Value = @ImportValue AND
  Matched_Key <> @PreferredKey
    
  EXEC [dbo].[usp_IWMatch_Species_Notes]
  
  EXEC [dbo].[usp_IWMatch_Species_Order_Single] @PreferredKey
  
GO
/****** Object:  StoredProcedure [dbo].[usp_IWNotes_Species_Detail]    Script Date: 01/18/2019 15:55:57 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

/*===========================================================================*\
  Description:		Returns additional information on a species
                    selected for possible matching 
  Parameters: @Key TLI Key 
  Created:	November 2018 
  Updated: January 2019 
  
\*===========================================================================*/
ALTER PROCEDURE [dbo].[usp_IWNotes_Species_Detail]
(@Key varchar(16), @Detail integer, @ImportValue varchar(100) = Null)
AS
  DECLARE @EOL CHAR(2), @PREFERREDKEY CHAR(16)
  
  SELECT	@PreferredKey = TLI.Preferred_Name
  FROM	Taxon_List_Item TLI WHERE
  TAXON_LIST_ITEM_KEY = @Key 
  
  SET @EOL = CHAR(13) + CHAR(10)  
  IF @Detail = 0 
  BEGIN 
     SELECT
     'Is current match : '  
     + Case when #SPECIES.MATCH_KEY = @PREFERREDKEY  then 'Yes'
          else 'No'
      End 
     + @EOL	
     + 'Taxon Name: '     
     + dbo.ufn_GetFullSpeciesName (ITN.TAXON_LIST_ITEM_KEY)
     + @EOL	
	 + 'Taxon List : ' + TL.Item_Name 
	 + @EOL	
	 + 'Common Name : ' + ITN.COMMON_NAME
	 + @EOL	
	 + 'Preferred Name : ' + ITN.PREFERRED_NAME
	 + @EOL	
	 + 'Recommended Name : ' + dbo.ufn_GetFullSpeciesName(ITN.RECOMMENDED_TAXON_LIST_ITEM_KEY)
	 + @EOL	
	 + 'Taxon Group : ' + TG.Taxon_Group_Name
	  + @EOL	
	 + 'Is User Added : ' 
	 + Case when TV.SYSTEM_SUPPLIED_DATA = 0 then 'Yes'
          else 'No'
	   End 
	 + @EOL	
	 + 'Deprecated : '   
	 + Case when Deprecated = 1 then 'Yes'
          else 'No'
	   End 
	  + @EOL	
	  + 'Allow Data Entry : '   
	  + Case when Allow_Data_Entry = 1 then 'Yes'
          else 'No'
	  +  @EOL
	  + 'Taxon User Names : '
	  + ISNULL(dbo.LCUserAddedNames (ITN.TAXON_LIST_ITEM_KEY),'') 
	  + @EOL
	  + 'TLI Key : '
	  +  ITN.TAXON_LIST_ITEM_KEY 
	  End 
	  AS Details
	  FROM INDEX_TAXON_NAME ITN
	  INNER JOIN TAXON_LIST_VERSION TLV
	  ON TLV.TAXON_LIST_VERSION_KEY = ITN.TAXON_LIST_VERSION_KEY
	  INNER JOIN TAXON_LIST TL
	  ON TL.TAXON_LIST_KEY = TLV.TAXON_LIST_KEY
	  INNER JOIN TAXON_VERSION TV
	  ON TV.TAXON_VERSION_KEY = ITN.TAXON_VERSION_KEY
	  INNER JOIN TAXON_GROUP TG 
	  ON TG.TAXON_GROUP_KEY = TV.OUTPUT_GROUP_KEY
	  INNER JOIN #SPECIES ON #SPECIES.IMPORT_VALUE = @IMPORTVALUE 
	  WHERE ITN.Taxon_List_Item_Key = @Key
	  AND ITN.SYSTEM_SUPPLIED_DATA = 1
  END ELSE
  BEGIN
     SELECT 
     'Is current match : ' 
     + Case when #SPECIES.MATCH_KEY = @PREFERREDKEY   then 'Yes'
          else 'No'
       End 
     + @EOL	
     + 'Taxon Name: ' + dbo.ufn_GetFullSpeciesName (ITN.TAXON_LIST_ITEM_KEY)
     + @EOL	
	 + 'Taxon List : ' + TL.Item_Name 
	 + @EOL	
	 + 'Common Name : ' + ITN.COMMON_NAME
	 + @EOL	
	 + 'Preferred Name : ' + ITN.PREFERRED_NAME
	 + @EOL	
	 + 'Recommended Name : ' + dbo.ufn_GetFullSpeciesName(ITN.RECOMMENDED_TAXON_LIST_ITEM_KEY)
	 + @EOL	
	 + 'Taxon Group : ' + TG.Taxon_Group_Name
	  + @EOL	
	 + 'Is User Added : ' 
	 + Case when TV.SYSTEM_SUPPLIED_DATA = 0 then 'Yes'
          else 'No'
	   End 
	 + @EOL	
	 + 'Deprecated : '   
	 + Case when Deprecated = 1 then 'Yes'
          else 'No'
	   End 
	  + @EOL	
	  + 'Allow Data Entry : '   
	  + Case when Allow_Data_Entry = 1 then 'Yes'
          else 'No'
	  End 
	  + @EOL 
	  + 'Taxon User Names : '
	  + ISNULL(dbo.LCUserAddedNames (ITN.TAXON_LIST_ITEM_KEY),'') 
	  + @EOL
	  + 'TLI Key : '
	  +  ITN.TAXON_LIST_ITEM_KEY
	  + @EOL
	  + 'Number of Records : ' 
	  + (SELECT STR(COUNT(Taxon_Determination_Key))
	     FROM TAXON_DETERMINATION TDET
	     WHERE TDET.TAXON_LIST_ITEM_KEY = @Key)
	  AS Details 
	  FROM INDEX_TAXON_NAME ITN
	  INNER JOIN TAXON_LIST_VERSION TLV
	  ON TLV.TAXON_LIST_VERSION_KEY = ITN.TAXON_LIST_VERSION_KEY
	  INNER JOIN TAXON_LIST TL
	  ON TL.TAXON_LIST_KEY = TLV.TAXON_LIST_KEY
	  INNER JOIN TAXON_VERSION TV
	  ON TV.TAXON_VERSION_KEY = ITN.TAXON_VERSION_KEY
	  INNER JOIN TAXON_GROUP TG 
	  ON TG.TAXON_GROUP_KEY = TV.OUTPUT_GROUP_KEY
	  INNER JOIN #SPECIES ON #SPECIES.IMPORT_VALUE = @IMPORTVALUE 
	  WHERE ITN.Taxon_List_Item_Key = @Key
      AND ITN.SYSTEM_SUPPLIED_DATA = 1
  END
  
GO
/****** Object:  StoredProcedure [dbo].[usp_RemoveUnwantedOccurrences]    Script Date: 02/06/2019 14:24:28 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
    
/*
  DESCRIPTION
  This procedure removes invalid taxon and biotope occurrences
  that could have appeared after an import.  Returns the deleted 
  occurrence keys. Also updates the Verified column in the
  Taxon_Occurrence table to match the imported data.

  PARAMETERS
  None

  Last Revision Details:
    $Revision: 9 $
    $Date: 05/03/13 15:50 $
    $Author: Michaelcaptain $
    Updates february 2019 to update verified on preferred
    and to cope with Taxon_Private_data  
*/

ALTER PROCEDURE [dbo].[usp_RemoveUnwantedOccurrences]
AS

SET NOCOUNT ON

   BEGIN TRAN
        -- Set Verified column in Taxon_Occurrence
        IF OBJECT_ID('tempdb..#Taxon_Occurrence') IS NOT NULL
	UPDATE taxo
	SET taxo.VERIFIED=det.Verified
	FROM Taxon_Occurrence taxo
	INNER JOIN #Taxon_Occurrence tt ON tt.TAXON_OCCURRENCE_KEY = taxo.TAXON_OCCURRENCE_KEY
	JOIN #Taxon_Determination taxd ON taxd.TAXON_OCCURRENCE_KEY = taxo.TAXON_OCCURRENCE_KEY
	AND taxd.preferred = 1
	JOIN DETERMINATION_TYPE det ON det.DETERMINATION_TYPE_KEY = taxd.DETERMINATION_TYPE_KEY
     
     
	-- Gather invalid taxon occurrences keys, they will be used several times
	SELECT Occ.Taxon_Occurrence_Key INTO #DeleteTaxa
	FROM Taxon_Occurrence Occ 
	LEFT JOIN Taxon_Determination TD ON TD.Taxon_Occurrence_Key=Occ.Taxon_Occurrence_Key
	WHERE TD.Taxon_Determination_Key IS NULL

	--Record the records we are about to remove
	SELECT Taxon_Occurrence_Key AS ItemKey, CAST('TAXON_OCCURRENCE' AS VARCHAR(30)) as TableName
 	INTO #Deletions
	FROM #DeleteTaxa

	INSERT INTO #Deletions
	SELECT TOS.Source_Link_Key AS ItemKey, 'TAXON_OCCURRENCE_SOURCES' as TableName
	FROM TAXON_OCCURRENCE_SOURCES TOS
	INNER JOIN #DeleteTaxa D ON D.Taxon_Occurrence_Key=TOS.Taxon_Occurrence_Key

	INSERT INTO #Deletions
	SELECT TOD.Taxon_Occurrence_Data_Key AS ItemKey, 'TAXON_OCCURRENCE_DATA' as TableName
	FROM TAXON_OCCURRENCE_DATA TOD
	INNER JOIN #DeleteTaxa D ON D.Taxon_Occurrence_Key=TOD.Taxon_Occurrence_Key

	-- Remove Taxon_Private_Data
	
	DELETE FROM Taxon_Private_Data
	WHERE Taxon_Occurrence_Key IN (
		SELECT Taxon_Occurrence_Key FROM #DeleteTaxa) 
	
	-- Remove associated Taxon Occurrence Sources
	DELETE FROM Taxon_Occurrence_Sources 
	WHERE Taxon_Occurrence_Key IN (
		SELECT Taxon_Occurrence_Key FROM #DeleteTaxa
	)
	IF @@Error > 0 GOTO RollBackAndExit

	-- Remove associated Taxon Occurrence Data
	DELETE FROM Taxon_Occurrence_Data
	WHERE Taxon_Occurrence_Key IN (
		SELECT Taxon_Occurrence_Key FROM #DeleteTaxa
	)
	IF @@Error > 0 GOTO RollBackAndExit

	-- Remove associated specimen data
	DELETE FROM Specimen
	WHERE Taxon_Occurrence_Key IN (
		SELECT Taxon_Occurrence_Key FROM #DeleteTaxa
	)
	IF @@Error > 0 GOTO RollBackAndExit

	-- Remove associated specimen field data
	IF OBJECT_ID(N'dbo.Specimen_Field_Data') IS NOT NULL
	BEGIN	
		DELETE FROM Specimen_Field_Data
		WHERE Taxon_Occurrence_Key IN (
			SELECT Taxon_Occurrence_Key FROM #DeleteTaxa
		)
		IF @@Error > 0 GOTO RollBackAndExit
	END

	-- Remove taxon occurrence relations
	DELETE FROM Taxon_Occurrence_Relation
	WHERE Taxon_Occurrence_Key_1 IN (
		SELECT Taxon_Occurrence_Key FROM #DeleteTaxa
	)
	OR Taxon_Occurrence_Key_2 IN (
		SELECT Taxon_Occurrence_Key FROM #DeleteTaxa
	)
	IF @@Error > 0 GOTO RollBackAndExit

	-- And finally remove Taxon Occurrences
	DELETE FROM Taxon_Occurrence 
	WHERE Taxon_Occurrence_Key IN (
		SELECT Taxon_Occurrence_Key FROM #DeleteTaxa
	)
	IF @@Error > 0 GOTO RollBackAndExit

	-- Gather invalid biotope occurrences keys, they will be used several times
	SELECT 	Occ.Biotope_Occurrence_Key into #DeleteBiotopes
	FROM Biotope_Occurrence Occ WHERE Occ.Biotope_Occurrence_Key NOT IN (
		SELECT Biotope_Occurrence_Key FROM Biotope_Determination
	)

	--Record the records we are about to remove
	INSERT INTO #Deletions
	SELECT Biotope_Occurrence_Key AS ItemKey, 'BIOTOPE_OCCURRENCE' as TableName
	FROM #DeleteBiotopes

	INSERT INTO #Deletions
	SELECT BOS.Source_Link_Key AS ItemKey, 'BIOTOPE_OCCURRENCE_SOURCES' as TableName
	FROM BIOTOPE_OCCURRENCE_SOURCES BOS
	INNER JOIN #DeleteBiotopes D ON D.Biotope_Occurrence_Key=BOS.Biotope_Occurrence_Key

	INSERT INTO #Deletions
	SELECT BOD.Biotope_Occurrence_Data_Key AS ItemKey, 'BIOTOPE_OCCURRENCE_DATA' as TableName
	FROM BIOTOPE_OCCURRENCE_DATA BOD
	INNER JOIN #DeleteBiotopes D ON D.Biotope_Occurrence_Key=BOD.Biotope_Occurrence_Key

	-- Remove associated Biotope Occurrence Sources
	DELETE FROM Biotope_Occurrence_Sources
	WHERE Biotope_Occurrence_Key IN (
		SELECT Biotope_Occurrence_Key FROM #DeleteBiotopes
	)
	IF @@Error > 0 GOTO RollBackAndExit

	-- Remove associated Biotope Occurrence Data
	DELETE FROM Biotope_Occurrence_Data
	WHERE Biotope_Occurrence_Key IN (
		SELECT Biotope_Occurrence_Key FROM #DeleteBiotopes
	)
	IF @@Error > 0 GOTO RollBackAndExit
	
	-- And finally remove Biotope Occurrences
	DELETE FROM Biotope_Occurrence 
	WHERE Biotope_Occurrence_Key IN (
		SELECT Biotope_Occurrence_Key FROM #DeleteBiotopes
	)
	IF @@Error > 0 GOTO RollBackAndExit

	SELECT * FROM #Deletions

  COMMIT TRAN

SET NOCOUNT OFF 

RollBackAndExit: 
    IF @@TranCount> 0 ROLLBACK TRAN 
    SET NOCOUNT OFF  

  
GO
/****** Object:  StoredProcedure [dbo].[usp_ImportWizard_Unwanted_Review]    Script Date: 02/05/2019 08:33:37 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

/*===========================================================================*\
  Description:  Removes entries in Taxon_Determination where there is no reviewer       
  Parameters:   None

  Created:      Feb 2018

  Last revision information:
       $Author: Mikeweideli$

\*===========================================================================*/

ALTER PROCEDURE [dbo].[usp_ImportWizard_Unwanted_Review]
AS
  DELETE FROM #Taxon_Determination where DETERMINER IS NULL 
 
  Update #Taxon_Determination  set Preferred = 1 where  
  PREFERRED = 0 and NOT EXISTS(Select * From #TAXON_DETERMINATION TDET
  WHERE TDET.TAXON_OCCURRENCE_KEY = #Taxon_Determination.TAXON_OCCURRENCE_KEY 
  AND TDET.PREFERRED = 1)
  

     
GO
/****** Object:  UserDefinedFunction [dbo].[ufn_ReturnReviewStatus]    Script Date: 02/07/2019 12:40:24 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO


ALTER FUNCTION [dbo].[ufn_ReturnReviewStatus] 
(@TOCCKey char(16))
RETURNS char(1)

AS
BEGIN
Declare @Competency integer
Declare @ReviewStatus char(1)

Set @Competency =  3
Set @ReviewStatus = '0'

IF @Competency > 0 

BEGIN

Set @ReviewStatus = '2'

SET @Competency = 3

IF NOT EXISTS (SELECT * FROM TAXON_DETERMINATION TDET 
INNER JOIN DETERMINER_ROLE DR ON DR.DETERMINER_ROLE_KEY =
TDET.DETERMINER_ROLE_KEY WHERE TDET.TAXON_OCCURRENCE_KEY = @TOCCKey
AND DR.VALIDATION_COMPETENCY >= @Competency AND  
Str(TDET.VAGUE_DATE_START)+ TDET.TAXON_DETERMINATION_KEY =  
(SELECT MAX(Str(TDET2.VAGUE_DATE_START)+ TDET2.TAXON_DETERMINATION_KEY)
 FROM TAXON_DETERMINATION TDET2 WHERE TDET2.TAXON_OCCURRENCE_KEY
= TDET.TAXON_OCCURRENCE_KEY))   
SET @ReviewStatus = 1

ELSE

IF EXISTS (SELECT * FROM TAXON_DETERMINATION TDET INNER JOIN
TAXON_DETERMINATION TDET2 ON TDET2.TAXON_OCCURRENCE_KEY = TDET.TAXON_OCCURRENCE_KEY
AND TDET.PREFERRED = 1
INNER JOIN DETERMINATION_TYPE TDT ON TDT.DETERMINATION_TYPE_KEY =
TDET.DETERMINATION_TYPE_KEY INNER JOIN DETERMINATION_TYPE TDT2 ON TDT2.DETERMINATION_TYPE_KEY =
TDET2.DETERMINATION_TYPE_KEY   
WHERE Str(TDET2.VAGUE_DATE_START)+ TDET2.TAXON_DETERMINATION_KEY =  
(SELECT MAX(Str(TDET3.VAGUE_DATE_START)+ TDET3.TAXON_DETERMINATION_KEY)
 FROM TAXON_DETERMINATION TDET3 INNER JOIN DETERMINER_ROLE DR
 ON DR.DETERMINER_ROLE_KEY = TDET3.DETERMINER_ROLE_KEY AND
 DR.VALIDATION_COMPETENCY >= @COMPETENCY 
 WHERE TDET3.TAXON_OCCURRENCE_KEY = @TOCCKey
 GROUP BY TAXON_OCCURRENCE_KEY)
 AND TDET2.VAGUE_DATE_START >= TDET.VAGUE_DATE_START 
AND (TDT2.VERIFIED = 2 OR TDT.VERIFIED = TDT2.VERIFIED OR (TDT2.VERIFIED = 0 AND TDT.VERIFIED = 1)) 
AND  TDET.TAXON_LIST_ITEM_KEY  =
TDET2.TAXON_LIST_ITEM_KEY)
SET @ReviewStatus = '3'  
 

END


Return @ReviewStatus

END

GO

INSERT INTO DETERMINER_ROLE (DETERMINER_ROLE_KEY,SHORT_NAME,LONG_NAME,VALIDATION_COMPETENCY,
ENTERED_BY,ENTRY_DATE,SYSTEM_SUPPLIED_DATA,CUSTODIAN,HIDE) VALUES
('NBNSYS0000000006','Determiner','Determiner',2,'TESTDATA00000001',
GETDATE(),1,'NBNSYS00',0)

GO

UPDATE Taxon_Private_Data SET Item_Date = CAST(Item_Date as date)


GO

/****** Object:  View [dbo].[VIRTUAL_COLEOPTE]    Script Date: 02/10/2019 17:01:19 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

ALTER VIEW [dbo].[VIRTUAL_COLEOPTE]
AS
SELECT NS.RECOMMENDED_TAXON_LIST_ITEM_KEY AS TAXON_LIST_ITEM_KEY, 'VIRTUAL_COLEOPTE' AS TAXON_LIST_VERSION_KEY, NS.RECOMMENDED_TAXON_VERSION_KEY AS TAXON_VERSION_KEY, 
       NS.RECOMMENDED_TAXON_LIST_ITEM_KEY AS PREFERRED_NAME, NULL AS TAXON_LIST_VERSION_TO, O.Sort_Code, NS2.RECOMMENDED_TAXON_LIST_ITEM_KEY AS PARENT, 
       O.ORGANISM_RANK_KEY AS TAXON_RANK_KEY, O.ENTERED_BY, O.ENTRY_DATE, O.SYSTEM_SUPPLIED_DATA, O.Has_Children
FROM   dbo.ORGANISM AS O INNER JOIN
       NAMESERVER AS NS ON O.TAXON_VERSION_KEY = NS.INPUT_TAXON_VERSION_KEY AND NS.RECOMMENDED_TAXON_VERSION_KEY = NS.INPUT_TAXON_VERSION_KEY
       AND O.REDUNDANT_FLAG IS NULL               
       INNER JOIN
       dbo.ORGANISM AS O2 ON O2.ORGANISM_KEY = O.PARENT_KEY INNER JOIN
       dbo.NAMESERVER AS NS2 ON O2.TAXON_VERSION_KEY = NS2.INPUT_TAXON_VERSION_KEY AND NS.RECOMMENDED_TAXON_VERSION_KEY = NS.INPUT_TAXON_VERSION_KEY
       INNER JOIN  dbo.Index_Virtual_Lists AS IVL ON IVL.Taxon_List_key = 'VIRTUAL_COLEOPTE' 
       AND O.Sort_Code >= IVL.Start_Sort_Code AND O.Sort_Code <= IVL.End_Sort_Code


GO

/****** Object:  View [dbo].[VIRTUAL_ORGANISM]    Script Date: 02/10/2019 17:03:19 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

ALTER VIEW [dbo].[VIRTUAL_ORGANISM]
AS
SELECT        NS.RECOMMENDED_TAXON_LIST_ITEM_KEY AS TAXON_LIST_ITEM_KEY, 'VIRTUAL_ORGANISM' AS TAXON_LIST_VERSION_KEY, NS.RECOMMENDED_TAXON_VERSION_KEY AS TAXON_VERSION_KEY, 
                         NS.RECOMMENDED_TAXON_LIST_ITEM_KEY AS PREFERRED_NAME, NULL AS TAXON_LIST_VERSION_TO, O.Sort_Code, NS2.RECOMMENDED_TAXON_LIST_ITEM_KEY AS PARENT, 
                         O.ORGANISM_RANK_KEY AS TAXON_RANK_KEY, O.ENTERED_BY, O.ENTRY_DATE, O.SYSTEM_SUPPLIED_DATA, O.Has_Children
FROM            dbo.ORGANISM AS O INNER JOIN
                         NAMESERVER AS NS ON O.TAXON_VERSION_KEY = NS.INPUT_TAXON_VERSION_KEY AND NS.RECOMMENDED_TAXON_VERSION_KEY = NS.INPUT_TAXON_VERSION_KEY
                        AND O.REDUNDANT_FLAG IS NULL               
                        INNER JOIN
                         dbo.ORGANISM AS O2 ON O2.ORGANISM_KEY = O.PARENT_KEY INNER JOIN
                         dbo.NAMESERVER AS NS2 ON O2.TAXON_VERSION_KEY = NS2.INPUT_TAXON_VERSION_KEY AND NS.RECOMMENDED_TAXON_VERSION_KEY = NS.INPUT_TAXON_VERSION_KEY
UNION
SELECT        NS.RECOMMENDED_TAXON_LIST_ITEM_KEY AS TAXON_LIST_ITEM_KEY, 'VIRTUAL_ORGANISM' AS TAXON_LIST_VERSION_KEY, NS.RECOMMENDED_TAXON_VERSION_KEY AS TAXON_VERSION_KEY, 
                         NS.RECOMMENDED_TAXON_LIST_ITEM_KEY, NULL AS TAXON_LIST_VERSION_TO, O.Sort_Code, NULL AS PARENT, O.ORGANISM_RANK_KEY AS TAXON_RANK_KEY, O.ENTERED_BY, O.ENTRY_DATE, 
                         O.SYSTEM_SUPPLIED_DATA, O.Has_Children
FROM            dbo.ORGANISM AS O INNER JOIN
                         NAMESERVER AS NS ON O.TAXON_VERSION_KEY = NS.INPUT_TAXON_VERSION_KEY AND NS.RECOMMENDED_TAXON_VERSION_KEY = NS.INPUT_TAXON_VERSION_KEY AND O.PARENT_KEY IS NULL AND 
                         O.PARENT_KEY IS NULL


GO
/****** Object:  StoredProcedure [dbo].[usp_IW_Names_Multi_Update]    Script Date: 02/11/2019 09:18:28 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

/*===========================================================================*\
  Description:		Updates the #Species table following 
  selction from the notes column 
  Parameters @ImportValue 
			 @MatchCount
			 @MatchKey
             @ManualMatch
             @Remembered
            

  Created:	November 2018 
 

\*===========================================================================*/
ALTER PROCEDURE [dbo].[usp_IW_Names_Multi_Update] 
@ImportValue VARCHAR(100),
@MatchCount INT,
@MatchKey CHAR(16),
@ManualMatch BIT,
@Remembered BIT

AS
  UPDATE #NAMES 
  SET Match_Count = @MatchCount,
  Match_Key = @MatchKey,
  Manual_Match = @ManualMatch,
  Remembered = @Remembered,
  Match_Value =  dbo.ufn_GetFormattedName(@MatchKey)
  FROM INDIVIDUAL  I 
  WHERE
  #NAMES.Import_Value = @ImportValue
  AND @MatchKey <>''
  
  DELETE FROM IW_Matched_Names WHERE Matched_Value =
  @ImportValue and Matched_Key <> @MatchKey  
  
  
GO
/****** Object:  StoredProcedure [dbo].[usp_IW_Species_Multi_Update]    Script Date: 02/11/2019 09:21:17 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

/*===========================================================================*\
  Description:		Updates the #Species table following 
  selction from the notes column 
  Parameters @ImportValue 
			 @MatchCount
			 @MatchKey
             @ManualMatch
             @Remembered
          

  Created:	November 2018 
 

\*===========================================================================*/
ALTER PROCEDURE [dbo].[usp_IW_Species_Multi_Update] 
@ImportValue VARCHAR(100),
@MatchCount INT,
@MatchKey CHAR(16),
@ManualMatch BIT,
@Remembered BIT
AS
  DECLARE @PreferredKey CHAR(16)

   -- Matches are saved with the preferred key
  SELECT	@PreferredKey = TLI.Preferred_Name
  FROM	Taxon_List_Item TLI 
  WHERE	TLI.Taxon_List_Item_Key = @MatchKey

		
 
  UPDATE #SPECIES 
  SET Match_Count = @MatchCount,
  Match_Key = @PreferredKey,
  Manual_Match = @ManualMatch,
  Remembered = @Remembered,
  Match_Value = dbo.ufn_GetFormattedSpeciesName(@PreferredKey),
  CheckList = TL.ITEM_NAME,
  CheckList_Key = '' 
  FROM #SPECIES S 
  INNER JOIN INDEX_TAXON_NAME ITN ON ITN.TAXON_LIST_ITEM_KEY = @MatchKey 
  INNER JOIN TAXON_LIST_VERSION TLV 
  ON TLV.TAXON_LIST_VERSION_KEY = ITN.TAXON_LIST_VERSION_KEY
  INNER JOIN TAXON_LIST TL
  ON TL.TAXON_LIST_KEY = TLV.TAXON_LIST_KEY
  WHERE
  S.Import_Value = @ImportValue
  AND ITN.Allow_Data_Entry = 1
  AND @PreferredKey <> ''
  
  DELETE FROM IW_Matched_Species
  WHERE Matched_Value = @ImportValue AND
  Matched_Key <> @PreferredKey
    
  EXEC [dbo].[usp_IWMatch_Species_Order_Single] @PreferredKey
  
GO
/****** Object:  StoredProcedure [dbo].[usp_IWNotes_Locations_Update]    Script Date: 02/11/2019 09:28:12 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

/*===========================================================================*\
  Description:		Updates the #table following 
  selection from the notes column 
  Parameters @ImportValue 
			 @MatchCount
			 @MatchKey
             @ManualMatch
             @Remembered
            
             

  Created:	November 2018 
 

\*===========================================================================*/
ALTER PROCEDURE [dbo].[usp_IWNotes_Locations_Update] 
@ImportValue VARCHAR(100),
@MatchCount INT,
@MatchKey CHAR(16),
@ManualMatch BIT,
@Remembered BIT

AS
  UPDATE	#Locations
		SET	Match_Value = LN.Item_Name,
			Match_Key = @MatchKey,
			Match_Count = @MatchCount,
			Manual_Match = @ManualMatch,
			Remembered = @Remembered,
			Spatial_Ref = L.Spatial_Ref,
			Spatial_Ref_System = L.Spatial_Ref_System,
			Lat = L.Lat,
			Long = L.Long
			
		FROM	Location L
		JOIN	Location_Name LN ON LN.Location_Key = L.Location_Key AND LN.Preferred = 1
		WHERE	L.Location_Key = @MatchKey
		AND	Import_Value = @ImportValue

  DELETE FROM IW_Matched_Locations WHERE 
      Matched_Value = @ImportValue 
      AND Matched_Key <> @MatchKey
      
GO
/****** Object:  StoredProcedure [dbo].[usp_IWMatchSet_Species]    Script Date: 02/11/2019 12:57:02 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER OFF
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
    $Revision: 1 $
    $Date: 12/02/09 12:07 $
    $Author: Simonwood $
    Mike Weideli February 2019
\*===========================================================================*/
ALTER PROCEDURE [dbo].[usp_IWMatchSet_Species]
	@ImportValue varchar(100),
	@MatchKey CHAR(16),
	@UserID CHAR(16),
	@ChecklistKey CHAR(16)
AS
	IF @MatchKey IS NOT NULL BEGIN
		DECLARE	@Order varchar(100), 
			@Checklist varchar(100),
			@MatchValue varchar(100)

		-- 'Not available' value handled and regionalised in app.
	    SET @Order = 'Not available'
	
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
		SET @MatchKey = null 
		UPDATE	#Species
		SET	Match_Value = NULL,
			Match_Key = NULL,
			Match_Count = 0,
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

EXEC  dbo.usp_IWMatch_Species_Order_Single @MatchKey
 
GO

/****** Object:  StoredProcedure [dbo].[usp_IWMatchSet_Name]    Script Date: 02/11/2019 13:01:24 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

/*===========================================================================*\
  Description:	Set the match for the specified import record in the match table.

  Parameters:	@ImportValue
		@MatchKey

  Created:	June 2004

  Last revision information:
    $Revision: 1 $
    $Date: 30/07/04 15:03 $
    $Author: Johnvanbreda $
    February 2019 - Mike Weideli Update notes 
      
\*===========================================================================*/
ALTER PROCEDURE [dbo].[usp_IWMatchSet_Name]
	@ImportValue varchar(100),
	@MatchKey char(16)
AS
	IF @MatchKey IS NOT NULL
		UPDATE	#Names
		SET	Match_Value = dbo.ufn_GetFormattedName(@MatchKey),
			Match_Key = @MatchKey,
			Match_Count = 1,
			Manual_Match = 1,
			Remembered = 0
		WHERE	Import_Value = @ImportValue
	ELSE
		UPDATE	#Names
		SET	Match_Value = NULL,
			Match_Key = NULL,
			Match_Count = 0,
			Manual_Match = 0,
			Remembered = 0
		WHERE	Import_Value = @ImportValue

GO
/****** Object:  StoredProcedure [dbo].[usp_IWMatchSet_Location]    Script Date: 02/11/2019 13:02:53 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

/*===========================================================================*\
  Description:	Set the match for the specified import record in the match table.

  Parameters:	@ImportValue
		@MatchKey

  Created:	June 2004

  Last revision information:
    $Revision: 1 $
    $Date: 30/07/04 15:03 $
    $Author: Johnvanbreda $
    Mike Weideli Feb 2019 
\*===========================================================================*/
ALTER PROCEDURE [dbo].[usp_IWMatchSet_Location]
	@ImportValue varchar(100),
	@MatchKey char(16)
AS
	IF @MatchKey IS NOT NULL
		UPDATE	#Locations
		SET	Match_Value = LN.Item_Name,
			Match_Key = @MatchKey,
			Match_Count = 1,
			Manual_Match = 1,
			Remembered = 0,
			Spatial_Ref = L.Spatial_Ref,
			Spatial_Ref_System = L.Spatial_Ref_System,
			Lat = L.Lat,
			Long = L.Long
			
		FROM	Location L
		JOIN	Location_Name LN ON LN.Location_Key = L.Location_Key AND LN.Preferred = 1
		WHERE	L.Location_Key = @MatchKey
		AND	Import_Value = @ImportValue
	ELSE
		UPDATE	#Locations
		SET	Match_Value = NULL,
			Match_Key = NULL,
			Match_Count = NULL,
			Manual_Match = 0,
			Remembered = 0,
			Spatial_Ref = NULL,
			Spatial_Ref_System = NULL,
			Lat = NULL,
			Long = NULL
						
		WHERE	Import_Value = @ImportValue

GO
/****** Object:  StoredProcedure [dbo].[usp_IWNotes_Species_Select]    Script Date: 02/11/2019 14:34:24 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

/*===========================================================================*\
  Description:		Returns details matches where  one or more 
      possible match is acceptable 

  Parameters: @Key TaxonName 

  Created:	November 2018 
   
    
\*===========================================================================*/
ALTER PROCEDURE [dbo].[usp_IWNotes_Species_Select]
@Key varCHAR(75)
AS
  Declare @MatchedCount integer,@Remembered bit, @ManualMatch bit,
  @OutputName varchar(100)
  Select @MatchedCount = Match_Count from #Species where Import_Value = @Key   
  Select @Remembered = Remembered from #Species where Import_Value = @Key   
  Select @ManualMatch =  Manual_Match from #Species where Import_Value = @Key   
  Select @OutputName =  Species_Name from #Species where Import_Value = @Key   


  IF @MatchedCount = 0 Or  @Remembered = 1 Or @ManualMatch = 1 
  BEGIN
    SELECT	
    ITN.TAXON_LIST_ITEM_KEY AS AKey,	
    LTRIM(dbo.ufn_GetDeprecated(ITN.DEPRECATED,ITN.Allow_Data_Entry)+ ' ') +
    [dbo].[ufn_GetFullSpeciesName] (ITN.TAXON_LIST_ITEM_KEY) +
    ' (' + TG.TAXON_GROUP_NAME + ')'  
    AS FullDetails,ISNULL(#Species.Match_Key,'') As MatchKey, OUTPUT_TAXON_NAME, 1 AS POSSIBLE, 
    TG.Taxon_Group_Name   
    FROM INDEX_TAXON_NAME ITN INNER JOIN
    TAXON_VERSION TV ON TV.TAXON_VERSION_KEY = ITN.TAXON_VERSION_KEY
    INNER JOIN TAXON_GROUP TG ON TG.TAXON_GROUP_KEY = TV.OUTPUT_GROUP_KEY   
    INNER JOIN #Species ON #Species.Import_Value = @Key
    WHERE (OUTPUT_TAXON_NAME = @OutputName AND PREFERRED_TAXA > 0)
    OR (#Species.MATCH_KEY = ITN.TAXON_LIST_ITEM_KEY) 
   UNION SELECT
    ITN.TAXON_LIST_ITEM_KEY AS AKey,	
    '---- ' + LTRIM(dbo.ufn_GetDeprecated(ITN.DEPRECATED,ITN.Allow_Data_Entry)+ ' ') +
    [dbo].[ufn_GetFullSpeciesName] (ITN.TAXON_LIST_ITEM_KEY) +
    ' (' + TG.TAXON_GROUP_NAME + ')'  
    AS FullDetails,ISNULL(#Species.Match_Key,'') As MatchKey, OUTPUT_TAXON_NAME, 0 AS POSSIBLE, 
    TG.Taxon_Group_Name   
    FROM INDEX_TAXON_NAME ITN INNER JOIN
    TAXON_VERSION TV ON TV.TAXON_VERSION_KEY = ITN.TAXON_VERSION_KEY
    INNER JOIN TAXON_GROUP TG ON TG.TAXON_GROUP_KEY = TV.OUTPUT_GROUP_KEY   
    INNER JOIN #Species ON #Species.Import_Value = @Key
    WHERE (OUTPUT_TAXON_NAME  LIKE(LEFT(@OutputName ,CHARINDEX(' ',@OutputName + ' ')-1)+'%') OR
    OUTPUT_TAXON_NAME  LIKE('%' + RIGHT(@OutputName,LEN(@OutputName) - CHARINDEX(' ',@OutputName))))
    AND PREFERRED_TAXA > 0
    ORDER BY POSSIBLE DESC, TAXON_GROUP_NAME, OUTPUT_TAXON_NAME
  END
  ELSE
  BEGIN
    SELECT	
    ITN.TAXON_LIST_ITEM_KEY AS AKey,	
    LTRIM(dbo.ufn_GetDeprecated(ITN.DEPRECATED,ITN.Allow_Data_Entry)+ ' ') +
    [dbo].[ufn_GetFullSpeciesName] (ITN.TAXON_LIST_ITEM_KEY) +
    ' (' + TG.TAXON_GROUP_NAME + ')'  
    AS FullDetails,ISNULL(#Species.Match_Key,'') As MatchKey, OUTPUT_TAXON_NAME, 1 AS POSSIBLE, 
    TG.Taxon_Group_Name   
    FROM INDEX_TAXON_NAME ITN INNER JOIN
    TAXON_VERSION TV ON TV.TAXON_VERSION_KEY = ITN.TAXON_VERSION_KEY
    INNER JOIN TAXON_GROUP TG ON TG.TAXON_GROUP_KEY = TV.OUTPUT_GROUP_KEY   
    INNER JOIN #Species ON #Species.Import_Value = @Key 
    WHERE OUTPUT_TAXON_NAME = @OutputName AND PREFERRED_TAXA > 0
    ORDER BY TAXON_GROUP_NAME, OUTPUT_TAXON_NAME
   END

GO
/****** Object:  StoredProcedure [dbo].[usp_IWMatchNewEntry_Name]    Script Date: 02/11/2019 15:18:17 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

/*===========================================================================*\
  Description:	Create a new individual from an import value.

  Parameters:	
	@ImportValue	The raw name to parse and insert in database.
	@EnteredBy

  Created:	June 2004

  Last revision information:
    $Revision: 7 $
    $Date: 22/02/06 10:58 $
    $Author: Johnvanbreda $
    Changed by M Weideli to bring in line with the
    parsing of names used in IW processing. Also not to save the records if they
    are not sufficiently complete. 
\*===========================================================================*/
ALTER PROCEDURE [dbo].[usp_IWMatchNewEntry_Name]
	@ImportValue varchar(100),
	@EnteredBy char(16)
AS
	DECLARE @Key char(16)
		
   UPDATE #Names set Title  = dbo.ufn_IWParseImportValue(Import_Value,1)
   UPDATE #Names set Forename  = dbo.ufn_IWParseImportValue(Import_Value,2)
   UPDATE #Names set Initials  = dbo.ufn_IWParseImportValue(Import_Value,3)
   UPDATE #Names set Surname  = dbo.ufn_IWParseImportValue(Import_Value,4)

   EXECUTE spNextKey 'Name', @Key OUTPUT

   
   INSERT INTO [Name] (	
		Name_Key, Organisation, Entered_By,Entry_Date
				) 
		SELECT @Key,0,@EnteredBy,GETDATE()
		FROM #Names WHERE 
		LEN(Surname) > 2 AND (Initials IS NOT NULL 
	    OR ForeName is not null) AND IMPORT_VALUE = @ImportValue
	    AND NOT EXISTS (SELECT * FROM INDIVIDUAL I WHERE
	    dbo.ufn_CompareNames(#Names.Title,#Names.Forename,#Names.Initials,
	    #Names.Surname,I.Title,I.FORENAME,I.INITIALS,I.SURNAME) > 13) 
	    
	    INSERT INTO Individual (
			Name_Key, Title, Forename, Initials, Surname, Entered_By,Entry_Date
		) 
		SELECT @Key,Title,Forename,Initials,Surname,@enteredBy,GETDATE()
		FROM #Names WHERE IMPORT_VALUE = @ImportValue AND 
		EXISTS (SELECT * FROM NAME WHERE NAME.NAME_KEY = @Key)       	    	
   
		UPDATE	#Names
		SET	Match_Value = dbo.ufn_GetFormattedName(@Key),
			Match_Key = @Key,
			Match_Count = 1,
			Manual_Match = 1,
			Remembered = 0
			
	    WHERE Import_Value = @ImportValue
        AND EXISTS(SELECT * FROM INDIVIDUAL WHERE 
        NAME_KEY = @Key)  	  

        UPDATE	#Names
		SET	Notes = 'Add failed. Exists or insufficient info.'
	    WHERE Import_Value = @ImportValue
        AND NOT EXISTS(SELECT * FROM INDIVIDUAL WHERE 
        NAME_KEY = @Key)  

GO
/****** Object:  StoredProcedure [dbo].[usp_IWMatchNewEntry_Location]    Script Date: 02/11/2019 15:19:26 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

/*===========================================================================*\
  Description:	Create a new location from an import value.

  Parameters:	
	@ImportValue	The name of the location.
	@EnteredBy

  Created:	June 2004

  Last revision information:
    $Revision: 1 $
    $Date: 30/07/04 15:03 $
    $Author: Johnvanbreda $

\*===========================================================================*/
ALTER PROCEDURE [dbo].[usp_IWMatchNewEntry_Location]
	@ImportValue varchar(100),
	@EnteredBy char(16)
AS

	DECLARE	@Key char(16),
		@LNKey char(16)

	/*===================================================*\
	  Now create new Location and Location Name records
	\*===================================================*/
	BEGIN TRANSACTION
		EXECUTE spNextKey 'Location', @Key OUTPUT

		INSERT INTO Location (
			Location_Key, Location_Type_Key, Entered_By,
			Spatial_Ref, Spatial_Ref_System, Lat, Long, Spatial_Ref_Qualifier
		)
		SELECT	@Key, 'NBNSYS0000000001', @EnteredBy,
			Spatial_Ref, Spatial_Ref_System, Lat, Long, Spatial_Ref_Qualifier			
		FROM	#Locations
		WHERE	Import_Value = @ImportValue

		IF @@Error <> 0 GOTO RollbackAndExit

		EXECUTE spNextKey 'Location_Name', @LNKey OUTPUT
		INSERT INTO Location_Name (
			Location_Name_Key, Item_Name, Location_Key, Preferred, Entered_By
		) VALUES (
			@LNKey, @ImportValue, @Key, 1, @EnteredBy
		)

		IF @@Error <> 0 GOTO RollbackAndExit

		-- And update import table with new data.
		UPDATE	#Locations
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
/****** Object:  StoredProcedure [dbo].[usp_Index_Taxon_Designation_Rebuild]    Script Date: 02/11/2019 22:10:18 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO


/*===========================================================================*\
  Description:      
                    Procedure which rebuilds the indexed list of Taxon Designations for
                    the supplied list of Taxon_List_Keys. Includes special processing to include Chiroptera 
                         

  Created:    January 2009

  Updated April 2018. SQl Server 2017 was taking forever to process the Chiroptera.  
  Changed Feb 2019 to use Truncate instead of Delete From 

\*===========================================================================*/
ALTER PROCEDURE [dbo].[usp_Index_Taxon_Designation_Rebuild]
AS
       DECLARE       @KeyList      VARCHAR(250)
       
       SET          @KeyList      =      ''
       
       SELECT @KeyList      =      Data
       FROM   Setting
       WHERE  Name         =      'TaxDesList'

       TRUNCATE TABLE  Index_Taxon_Designation

       INSERT INTO   Index_Taxon_Designation    (
                           Taxon_List_Item_Key,
                           Taxon_Designation_Type_Key )
       SELECT       ITN.Taxon_List_Item_Key,
                           TDES.Taxon_Designation_Type_Key
       FROM         Index_Taxon_Name                                      ITN
       INNER JOIN    Index_Taxon_Name                                      ITN2
                    ON     ITN.Recommended_Taxon_List_Item_Key             =       ITN2.Recommended_Taxon_List_Item_Key
       INNER JOIN    Index_Taxon_Group                                     ITG
                    ON     ITG.Contained_List_Item_Key                    =       ITN2.Taxon_List_Item_Key
       INNER JOIN    Index_Taxon_Name                                      ITN3
                    ON     ITN3.Taxon_List_Item_Key                       =       ITG.Taxon_List_Item_Key
       INNER JOIN    Index_Taxon_Name                                      ITN4
                    ON     ITN4.Recommended_Taxon_List_Item_Key     =       ITN3.Recommended_Taxon_List_Item_Key
       INNER JOIN    Taxon_Designation                                     TDES
                    ON     TDES.Taxon_List_Item_Key                       =       ITN4.Taxon_List_Item_Key
       INNER JOIN    Taxon_List_Version                                    TLV
                    ON     TLV.Taxon_List_Version_Key                     =       ITN4.Taxon_List_Version_Key

       WHERE        (@Keylist     LIKE '%' + TLV.Taxon_List_Key + '%'
                                 OR     @Keylist      =      ''
                                 OR     TDES.System_Supplied_Data  =      0)
                    AND    TDES.Date_To IS NULL
       GROUP BY      ITN.TAXON_LIST_ITEM_KEY,
                           TDES.TAXON_DESIGNATION_TYPE_KEY
       INSERT INTO Index_Taxon_Designation
   


 SELECT  D.Taxon_List_Item_Key, D.Taxon_Designation_Type_Key FROM 
    (SELECT Distinct iTN3.Taxon_List_Item_Key, ITD.Taxon_Designation_Type_Key from  Index_taxon_Name ITN
    INNER JOin Index_taxon_Group ITG ON ITG.Contained_List_Item_Key = ITN.Recommended_Taxon_List_Item_Key
    INNER JOIn INdex_Taxon_name ITN2 On ITN2.Taxon_List_Item_Key = ITG.Taxon_List_Item_Key AND ITN2.Actual_Name =     'Chiroptera'
    INNER JOIN Index_Taxon_Group ITG2 ON ITG2.Contained_List_Item_Key = ITn.Taxon_List_Item_Key
    INNER JOiN Index_Taxon_Designation ITD ON ITD.Taxon_List_Item_key = ITN.Taxon_List_Item_Key 
    INNER JOIN Index_taxon_Group ITG3 ON ITG3.Contained_List_Item_Key = ITN.Taxon_List_Item_Key
    INNER JOIN Index_taxon_Name ITN3 ON ITN3.RECOMMENDED_TAXON_LIST_ITEM_KEY = ITG3.TAXON_LIST_ITEM_KEY) AS D
       INNER JOIN    Taxon_List_Item TLI ON TLI.Taxon_List_Item_Key = D.Taxon_List_Item_Key
    INNER JOIN Taxon_Rank TR ON TR.TAXON_RANK_KEY = TLI.TAXON_RANK_KEY AND TR.Sequence > 99  
    AND NOT EXists (Select * FRom Index_taxon_Designation where Index_Taxon_Designation.Taxon_List_Item_Key 
    =D.Taxon_List_Item_Key and Index_Taxon_Designation.Taxon_Designation_Type_Key = D.Taxon_Designation_Type_Key)


GO
/****** Object:  StoredProcedure [dbo].[usp_IWNotes_Names_Select]    Script Date: 02/12/2019 10:43:31 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

/*===========================================================================*\
  Description:		Returns details matches where  one or more 
      possible matches is acceptable 

  Parameters: @Key = Import_Value

  Created:	November 2018 
  Changed Feb 2019 to allow for a check on key as well as name 
\*===========================================================================*/
ALTER PROCEDURE [dbo].[usp_IWNotes_Names_Select]
@Key varchar(75)
AS
   
  SELECT I.NAME_KEY AS AKey,[dbo].[FormatIndividualFull](I.Title,I.Initials,I.ForeName,I.Surname)
  AS FullDetails, ISNULL(#NAMES.Match_Key,'') As MatchKey, 1 AS POSSIBLE, I.Surname,I.Forename,I.Initials,I.Title
  FROM INDIVIDUAL I 
  INNER JOIN  #NAMES ON #NAMES.Import_Value = @Key 
  WHERE  (dbo.ufn_CompareNames(#Names.Title,#Names.Forename,#Names.Initials,
  #Names.Surname,I.TITLE,I.ForeName,I.INITIALS,I.Surname) > 7 )
  OR #NAMES.MATCH_KEY = I.NAME_KEY
  UNION SELECT
  I.NAME_KEY AS AKey, '---- ' + [dbo].[FormatIndividualFull](I.Title,I.Initials,I.ForeName,I.Surname)
  AS FullDetails, ISNULL(#NAMES.Match_Key,'') As MatchKey, 0 AS POSSIBLE,    I.Surname AS ASurname,I.Forename,I.Initials,I.Title 
  FROM INDIVIDUAL I 
  INNER JOIN  #NAMES ON #NAMES.Import_Value = @Key 
  WHERE #NAMES.SURNAME = I.SURNAME  
  ORDER BY POSSIBLE DESC,I.Forename,I.Initials,I.Title
  
GO

/****** Object:  UserDefinedFunction [dbo].[ufn_CompareNames]    Script Date: 02/13/2019 14:03:22 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

/*===========================================================================*\
  Description:	Returns a numeric value showing how well
  two names compare. Is using Title,ForeName,Initials and Surname.
  Takes into account that either of the two names sets may have null values,
  however, surnames must be the same to match - assumes that at least one
  of the surnames is not null. 
  Returns 0 if not a possible match 
		  8 surname only matching
		  9 surname + title             
		  10 surname + initials                
		  11 surname + title + initials or   surname + title + long(initials)
		  12 surname + forename 
		  13 surname + title +  forename
		  14 surname + Initial +  forename
		  15 surname + Initial + Forename  + title
		  16 surname + long Initial + Forename  + title 	  
 
  Parameters Title,Forename,Initials,Surname
  Title2,Forename2,Initials2,Surname2
       
  Created:	December 2018

  Mike Weideli:
    

\*===========================================================================*/
ALTER FUNCTION [dbo].[ufn_CompareNames]
 (@Title varchar(4),
 @Forename varchar(20),
 @Initials varchar(8),
 @Surname varchar(30),
 @Title2 varchar(4),	
 @ForeName2 varchar(20),
 @Initials2 varchar(8),
 @Surname2 varchar(30))	

RETURNS integer
AS
BEGIN
  DECLARE @Compare integer 
  Set @Compare = 0
 -- try to deal with situations around Forenames and Initials
 -- Make the initials 

 if @Initials is null and @Forename is not null set @Initials = left(@Forename,1)
 if @Initials2 is null and @Forename2 is not null set @Initials2 = left(@Forename2,1)
 
  -- If there is a Forename then Initial is often the middle initial
  
  if @Forename is not null and Left(@forename,1) <> Left(@Initials,1) 
    set @Initials = left(@Forename,1) + '.' + isnull(@Initials,'') 
    
  if @Forename2 is not null and Left(@forename2,1) <> Left(@Initials2,1) 
    set @Initials2 = left(@Forename2,1) + '.' + isnull(@Initials2,'') 
     
  -- Deal with full stop
 
  SET @Title = REPLACE(@Title,'.', '' ) 
  SET @Title2 = REPLACE(@Title2,'.', '' ) 

  SET @Title = REPLACE(@Title,'Miss', 'Ms' ) 
  SET @Title2 = REPLACE(@Title2,'Miss', 'Ms' ) 
  
  SET @Initials = REPLACE(@Initials,'.', '' ) 
  SET @Initials2 = REPLACE(@Initials2,'.', '' )

  

  -- Deal with spaces
  
  SET @Initials = REPLACE(@Initials,' ', '' ) 
  SET @Initials2 = REPLACE(@Initials2,' ', '' ) 
  
  -- Deal with different lengths of initials 
  
  If len(@Initials) > len(@Initials2)  set @Initials = left(@initials,len(@Initials2))
  If len(@Initials2) > len(@Initials)  set @Initials2 = left(@initials2,len(@Initials))
  
   -- Deal with different lengths of forename but
   -- only if over 3 characters 
  
  If len(@Forename) > len(@Forename2) AND len(@Forename2) > 3 
    set @Forename = left(@Forename,len(@Forename2))
  If len(@Forename2) > len(@Forename) AND len(@Forename) > 3 
    set @Forename2 = left(@Forename2,len(@Forename))
   
  -- The main process 
  if (isnull(@Title,'') = isnull(@Title2,'') 
    or @Title + @Title2 IS NULL)
  AND (isnull(@Initials,'') = isnull(@Initials2,'') 
    or @Initials + @Initials2 IS NULL)
  AND (isnull(@Forename,'') = isnull(@ForeName2,'')  
    or @Forename+@Forename2 IS NULL) 
  AND (ISNULL(@Surname,'') = ISNULL(@Surname2,'')) 
  BEGIN
    Set @Compare = 8 
    IF @Title= @Title2  set @Compare = @Compare + 1
    IF @Initials = @Initials2 set  @Compare = @Compare + 2
    IF @Initials = @Initials2 and len(@Initials) > 1 set  @Compare = @Compare + 1
    IF @Forename = @Forename2 set @Compare = @Compare + 4
    IF @Surname IN ('Unknown','Withheld') set @Compare = 15 
  END
  Return  @Compare   
      
  
END

