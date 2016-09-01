/****** Changes to alter the method by which Allow data entry is set. Now based on Redundant flag if Organism table exists ******/

/****** Object:  StoredProcedure [dbo].[usp_IndexTaxonName_ApplyNameServer]    Script Date: 08/30/2016 15:05:02 ******/
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

  Last revision August 2016 

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

-- Set Can_Expand to True

Update Index_Taxon_Name set Can_Expand = 1

-- Poulate Can_Expand in ITN

EXECUTE [dbo].[usp_Populate_Can_Expand]

-- Rebuild the lineage and sort order on the Organism table

EXECUTE [dbo].[spPopulateOrganismLineage]


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

 
EXECUTE [dbo].[usp_Populate_Index_Taxon_Hierarchy]

GO


USE [NBNData]
GO
/****** Object:  UserDefinedFunction [dbo].[ufn_FormattedSpeciesName]    Script Date: 08/30/2016 18:47:03 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER OFF
GO

/*===========================================================================*\
 * Description:	Returns the formatted species Name.
 *
 * Parameters:	@ActualName 		ITN.Actual_Name,
 *				@Authority 			ITN.Authority ,
 *				@PreferredAuthority	ITN.Authority where ITN.Taxon_List_Item_Key = TLI.Preferred_Name,
 *				@PreferredName		ITN.Preferred_Name,
 *				@ActualItalic 		ITN.Actual_Name_Italic,
 *				@PreferredItalic	ITN.Preferred_Name_Italic,
 *				@Attribute			ITN.Attribute,
 *				@RankName			ITN.Short_Name
 *              @CanExpand			BIT,
 *              @CanExpandChar      VARCHAR(50)
 * AUTHOR:	Qing Sun, Dorset Software
 * CREATED: 25/11/2008
 * Modified for Mantis 594  November 2015
\*===========================================================================*/
ALTER FUNCTION [dbo].[ufn_FormattedSpeciesName]
( @ActualName 			VARCHAR(100),
  @Authority 			VARCHAR(100),
  @PreferredAuthority	VARCHAR(100),
  @PreferredName		VARCHAR(100),
  @ActualItalic 		BIT,
  @PreferredItalic		BIT,
  @Attribute			VARCHAR(100),
  @RankName				VARCHAR(100),
  @CanExpand			BIT,
  @CantExpandSuffix       VARCHAR(50))
RETURNS varchar(200)
AS

BEGIN
	DECLARE	@FormattedName 	VARCHAR(200)
	
	SET  @FormattedName = ''
    SET  @ActualName = dbo.LCRemoveSubGenusText(@ActualName)
    IF @CanExpand = 1  SET
       @CantExpandSuffix = NULL 	
	
	IF @ActualItalic = 1
		SET @FormattedName = '<i>' + @ActualName + '</i>'
	ELSE
		SET @FormattedName = @ActualName

	SET @FormattedName = @FormattedName + ISNULL(' ' + @Attribute, '') + ISNULL(' ' + @Authority, '') + ISNULL(' [' + @RankName + ']', '')

	IF @ActualName <> @PreferredName
	BEGIN
		
		IF @PreferredItalic = 1
			SET @FormattedName = @FormattedName + ' (' + '<i>' + @PreferredName + '</i>' + ISNULL(' ' + @PreferredAuthority, '') + ')'
		ELSE
			SET @FormattedName = @FormattedName + ' (' + @PreferredName + ISNULL(' ' + @PreferredAuthority, '') + ')'
	END	
   
    SET @FormattedName = @FormattedName + ISNull(@CantExpandSuffix,'')
	
	RETURN @FormattedName
END

