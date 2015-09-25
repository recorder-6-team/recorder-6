/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_IndexTaxonName_ApplyNameServer]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_IndexTaxonName_ApplyNameServer]
GO

/*===========================================================================*\
  Description: Applies the NameServer information to the Index_Taxon_Name
		Recommended_Taxon_List_Item_Key table.  Updates all records where this value
		is null.

  Parameters:	None

  Created:	November 2004

  Last revision information:
    $Revision: 1 $
    $Date: 4/11/05 10:24 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_IndexTaxonName_ApplyNameServer]
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

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IndexTaxonName_ApplyNameServer') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_IndexTaxonName_ApplyNameServer'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_IndexTaxonName_ApplyNameServer TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_IndexTaxonName_ApplyNameServer TO [Dev - JNCC SQL]
END
GO