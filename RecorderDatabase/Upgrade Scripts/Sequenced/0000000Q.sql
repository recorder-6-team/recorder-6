/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_TaxonListTopLevel_Select]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_TaxonListTopLevel_Select]
GO
 
/*===========================================================================*\
  Description:	Returns the top level of a taxon dictionary list

  Parameters:	@Key	Taxon_List_Key

  Created:	Oct 2003

  Last revision information:
    $Revision: 1 $
    $Date: 23/02/04 10:06 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_TaxonListTopLevel_Select]
	@Key char(16)
AS

SET NOCOUNT ON

SELECT DISTINCT ITN.Taxon_List_Item_Key AS ListKey, ITN.Authority, ITN.Preferred_Name AS ItemName,
  ITN.Preferred_Name_Italic AS ItemNameItalic, TV.Taxon_Key AS TaxonKey, ITN.Preferred_Name AS DisplayField,
  TLI.Taxon_Rank_Key AS RankKey, TLI.Sort_Code AS SortCode, TLI.Preferred_Name AS PrefNameKey,
  TLI.System_Supplied_Data AS SystemSupplied, TV.Taxon_Version_Key AS TaxonVersionKey,
  TV.Validation_Level AS ValidationLevel, ITN.Common_Name AS CommonName, ITN.Common_Name_Italic AS CommonItalic,
  CASE When TLICount.Taxon_List_Item_Key is null Then 0 Else 1 END AS HasChildren
FROM (((Taxon_List_Item TLI
	INNER JOIN Index_Taxon_Name ITN ON ITN.Taxon_List_Item_Key = TLI.Taxon_List_Item_Key)
	INNER JOIN Taxon_Version TV ON TV.Taxon_Version_Key = TLI.Taxon_Version_Key)
	INNER JOIN Taxon_List_Version TLV ON TLV.Taxon_List_Version_Key = TLI.Taxon_List_Version_Key)
	LEFT JOIN Taxon_List_Item AS TLICount ON TLICount.Parent = TLI.Taxon_List_Item_Key
WHERE TLV.Taxon_List_Key = @Key
	AND TLI.Taxon_List_Version_To IS NULL
	AND TLI.Taxon_List_Item_Key = TLI.Preferred_Name
	AND ITN.Actual_Name = ITN.Preferred_Name
	AND TLI.Parent IS NULL

SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
GRANT EXECUTE ON [dbo].[usp_TaxonListTopLevel_Select] TO [R2K_ReadOnly]
GO
GRANT EXECUTE ON [dbo].[usp_TaxonListTopLevel_Select] TO [R2K_RecordCardsOnly]
GO
GRANT EXECUTE ON [dbo].[usp_TaxonListTopLevel_Select] TO [R2K_AddOnly]
GO
GRANT EXECUTE ON [dbo].[usp_TaxonListTopLevel_Select] TO [R2K_FullEdit]
GO
GRANT EXECUTE ON [dbo].[usp_TaxonListTopLevel_Select] TO [R2K_Administrator]
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_TaxonListChildLevel_Select]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_TaxonListChildLevel_Select]
GO
 
/*===========================================================================*\
  Description:	Returns the child level of a taxon dictionary list under a
								given parent

  Parameters:	@ParentKey - taxon list item key of parent

  Created:	Oct 2003

  Last revision information:
    $Revision: 1 $
    $Date: 23/02/04 10:06 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_TaxonListChildLevel_Select]
	@ParentKey char(16)
AS

SET NOCOUNT ON

SELECT DISTINCT ITN.Taxon_List_Item_Key AS ListKey, ITN.Authority, ITN.Preferred_Name AS ItemName,
  ITN.Preferred_Name_Italic AS ItemNameItalic, TV.Taxon_Key AS TaxonKey, ITN.Preferred_Name AS DisplayField,
  TLI.Taxon_Rank_Key AS RankKey, TLI.Sort_Code AS SortCode, TLI.Preferred_Name AS PrefNameKey,
  TLI.System_Supplied_Data AS SystemSupplied, TV.Taxon_Version_Key AS TaxonVersionKey,
  TV.Validation_Level AS ValidationLevel, ITN.Common_Name AS CommonName, ITN.Common_Name_Italic AS CommonItalic,
  CASE When TLICount.Taxon_List_Item_Key is null Then 0 Else 1 END AS HasChildren
FROM (((Taxon_List_Item TLI
INNER JOIN Index_Taxon_Name ITN ON ITN.Taxon_List_Item_Key = TLI.Taxon_List_Item_Key)
INNER JOIN Taxon_Version TV ON TV.Taxon_Version_Key = TLI.Taxon_Version_Key)
INNER JOIN Taxon_List_Version TLV ON TLV.Taxon_List_Version_Key = TLI.Taxon_List_Version_Key)
LEFT JOIN Taxon_List_Item AS TLICount ON TLICount.Parent = TLI.Taxon_List_Item_Key
WHERE TLI.Parent = @ParentKey
AND TLI.Taxon_List_Item_Key = TLI.Preferred_Name
AND ITN.Actual_Name = ITN.Preferred_Name

SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
GRANT EXECUTE ON [dbo].[usp_TaxonListChildLevel_Select] TO [R2K_ReadOnly]
GO
GRANT EXECUTE ON [dbo].[usp_TaxonListChildLevel_Select] TO [R2K_RecordCardsOnly]
GO
GRANT EXECUTE ON [dbo].[usp_TaxonListChildLevel_Select] TO [R2K_AddOnly]
GO
GRANT EXECUTE ON [dbo].[usp_TaxonListChildLevel_Select] TO [R2K_FullEdit]
GO
GRANT EXECUTE ON [dbo].[usp_TaxonListChildLevel_Select] TO [R2K_Administrator]
GO

