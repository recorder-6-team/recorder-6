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
    $Date: 9-05-08 11:35 $
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
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Species_Select_ForSearchByList_Partial') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_Species_Select_ForSearchByList_Partial]
GO

/*===========================================================================*\
  Description:	Returns a list of taxa matching a search string,
					using partial taxon matching.

  Parameters:	@SearchText

  Created:	January 2008

  Last revision information:
    $Revision: 1 $
    $Date: 9-05-08 11:35 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Species_Select_ForSearchByList_Partial]
	@SearchKey 	CHAR(16),
	@SearchText VARCHAR(100)
AS
	SET @SearchText = dbo.[ufn_GetPartialTaxonSearchText](@SearchText)

	SELECT	ITN.Taxon_List_Item_Key                                  AS Item_Key,
			dbo.ufn_GetFormattedSpeciesName(ITN.Taxon_List_Item_Key) 
			+ ISNULL(' - [' + ITN.Abbreviation + ']', '')            AS DisplayTerm, 
			Actual_Name + ' ' 
			+ ISNULL(ITN.Authority, '')                              AS SearchTerm

	FROM	Index_Taxon_Name 	ITN
	JOIN 	Taxon_List_Version 	TLV ON 	TLV.Taxon_List_Version_Key 	= ITN.Taxon_List_Version_Key 
									AND TLV.Taxon_List_Key 			= @SearchKey
	JOIN	Taxon_List_Item 	TLI ON 	TLI.Taxon_List_Item_Key 	= ITN.Taxon_List_Item_Key

	WHERE 	(Actual_Name 						LIKE @SearchText
	OR		Actual_Name + ' ' + ITN.Authority 	LIKE @SearchText
	OR 		ITN.Authority 						LIKE @SearchText)
	AND		TLI.Taxon_List_Version_To 			IS NULL
	AND 	TLV.Version >= (SELECT MAX(Version)
		FROM Taxon_List_Version
		WHERE Taxon_List_Key = (SELECT Taxon_List_Key From Taxon_List_Version
		WHERE Taxon_List_Version_Key = TLV.Taxon_List_Version_Key)
		AND Version_Is_Amendment = 0)
	ORDER BY SearchTerm
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Species_Select_ForSearchByList_Partial') AND SysStat & 0xf = 4)
BEGIN
   	PRINT 'Setting up security on procedure usp_Species_Select_ForSearchByList_Partial'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
       	GRANT EXECUTE ON dbo.usp_Species_Select_ForSearchByList_Partial TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Species_Select_ForSearchByList_Partial TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Species_Select_ForSearchByList_Partial TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Species_Select_ForSearchByList_Partial TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Species_Select_ForSearchByList_Partial TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
       	GRANT EXECUTE ON dbo.usp_Species_Select_ForSearchByList_Partial TO [Dev - JNCC SQL]
END
GO