/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Species_Select_ForSearch') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_Species_Select_ForSearch]
GO

/*===========================================================================*\
  Description:	Returns a list of taxa matching a search string.

  Parameters:	
		@SearchKey	Taxon List key
		@SearchText

  Created:	June 2004

  Last revision information:
    $Revision: 10 $
    $Date: 25/11/08 14:44 $
    $Author: Qingsun $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Species_Select_ForSearch]
	@SearchKey 	CHAR(16) = NULL,
	@SearchText VARCHAR(100)
AS
	IF @SearchKey IS NULL
		SELECT	ITN.Taxon_List_Item_Key                                  AS Item_Key,
				dbo.ufn_FormattedSpeciesName(	ITN.Actual_Name,
												ITN.Authority,
												ITN2.Authority,
												ITN.Preferred_Name, 
												ITN.Actual_Name_Italic,
												ITN.Preferred_Name_Italic,
												TV.Attribute,
												TR.Short_Name ) 
				+ ISNULL(' - [' + ITN.Abbreviation + ']', '')
				+ ISNULL(' - ' + TL.Item_Name, '')                       AS DisplayTerm,
				ITN.Actual_Name 
				+ ISNULL(' ' + ITN.Authority, '') 
				+ ISNULL(' - ' + TL.Item_Name, '')                       AS SearchTerm

		FROM	Index_Taxon_Name 	ITN
		INNER JOIN 	Taxon_List_Version 	TLV ON TLV.Taxon_List_Version_Key 	= ITN.Taxon_List_Version_Key
		INNER JOIN 	Taxon_List 			TL 	ON TL.Taxon_List_Key 			= TLV.Taxon_List_Key
		INNER JOIN	Taxon_List_Item 	TLI ON TLI.Taxon_List_Item_Key 		= ITN.Taxon_List_Item_Key
		INNER JOIN	Taxon_Version		TV	ON	TV.Taxon_Version_Key		= TLI.Taxon_Version_Key
		INNER JOIN	Taxon_Rank			TR	ON	TR.Taxon_Rank_Key			= TLI.Taxon_Rank_Key
		INNER JOIN	Index_Taxon_Name	ITN2  ON ITN2.Taxon_List_Item_Key	= TLI.Preferred_Name
		
		WHERE 	(ITN.Actual_Name 						LIKE @SearchText + '%'
		OR		 ITN.Actual_Name + ' ' + ITN.Authority 	LIKE @SearchText + '%'
		OR 		 ITN.Authority 							LIKE @SearchText + '%')
		AND		 TLI.Taxon_List_Version_To 				IS NULL
		AND 	 TLV.Version >= (
					SELECT 	MAX(Version) FROM Taxon_List_Version 
					WHERE 	Taxon_List_Key = (SELECT Taxon_List_Key FROM Taxon_List_Version WHERE Taxon_List_Version_Key = TLV.Taxon_List_Version_Key)
					AND		Version_Is_Amendment = 0)
		ORDER BY SearchTerm
	ELSE
		SELECT	ITN.Taxon_List_Item_Key                                  AS Item_Key,
				dbo.ufn_FormattedSpeciesName(	ITN.Actual_Name,
												ITN.Authority,
												ITN2.Authority,
												ITN.Preferred_Name, 
												ITN.Actual_Name_Italic,
												ITN.Preferred_Name_Italic,
												TV.Attribute,
												TR.Short_Name ) 
				+ ISNULL(' - [' + ITN.Abbreviation + ']', '')            AS DisplayTerm,
				ITN.Actual_Name 
				+ ISNULL(' ' + ITN.Authority, '')                        AS SearchTerm

		FROM	Index_Taxon_Name 	ITN
		INNER JOIN 	Taxon_List_Version 	TLV ON	TLV.Taxon_List_Version_Key 	= ITN.Taxon_List_Version_Key 
										AND TLV.Taxon_List_Key 			= @SearchKey
		INNER JOIN	Taxon_List_Item 	TLI ON 	TLI.Taxon_List_Item_Key 	= ITN.Taxon_List_Item_Key
		INNER JOIN	Taxon_Version		TV	ON	TV.Taxon_Version_Key		= TLI.Taxon_Version_Key
		INNER JOIN	Taxon_Rank			TR	ON	TR.Taxon_Rank_Key			= TLI.Taxon_Rank_Key
		INNER JOIN	Index_Taxon_Name	ITN2  ON ITN2.Taxon_List_Item_Key	= TLI.Preferred_Name
		
		WHERE 	(ITN.Actual_Name 						LIKE @SearchText + '%'
		OR		 ITN.Actual_Name + ' ' + ITN.Authority 	LIKE @SearchText + '%'
		OR 		 ITN.Authority 							LIKE @SearchText + '%')
		AND		 TLI.Taxon_List_Version_To 				IS NULL
		AND 	 TLV.Version >= (
					SELECT 	MAX(Version) FROM Taxon_List_Version
					WHERE 	Taxon_List_Key = (SELECT Taxon_List_Key From Taxon_List_Version WHERE Taxon_List_Version_Key = TLV.Taxon_List_Version_Key)
					AND 	Version_Is_Amendment = 0)
		ORDER BY SearchTerm
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Species_Select_ForSearch') AND SysStat & 0xf = 4)
BEGIN
   	PRINT 'Setting up security on procedure usp_Species_Select_ForSearch'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
       	GRANT EXECUTE ON dbo.usp_Species_Select_ForSearch TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Species_Select_ForSearch TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Species_Select_ForSearch TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Species_Select_ForSearch TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Species_Select_ForSearch TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
       	GRANT EXECUTE ON dbo.usp_Species_Select_ForSearch TO [Dev - JNCC SQL]
END
GO