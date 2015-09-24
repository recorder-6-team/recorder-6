/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF Object_Id('dbo.usp_Locations_Select_ForSearch') IS NOT NULL
    DROP PROCEDURE dbo.usp_Locations_Select_ForSearch
GO

/*===========================================================================*\
  Description:
	Returns a list of individual names matching a search string.

  Parameters:
	@SearchText

  Created:	August 2003

  Last revision information:
    $Revision: 1 $
    $Date: 21/07/09 10:06 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE dbo.usp_Locations_Select_ForSearch
	@SearchText VARCHAR(100)
AS
	SET NO_BROWSETABLE OFF

	SELECT		LN.Location_Key AS Item_Key, 
				LN.Item_Name	AS DisplayTerm, 
				LN.Item_Name	AS SearchTerm,
				L.Spatial_Ref,
				L.Spatial_Ref_System,
				L.Lat,
				L.Long,
				L.Location_Type_Key,
				L.File_Code,
				LT.Short_Name	AS Location_Type
	FROM		Location_Name	LN
	JOIN		Location		L	ON L.Location_Key		= LN.Location_Key
	JOIN		Location_Type	LT	ON LT.Location_Type_Key = L.Location_Type_Key
	WHERE 		LN.Item_Name LIKE @SearchText + '%'
	ORDER BY 	DisplayTerm
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
PRINT 'Setting up security on procedure usp_Locations_Select_ForSearch'
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
   	GRANT EXECUTE ON dbo.usp_Locations_Select_ForSearch TO R2k_AddOnly
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
	GRANT EXECUTE ON dbo.usp_Locations_Select_ForSearch TO R2k_Administrator
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
	GRANT EXECUTE ON dbo.usp_Locations_Select_ForSearch TO R2k_FullEdit
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
	GRANT EXECUTE ON dbo.usp_Locations_Select_ForSearch TO R2k_ReadOnly
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
	GRANT EXECUTE ON dbo.usp_Locations_Select_ForSearch TO R2k_RecordCardsOnly
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
   	GRANT EXECUTE ON dbo.usp_Locations_Select_ForSearch TO "Dev - JNCC SQL"
GO