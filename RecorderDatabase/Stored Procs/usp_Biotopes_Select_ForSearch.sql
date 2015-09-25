/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Biotopes_Select_ForSearch]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Biotopes_Select_ForSearch]
GO

/*===========================================================================*\
  Description:	Returns a list of biotopes matching a search string.

  Parameters:	@SearchText

  Created:	June 2004

  Last revision information:
    $Revision: 3 $
    $Date: 15/09/04 17:47 $
    $Author: Anthonysimpson $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Biotopes_Select_ForSearch]
	@SearchKey char(16) = NULL,
	@SearchText varchar(100)
AS
	IF @SearchKey IS NULL
		SELECT	Biotope_List_Item_Key AS Item_Key,
			IsNull(Original_Code + ', ' + Full_Term, Full_Term) AS DisplayTerm,
			IsNull(Original_Code + ', ' + Full_Term, Full_Term) AS SearchTerm
		FROM	Biotope_List_Item BLI
		JOIN	Biotope B ON B.Biotope_Key = BLI.Biotope_Key
		WHERE 	BLI.BT_CL_Version_To IS NULL
		AND	(Short_Term LIKE @SearchText + '%'
		OR	 Original_Code LIKE @SearchText + '%'
		OR	 IsNull(Original_Code + ', ' + Full_Term, Full_Term) LIKE @SearchText + '%')
		ORDER BY DisplayTerm
	ELSE
		SELECT	Biotope_List_Item_Key AS Item_Key,
			IsNull(Original_Code + ', ' + Full_Term, Full_Term) AS DisplayTerm,
			IsNull(Original_Code + ', ' + Full_Term, Full_Term) AS SearchTerm
		FROM	Biotope_List_Item BLI
		JOIN	Biotope B ON B.Biotope_Key = BLI.Biotope_Key
		JOIN	Biotope_Classification_Version BCV ON BCV.BT_CL_Version_Key = BLI.BT_CL_Version_Key
				AND BCV.Biotope_Classification_Key = @SearchKey
		WHERE 	BLI.BT_CL_Version_To IS NULL
		AND	(Short_Term LIKE @SearchText + '%'
		OR	 Original_Code LIKE @SearchText + '%'
		OR	 IsNull(Original_Code + ', ' + Full_Term, Full_Term) LIKE @SearchText + '%')
		ORDER BY DisplayTerm
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Biotopes_Select_ForSearch') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Biotopes_Select_ForSearch'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Biotopes_Select_ForSearch TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Biotopes_Select_ForSearch TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Biotopes_Select_ForSearch TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Biotopes_Select_ForSearch TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Biotopes_Select_ForSearch TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Biotopes_Select_ForSearch TO [Dev - JNCC SQL]
END
GO