If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Biotope_Select_ForSearch]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Biotope_Select_ForSearch]
GO

CREATE PROCEDURE [dbo].[usp_Biotope_Select_ForSearch] 
@SearchText VARCHAR(100)

AS
/*===========================================================================*\
  Description:	Proc. for searching on the Biotope names

  Parameters:	@SearchText

  Created:	June 2004

  Last revision information:
    $Revision: 1 $
    $Date: 8/06/04 10:44 $
    $Author: Anthonysimpson $

\*===========================================================================*/

SET NOCOUNT ON

	SELECT 		Biotope_List_Item_Key AS Item_Key,
			IsNull(Original_Code + ' - ' + Full_Term, Full_Term) AS DisplayTerm,
			IsNull(Original_Code + ' - ' + Full_Term, Full_Term) AS SearchTerm
	FROM 		Biotope AS B
	INNER JOIN	Biotope_List_Item AS BLI ON BLI.Biotope_Key = B.Biotope_Key
	WHERE 		(Original_Code + ' - ' + Full_Term LIKE @SearchText + '%')
	OR		(Full_Term LIKE @SearchText + '%')
	ORDER BY 	IsNull(Original_Code + ' - ' + Full_Term, Full_Term)

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Biotope_Select_ForSearch') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Biotope_Select_ForSearch'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Biotope_Select_ForSearch TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Biotope_Select_ForSearch TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Biotope_Select_ForSearch TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Biotope_Select_ForSearch TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Biotope_Select_ForSearch TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Biotope_Select_ForSearch TO [Dev - JNCC SQL]
END

GO