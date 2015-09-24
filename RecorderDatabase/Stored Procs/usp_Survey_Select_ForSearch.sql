If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Survey_Select_ForSearch]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Survey_Select_ForSearch] 
GO

CREATE PROCEDURE [dbo].[usp_Survey_Select_ForSearch] 
@SearchText VARCHAR(100)

AS
/*===========================================================================*\
  Description:	Proc for the search dialog window that searched for survey 
		names and their authors. 

  Parameters:	@SearchText

  Created:	October 2003

  Last revision information:
    $Revision: 3 $
    $Date: 6/07/04 13:42 $
    $Author: Johnvanbreda $

\*===========================================================================*/

SET NOCOUNT ON

SELECT 	SV.Survey_Key AS Item_Key, 
	SV.Item_Name + ' - ' + dbo.ufn_GetFormattedName(SV.Run_By) AS DisplayTerm,
	SV.Item_Name + ' - ' + dbo.ufn_GetFormattedName(SV.Run_By) AS SearchTerm
FROM 	Survey AS SV
WHERE 	SV.Item_Name + ' - ' + dbo.ufn_GetFormattedName(SV.Run_By) LIKE @SearchText + '%'
ORDER BY DisplayTerm

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Survey_Select_ForSearch') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Survey_Select_ForSearch'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Survey_Select_ForSearch TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Survey_Select_ForSearch TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Survey_Select_ForSearch TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Survey_Select_ForSearch TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Survey_Select_ForSearch TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Survey_Select_ForSearch TO [Dev - JNCC SQL]
END

GO