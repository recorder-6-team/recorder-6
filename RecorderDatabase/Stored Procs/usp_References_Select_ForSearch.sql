/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_References_Select_ForSearch') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_References_Select_ForSearch]
GO

/*===========================================================================*\
  Description:	Returns a list of references matching a search string.

  Parameters:	@SearchText

  Created:	June 2004

  Last revision information:
    $Revision: 1 $
    $Date: 23/06/04 16:44 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_References_Select_ForSearch]
	@SearchText varchar(500)
AS
	SELECT	R.Source_Key AS Item_Key,
		dbo.ufn_GetFormattedReferenceName(R.Source_Key) AS DisplayTerm, 
		dbo.ufn_GetFormattedReferenceName(R.Source_Key) AS SearchTerm
	FROM	Reference R
	JOIN	vw_Reference_Authors RA ON RA.Source_Key = R.Source_Key
	WHERE 	Cast(dbo.FormatDatePart(Year_Vague_date_Start, Year_Vague_Date_End, Year_Vague_Date_Type, 0) AS varchar(50)) LIKE @SearchText + '%'
	OR	Cast(Title AS varchar(1000)) LIKE Replace(@SearchText, ' ', '%') + '%'
	OR 	Author LIKE @SearchText + '%'
	ORDER BY DisplayTerm
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_References_Select_ForSearch') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_References_Select_ForSearch'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_References_Select_ForSearch TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_References_Select_ForSearch TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_References_Select_ForSearch TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_References_Select_ForSearch TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_References_Select_ForSearch TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_References_Select_ForSearch TO [Dev - JNCC SQL]
END
GO