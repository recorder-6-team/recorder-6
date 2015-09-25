If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Terms_Select_ForSearch]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Terms_Select_ForSearch]
GO

/*===========================================================================*\
  Description: 	Search proc for Term table.

  Parameters:	@SearchText
		@SearchKey	Language_key

  Created:	December 2003

  Last revision information:
    $Revision: 6 $
    $Date: 20/02/07 12:06 $
    $Author: Ericsalmon $

\*===========================================================================*/

CREATE PROCEDURE [dbo].[usp_Terms_Select_ForSearch] 
	@SearchText varchar(100),
	@SearchKey varchar(4) = NULL
AS

SET NOCOUNT ON
	-- NB: This proc does want to search on Item_Name (as opposed to 
	-- Plaintext). This is because as we are dealing with terms, we could
	-- have two terms with the same Plaintext, but different Item_Name
	-- i.e. one is italic, one isn't.

	IF @SearchKey IS NOT NULL 
		SELECT 
				T.Term_Key AS Item_Key,
				Item_Name AS DisplayTerm,
				Item_Name AS SearchTerm,
				Language_Key
		FROM		Term AS T
		WHERE		PlainText LIKE @SearchText + '%'  -- Still want to filter on PlainText though
		AND 		Language_Key = @SearchKey
		ORDER BY 	Plaintext
	ELSE
		SELECT 
				T.Term_Key AS Item_Key,
				Item_Name AS DisplayTerm,
				Item_Name AS SearchTerm,
				Language_Key
		FROM		Term AS T
		WHERE		PlainText LIKE @SearchText + '%'  -- Still want to filter on PlainText though
		ORDER BY 	Plaintext
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Terms_Select_ForSearch') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Terms_Select_ForSearch'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Terms_Select_ForSearch TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Terms_Select_ForSearch TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Terms_Select_ForSearch TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Terms_Select_ForSearch TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Terms_Select_ForSearch TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Terms_Select_ForSearch TO [Dev - JNCC SQL]
END

GO
