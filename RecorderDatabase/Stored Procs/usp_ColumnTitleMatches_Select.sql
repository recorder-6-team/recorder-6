/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ColumnTitleMatches_Select]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ColumnTitleMatches_Select]
GO

/*===========================================================================*\
  Description:	Returns a list of possible column type matches for a column title

  Parameters:	@ColumnTitle

  Created:	May 2004

  Last revision information:
    $Revision: 1 $
    $Date: 27/05/04 11:43 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ColumnTitleMatches_Select]
	@ColumnTitle VARCHAR(50)
AS

DECLARE @Count INT

-- Try getting an exact match against the user-specified matches first
SELECT @Count=Count(*) 
FROM IW_Column_Type_Pattern WHERE @ColumnTitle=Pattern AND Exclude_Match=0

IF @Count=1
	SELECT CT.IW_Column_Type_Key, CT.Item_Name
	FROM IW_Column_Type CT
	INNER JOIN IW_Column_Type_Pattern CTP ON CTP.IW_Column_Type_Key=CT.IW_Column_Type_Key
	WHERE @ColumnTitle=CTP.Pattern AND CTP.Exclude_Match=0
ELSE
	-- No match to user specified title, so use like to get pattern match
	SELECT IW_Column_Type_Key, Item_Name
	FROM IW_Column_Type
	WHERE IW_Column_Type_Key IN 
			(SELECT IW_Column_Type_Key FROM IW_Column_Type_Pattern WHERE @ColumnTitle LIKE Pattern AND Exclude_Match=0)
	AND IW_Column_Type_Key NOT IN 
			(SELECT IW_Column_Type_Key FROM IW_Column_Type_Pattern WHERE @ColumnTitle LIKE Pattern AND Exclude_Match=1)

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ColumnTitleMatches_Select') AND SysStat & 0xf = 4)
BEGIN
 	PRINT 'Setting up security on procedure usp_ColumnTitleMatches_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ColumnTitleMatches_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ColumnTitleMatches_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ColumnTitleMatches_Select TO [Dev - JNCC SQL]
END

GO