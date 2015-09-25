/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Locations_Select_AllChildrenForLevel') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].usp_Locations_Select_AllChildrenForLevel
GO

/*===========================================================================*\
  Description:	Returns All locations and features below a given location.

  Parameters:
	@Key	Location key

  Created:	January 2008

  Last revision information:
    $Revision: 2 $
    $Date: 8/04/08 15:25 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE [dbo].usp_Locations_Select_AllChildrenForLevel
	@Key 	CHAR(16)
AS
	SET NOCOUNT ON

	DECLARE	@Results TABLE (
		TableName	VARCHAR(20) COLLATE Database_Default,
		ItemKey		CHAR(16) COLLATE Database_Default
	)
	
	INSERT INTO @Results
	SELECT	'Location', Location_Key
	FROM	Location
	WHERE	Location_Key = @Key
	
	-- Gather hierarchy
	WHILE @@RowCount > 0
		INSERT INTO @Results
		SELECT	'Location', Location_Key
		FROM	Location
		JOIN	@Results		ON	ItemKey = Parent_Key
		-- Don't want to loop forever!
		WHERE	Location_Key	NOT IN (SELECT ItemKey FROM @Results)
	
	INSERT INTO @Results
	SELECT	'Location_Feature', Location_Feature_Key
	FROM	Location_Feature
	JOIN	@Results			ON	ItemKey = Location_Key

	SELECT 	ItemKey, TableName
	FROM	@Results
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Locations_Select_AllChildrenForLevel') AND SysStat & 0xf = 4)
BEGIN
   	PRINT 'Setting up security on procedure usp_Locations_Select_AllChildrenForLevel'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Locations_Select_AllChildrenForLevel TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Locations_Select_AllChildrenForLevel TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Locations_Select_AllChildrenForLevel TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Locations_Select_AllChildrenForLevel TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Locations_Select_AllChildrenForLevel TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
       	GRANT EXECUTE ON dbo.usp_Locations_Select_AllChildrenForLevel TO [Dev - JNCC SQL]
END
GO
