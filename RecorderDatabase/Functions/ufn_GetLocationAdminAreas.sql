/*===========================================================================*\
  Drop function before re-creating.
\*===========================================================================*/
IF Object_Id(N'dbo.ufn_GetLocationAdminAreas') IS NOT NULL
	DROP FUNCTION ufn_GetLocationAdminAreas
GO

/*===========================================================================*\
  Description:	
		Gets a concatenated string of Admin Areas for a Location.

  Parameters:	
		@Location_Key 		The key of the Location of interest.

  Created:	April 2009

  Last revision information:
    $Revision: 1 $
    $Date: 3/04/09 13:51 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE FUNCTION dbo.ufn_GetLocationAdminAreas
(
	@Location_Key		CHAR(16)
)
RETURNS	VARCHAR(1000)

AS
BEGIN
	DECLARE @Seperator	VARCHAR(100)
	
	-- Gets the Seperator from the settings table.
	SELECT	@Seperator	=	Data
	FROM	Setting
	WHERE	Name		=	'DBListSep' 
	
	DECLARE	@ReturnValue	VARCHAR(1000)
	
	SELECT		@ReturnValue	=	
					-- Blank when this is the first value, otherwise
					-- the previous string plus the seperator
					CASE
						WHEN @ReturnValue IS NULL THEN ''
						ELSE @ReturnValue + @Seperator
					END + AA.Item_Name
	FROM		Location_Admin_Areas		LAA
	INNER JOIN	Admin_Area				AA
			ON	LAA.Admin_Area_Key		=	AA.Admin_Area_Key
	WHERE		LAA.Location_Key		=	@Location_Key
	
	RETURN	@ReturnValue
END
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * 
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[ufn_GetLocationAdminAreas]')
	   AND    Type IN ('FN', 'IF', 'TF'))
	BEGIN
    	PRINT 'Setting up security on function ufn_GetLocationAdminAreas'
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
	        	GRANT EXECUTE ON dbo.ufn_GetLocationAdminAreas TO [R2k_AddOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
			GRANT EXECUTE ON dbo.ufn_GetLocationAdminAreas TO [R2k_Administrator]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
			GRANT EXECUTE ON dbo.ufn_GetLocationAdminAreas TO [R2k_FullEdit]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
			GRANT EXECUTE ON dbo.ufn_GetLocationAdminAreas TO [R2k_ReadOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
			GRANT EXECUTE ON dbo.ufn_GetLocationAdminAreas TO [R2k_RecordCardsOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
	        GRANT EXECUTE ON dbo.ufn_GetLocationAdminAreas TO [Dev - JNCC SQL]
	END
GO