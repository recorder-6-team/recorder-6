/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_UserAccessLevel_Get]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_UserAccessLevel_Get]
GO

/*===========================================================================*\
  Description:	Takes a User key and returns the Access Level.
			1 = read only, 
			2 = record cards only, 
			3 = read/write, 
			4 = full user, 
			5 = system manager.  	
 
  Parameters:	@Key  
		@AccessLevel OUTPUT

  Created:	June 2004

  Last revision information:
    $Revision: 1 $
    $Date: 9/06/04 12:28 $
    $Author: Anthonysimpson $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_UserAccessLevel_Get] 
	@Key CHAR(16),
	@AccessLevel Int OUTPUT
AS

SET NOCOUNT ON

	SELECT 	@AccessLevel = Security_Level
	FROM	[User]
	WHERE	Name_key = @Key

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_UserAccessLevel_Get') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_UserAccessLevel_Get'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_UserAccessLevel_Get TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_UserAccessLevel_Get TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_UserAccessLevel_Get TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_UserAccessLevel_Get TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_UserAccessLevel_Get TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_UserAccessLevel_Get TO [Dev - JNCC SQL]
END

GO
