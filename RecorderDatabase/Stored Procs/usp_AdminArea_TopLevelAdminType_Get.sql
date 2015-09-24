/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_AdminArea_TopLevelAdminType_Get]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_AdminArea_TopLevelAdminType_Get]
GO

/*===========================================================================*\
  Description:	Gets the top level admin type key for an admin area

  Parameters:	None

  Created:	July 2004

  Last revision information:
    $Revision: 1 $
    $Date: 29/07/04 12:47 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_AdminArea_TopLevelAdminType_Get]
	@Key CHAR(16),
	@AdminTypeKey CHAR(16) OUTPUT	
AS

DECLARE @AdminAreaKey CHAR(16)
DECLARE @ParentKey CHAR(16)

SET @AdminAreaKey=@Key

WHILE 1=1
BEGIN
	SELECT @ParentKey=Parent
	FROM Admin_Area
	WHERE Admin_Area_Key=@AdminAreaKey

	IF @ParentKey IS NULL
		BREAK
	ELSE
		SET @AdminAreaKey=@ParentKey	
END

SELECT @AdminTypeKey=Admin_Type_Key 
FROM Admin_Area 
WHERE Admin_Area_Key=@AdminAreaKey

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_AdminArea_TopLevelAdminType_Get') AND SysStat & 0xf = 4)
BEGIN
 	PRINT 'Setting up security on procedure usp_AdminArea_TopLevelAdminType_Get'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_AdminArea_TopLevelAdminType_Get TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_AdminArea_TopLevelAdminType_Get TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_AdminArea_TopLevelAdminType_Get TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_AdminArea_TopLevelAdminType_Get TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
		GRANT EXECUTE ON dbo.usp_AdminArea_TopLevelAdminType_Get TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_AdminArea_TopLevelAdminType_Get TO [Dev - JNCC SQL]
END

GO