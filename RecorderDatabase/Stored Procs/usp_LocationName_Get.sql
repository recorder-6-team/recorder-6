If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_LocationName_Get]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_LocationName_Get] 
GO

/*===========================================================================*\
  Description:	Gets the Location Name given a location key

  Parameters:	@Key
		@Caption OUTPUT	

  Created:	November 2003

  Last revision information:
    $Revision: 2 $
    $Date: 6/07/04 13:41 $
    $Author: Johnvanbreda $

\*===========================================================================*/

CREATE PROCEDURE [dbo].[usp_LocationName_Get] 
@Key CHAR(16),
@Caption VARCHAR(100) OUTPUT

AS
	SET NOCOUNT ON

	SELECT		@Caption = Item_Name
	FROM		Location_Name AS LN
	INNER JOIN	Location AS L ON L.Location_Key = LN.Location_Key
	WHERE		L.Location_Key = @Key
	AND		LN.Preferred = 1
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_LocationName_Get') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_LocationName_Get'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_LocationName_Get TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_LocationName_Get TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_LocationName_Get TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_LocationName_Get TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_LocationName_Get TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_LocationName_Get TO [Dev - JNCC SQL]
END

GO