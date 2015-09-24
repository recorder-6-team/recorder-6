/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_LocationKeyFromVCNumber_Get]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_LocationKeyFromVCNumber_Get]
GO

/*===========================================================================*\
  Description:	Tests if a vice county number is recognised and returns the 
		location_Key.  Used by import wizard.

  Parameters:	@Value - vice county number to test
		@Location_Key - output.  Null if not found.

  Created:	May 2004

  Last revision information:
    $Revision: 1 $
    $Date: 26/05/04 12:21 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_LocationKeyFromVCNumber_Get]
  @Value VARCHAR(20),
	@Location_Key CHAR(16) OUTPUT
AS

SELECT @Location_Key=Location_Key FROM Location WHERE File_Code=@Value AND Location_Type_Key='JNCCIMPW00000001'

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_LocationKeyFromVCNumber_Get') AND SysStat & 0xf = 4)
BEGIN
 	PRINT 'Setting up security on procedure usp_LocationKeyFromVCNumber_Get'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_LocationKeyFromVCNumber_Get TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_LocationKeyFromVCNumber_Get TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_LocationKeyFromVCNumber_Get TO [Dev - JNCC SQL]
END

GO