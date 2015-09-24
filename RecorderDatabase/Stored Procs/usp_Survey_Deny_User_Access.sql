/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Survey_Deny_User_Access') AND SysStat & 0xf = 4)
    DROP PROCEDURE [dbo].[usp_Survey_Deny_User_Access]
GO

/*===========================================================================*\
  Description:		
	Denies access to the specified survey for the specified user.

  Parameters:	
	@User_Name_Key CHAR(16) The Name_Key of the User.
	@SurveyKey CHAR(16) The Survey_Key of the Survey to deny access to.

  Created:	December 2008

  Last revision information:
    $Revision: 3 $
    $Date: 7/01/09 15:30 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Survey_Deny_User_Access]
	@User_Name_Key	CHAR(16),
	@Survey_Key		CHAR(16)
AS
	SET NOCOUNT ON

	DECLARE @Key	CHAR(16)

	EXECUTE spNextKey 'User_Survey_Restriction', @Key OUTPUT

	INSERT INTO User_Survey_Restriction (
		User_Survey_Restriction_Key,
		Name_Key, 
		Survey_Key
	)
    VALUES (
		@Key,
		@User_Name_Key, 
		@Survey_Key
	)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Survey_Deny_User_Access') AND SysStat & 0xf = 4)
BEGIN
   	PRINT 'Setting up security on procedure usp_Survey_Deny_User_Access'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Survey_Deny_User_Access TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
       	GRANT EXECUTE ON dbo.usp_Survey_Deny_User_Access TO [Dev - JNCC SQL]
END
GO
