/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Session_Insert]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Session_Insert]
GO

/*===========================================================================*\
  Description:	Inserts a new Session record and generates a SessionID value.

  Parameters:	@Key - output SessionID
		@UserID

  Created:	Nov 2003

  Last revision information:
    $Revision: 2 $
    $Date: 12/11/03 16:39 $
    $Author: Anthonysimpson $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Session_Insert]
	@Key char(16) OUTPUT,
	@UserID char(16)
AS

EXECUTE spNextKey 'Session', @Key OUTPUT

INSERT INTO Session (Session_ID, User_Name_Key)
VALUES (@Key, @UserID)

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Session_Insert') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Session_Insert'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Session_Insert TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Session_Insert TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Session_Insert TO [R2k_FullEdit]
	-- ReadOnly users have permission to run this stored proc because they will need a SessionID
	-- and this is the proc that actually generates the SessionID.
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Session_Insert TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Session_Insert TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Session_Insert TO [Dev - JNCC SQL]
END

GO