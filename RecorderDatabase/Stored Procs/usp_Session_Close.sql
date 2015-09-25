/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Session_Close]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Session_Close]
GO

/*===========================================================================*\
  Description: Records the time that a session was closed

  Parameters:	@SessionID

  Created:	Nov 2003

  Last revision information:
    $Revision: 3 $
    $Date: 12/05/04 9:57 $
    $Author: Anthonysimpson $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Session_Close]
	@SessionID char(16)
AS

UPDATE Session SET Date_Time_End=GetDate() WHERE Session_ID=@SessionID

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Session_Close') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Session_Close'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Session_Close TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Session_Close TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Session_Close TO [R2k_FullEdit]
	-- ReadOnly users have permission to run this stored proc because they need to shut
	-- the Session for their SessionID.
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Session_Close TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Session_Close TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Session_Close TO [Dev - JNCC SQL]
END

GO