/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatchRecord_Species') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_IWMatchRecord_Species]
GO

/*===========================================================================*\
  Description:	Populate remembered matches table for future imports.

  Parameters:
	@UserID			The ID of the current User.

  Created:	July 2004

  Last revision information:
    $Revision: 5 $
    $Date: 12/01/09 9:49 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_IWMatchRecord_Species]
	@UserID			char(16)
AS
	-- Removes existing matches which have a replacement.
	DELETE		MS1
	FROM		IW_Matched_Species		MS1
	INNER JOIN	IW_Matched_Species		MS2
			ON	MS2.Temp_User_ID		=	@UserID
			AND	MS1.Match_Checklist_Key	=	MS2.Match_Checklist_Key
			AND	MS1.Matched_Value		=	MS2.Matched_Value
	WHERE		MS1.Temp_User_ID		IS	NULL
	
	-- Makes the temporary changes this user made permenant.
	UPDATE	IW_Matched_Species
	SET		Temp_User_ID	=	NULL
	WHERE	Temp_User_ID	=	@UserID
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatchRecord_Species') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_IWMatchRecord_Species'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_IWMatchRecord_Species TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_IWMatchRecord_Species TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_IWMatchRecord_Species TO [Dev - JNCC SQL]
END
GO