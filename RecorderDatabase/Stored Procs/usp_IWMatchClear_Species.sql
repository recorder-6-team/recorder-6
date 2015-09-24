/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatchClear_Species') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_IWMatchClear_Species]
GO

/*===========================================================================*\
  Description:
	Deletes all temporary matches for the current user.

  Parameters:
	@UserID			The ID of the current user.
	@ChecklistKey	The key of the current checklist (null for all checklists).

  Created:	January	2009

  Last revision information:
    $Revision: 1 $
    $Date: 12/01/09 9:47 $
    $Author: Pauldavies $

\*===========================================================================*/

CREATE PROCEDURE [dbo].[usp_IWMatchClear_Species]
	@UserID CHAR(16),
	@ChecklistKey CHAR(16) = NULL
AS
	DELETE FROM	IW_Matched_Species
	WHERE		Temp_User_ID	=	@UserID
			AND	(Match_Checklist_Key = @ChecklistKey
			OR	@ChecklistKey IS NULL)

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatchClear_Species') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_IWMatchClear_Species'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_IWMatchClear_Species TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_IWMatchClear_Species TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_IWMatchClear_Species TO [Dev - JNCC SQL]
END
GO