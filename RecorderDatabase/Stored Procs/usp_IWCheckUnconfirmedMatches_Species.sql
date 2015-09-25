/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWCheckUnconfirmedMatches_Species') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_IWCheckUnconfirmedMatches_Species]
GO

/*===========================================================================*\
  Description:
	Checks whether there are any unconfirmed matches for this checklist.

  Parameters:
	@UserID			The Name_Key of the current user.
	@FoundMatches	An output boolean which is set to true if matches are found,
					and false otherwise.

  Created:	January 2009

  Last revision information:
    $Revision: 1 $
    $Date: 12/01/09 9:47 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_IWCheckUnconfirmedMatches_Species]
	@UserID CHAR(16),
	@FoundMatches BIT OUT
AS
	If EXISTS (
		SELECT	Temp_User_ID
		FROM	IW_Matched_Species
		LEFT JOIN #Species
			ON	(Checklist_Key	=	Match_Checklist_Key
				OR (Checklist_Key IS NULL AND Match_Checklist_Key IS NULL))
			AND	Match_Key		=	Matched_Key
			AND Import_Value	=	Matched_Value
		WHERE	Temp_User_ID	=	@UserID
			AND	Match_Key IS NOT NULL -- Only want temporary matches that are currently displayed
	)  
		SET @FoundMatches = 1
	ELSE
		SET @FoundMatches = 0
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWCheckUnconfirmedMatches_Species') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_IWCheckUnconfirmedMatches_Species'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_IWCheckUnconfirmedMatches_Species TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_IWCheckUnconfirmedMatches_Species TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_IWCheckUnconfirmedMatches_Species TO [Dev - JNCC SQL]
END
GO