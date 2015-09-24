IF Object_ID('dbo.usp_User_Update_FirstLogin') IS NOT NULL
	DROP PROCEDURE dbo.usp_User_Update_FirstLogin
GO

/*===========================================================================*\
  Description:
	Sets the First_Login flag to false for the given user.

  Parameters:
	@NameKey	Key of user.
	
  Created:	July 2009

  Last revision information:
    $Revision: 2 $
    $Date: 17/07/09 16:26 $
    $Author: Ericsalmon $

\*===========================================================================*/

CREATE PROCEDURE dbo.usp_User_Update_FirstLogin
	@NameKey	CHAR(16)
AS
	SET NOCOUNT ON

	UPDATE	"User"
	SET		First_Login = 0
	WHERE	Name_Key = @NameKey
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
PRINT 'Setting up security on procedure '
IF EXISTS (SELECT * FROM SysUsers WHERE NAME = 'R2k_AddOnly')
   	GRANT EXECUTE ON dbo.usp_User_Update_FirstLogin TO R2k_AddOnly
IF EXISTS (SELECT * FROM SysUsers WHERE NAME = 'R2k_Administrator')
	GRANT EXECUTE ON dbo.usp_User_Update_FirstLogin TO R2k_Administrator
IF EXISTS (SELECT * FROM SysUsers WHERE NAME = 'R2k_FullEdit')
	GRANT EXECUTE ON dbo.usp_User_Update_FirstLogin TO R2k_FullEdit
IF EXISTS (SELECT * FROM SysUsers WHERE NAME = 'R2k_ReadOnly')
	GRANT EXECUTE ON dbo.usp_User_Update_FirstLogin TO R2k_ReadOnly
IF EXISTS (SELECT * FROM SysUsers WHERE NAME = 'R2k_RecordCardsOnly')
	GRANT EXECUTE ON dbo.usp_User_Update_FirstLogin TO R2k_RecordCardsOnly
IF EXISTS (SELECT * FROM SysUsers WHERE NAME = 'Dev - JNCC SQL')
   	GRANT EXECUTE ON dbo.usp_User_Update_FirstLogin TO "Dev - JNCC SQL"
GO