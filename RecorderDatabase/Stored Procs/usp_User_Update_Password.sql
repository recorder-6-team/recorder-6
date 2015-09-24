IF Object_ID('dbo.usp_User_Update_Password') IS NOT NULL
	DROP PROCEDURE dbo.usp_User_Update_Password
GO

/*===========================================================================*\
  Description:
	Changes the password of the given user.

  Parameters:
	@NameKey	Key of user.
	@Pwd		User's password.

  Created:	July 2009

  Last revision information:
    $Revision: 1 $
    $Date: 16/07/09 11:47 $
    $Author: Ericsalmon $

\*===========================================================================*/

CREATE PROCEDURE dbo.usp_User_Update_Password
	@NameKey	CHAR(16),
	@Password	VARCHAR(20)
AS
	SET NOCOUNT ON

	UPDATE	"User"
	SET		Password = @Password
	WHERE	Name_Key = @NameKey
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
PRINT 'Setting up security on procedure '
IF EXISTS (SELECT * FROM SysUsers WHERE NAME = 'R2k_AddOnly')
   	GRANT EXECUTE ON dbo.usp_User_Update_Password TO R2k_AddOnly
IF EXISTS (SELECT * FROM SysUsers WHERE NAME = 'R2k_Administrator')
	GRANT EXECUTE ON dbo.usp_User_Update_Password TO R2k_Administrator
IF EXISTS (SELECT * FROM SysUsers WHERE NAME = 'R2k_FullEdit')
	GRANT EXECUTE ON dbo.usp_User_Update_Password TO R2k_FullEdit
IF EXISTS (SELECT * FROM SysUsers WHERE NAME = 'R2k_ReadOnly')
	GRANT EXECUTE ON dbo.usp_User_Update_Password TO R2k_ReadOnly
IF EXISTS (SELECT * FROM SysUsers WHERE NAME = 'R2k_RecordCardsOnly')
	GRANT EXECUTE ON dbo.usp_User_Update_Password TO R2k_RecordCardsOnly
IF EXISTS (SELECT * FROM SysUsers WHERE NAME = 'Dev - JNCC SQL')
   	GRANT EXECUTE ON dbo.usp_User_Update_Password TO "Dev - JNCC SQL"
GO