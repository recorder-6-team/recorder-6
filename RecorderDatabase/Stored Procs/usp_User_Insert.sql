IF Object_ID('dbo.usp_User_Insert') IS NOT NULL
	DROP PROCEDURE dbo.usp_User_Insert
GO

/*===========================================================================*\
  Description:
	Inserts a new user record.

  Parameters:
	@NameKey			Key of user.
	@Password			User's password.
	@SecurityLevel		Level to grant to the user.
	@FullEditOwnData	Flag.

  Created:	July 2009

  Last revision information:
    $Revision: 1 $
    $Date: 16/07/09 11:47 $
    $Author: Ericsalmon $

\*===========================================================================*/

CREATE PROCEDURE dbo.usp_User_Insert
	@NameKey			CHAR(16),
	@Password			VARCHAR(20),
	@SecurityLevel		TINYINT,
	@FullEditOwnData	BIT
AS
	SET NOCOUNT ON

	INSERT INTO "User"(
		Name_Key,
		Password,
		Security_Level,
		Full_Edit_Own_Data
	) VALUES (
		@NameKey,
		@Password,
		@SecurityLevel,
		@FullEditOwnData
	)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
PRINT 'Setting up security on procedure '
IF EXISTS (SELECT * FROM SysUsers WHERE NAME = 'R2k_Administrator')
	GRANT EXECUTE ON dbo.usp_User_Insert TO R2k_Administrator
IF EXISTS (SELECT * FROM SysUsers WHERE NAME = 'Dev - JNCC SQL')
   	GRANT EXECUTE ON dbo.usp_User_Insert TO "Dev - JNCC SQL"
GO