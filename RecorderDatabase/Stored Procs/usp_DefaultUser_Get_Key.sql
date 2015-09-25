IF Object_ID('dbo.usp_DefaultUser_Get_Key') IS NOT NULL
	DROP PROCEDURE dbo.usp_DefaultUser_Get_Key
GO

/*===========================================================================*\
  Description:
	Retrieves the key for Default User.

  Parameters:

  Created:	July 2009

  Last revision information:
    $Revision: 1 $
    $Date: 16/07/09 11:47 $
    $Author: Ericsalmon $

\*===========================================================================*/

CREATE PROCEDURE dbo.usp_DefaultUser_Get_Key
AS
	SET NOCOUNT ON

	SELECT	U.Name_Key 
	FROM	"User"		U
	JOIN	"Name"		N ON N.Name_Key = U.Name_Key
	JOIN	Individual	I ON I.Name_Key = N.Name_Key
	WHERE	I.Forename = 'Default' 
	AND		I.Surname  = 'User'
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
PRINT 'Setting up security on procedure '
IF EXISTS (SELECT * FROM SysUsers WHERE NAME = 'R2k_AddOnly')
   	GRANT EXECUTE ON dbo.usp_DefaultUser_Get_Key TO R2k_AddOnly
IF EXISTS (SELECT * FROM SysUsers WHERE NAME = 'R2k_Administrator')
	GRANT EXECUTE ON dbo.usp_DefaultUser_Get_Key TO R2k_Administrator
IF EXISTS (SELECT * FROM SysUsers WHERE NAME = 'R2k_FullEdit')
	GRANT EXECUTE ON dbo.usp_DefaultUser_Get_Key TO R2k_FullEdit
IF EXISTS (SELECT * FROM SysUsers WHERE NAME = 'R2k_ReadOnly')
	GRANT EXECUTE ON dbo.usp_DefaultUser_Get_Key TO R2k_ReadOnly
IF EXISTS (SELECT * FROM SysUsers WHERE NAME = 'R2k_RecordCardsOnly')
	GRANT EXECUTE ON dbo.usp_DefaultUser_Get_Key TO R2k_RecordCardsOnly
IF EXISTS (SELECT * FROM SysUsers WHERE NAME = 'Dev - JNCC SQL')
   	GRANT EXECUTE ON dbo.usp_DefaultUser_Get_Key TO "Dev - JNCC SQL"
GO