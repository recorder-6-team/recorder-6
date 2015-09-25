IF Object_ID('dbo.usp_Setting_Get') IS NOT NULL
	DROP PROCEDURE dbo.usp_Setting_Get
GO

/*===========================================================================*\
  Description:
	Returns a setting entry.

  Parameters:
	@Name	The name of the setting to retrieve.
	@Value	The value of the entry, as an OUTPUT.

  Created:	July 2009

  Last revision information:
    $Revision: 1 $
    $Date: 20/07/09 11:34 $
    $Author: Ericsalmon $

\*===========================================================================*/

CREATE PROCEDURE dbo.usp_Setting_Get
	@Name	VARCHAR(20),
	@Value	VARCHAR(250) OUTPUT

AS
	SET NOCOUNT ON

	SELECT	@Value = Data
	FROM	Setting
	WHERE	Name = @Name
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
PRINT 'Setting up security on procedure '
IF EXISTS (SELECT * FROM SysUsers WHERE NAME = 'R2k_AddOnly')
   	GRANT EXECUTE ON dbo.usp_Setting_Get TO R2k_AddOnly
IF EXISTS (SELECT * FROM SysUsers WHERE NAME = 'R2k_Administrator')
	GRANT EXECUTE ON dbo.usp_Setting_Get TO R2k_Administrator
IF EXISTS (SELECT * FROM SysUsers WHERE NAME = 'R2k_FullEdit')
	GRANT EXECUTE ON dbo.usp_Setting_Get TO R2k_FullEdit
IF EXISTS (SELECT * FROM SysUsers WHERE NAME = 'R2k_ReadOnly')
	GRANT EXECUTE ON dbo.usp_Setting_Get TO R2k_ReadOnly
IF EXISTS (SELECT * FROM SysUsers WHERE NAME = 'R2k_RecordCardsOnly')
	GRANT EXECUTE ON dbo.usp_Setting_Get TO R2k_RecordCardsOnly
IF EXISTS (SELECT * FROM SysUsers WHERE NAME = 'Dev - JNCC SQL')
   	GRANT EXECUTE ON dbo.usp_Setting_Get TO "Dev - JNCC SQL"
GO