IF Object_ID('dbo.usp_Setting_Update') IS NOT NULL
	DROP PROCEDURE dbo.usp_Setting_Update
GO

/*===========================================================================*\
  Description:
	Updates a setting entry, or inserts a new entry if not found.

  Parameters:
	@Name	The name of the setting to update.
	@Value	The value of the entry.

  Created:	July 2009

  Last revision information:
    $Revision: 1 $
    $Date: 20/07/09 11:34 $
    $Author: Ericsalmon $

\*===========================================================================*/

CREATE PROCEDURE dbo.usp_Setting_Update
	@Name	VARCHAR(20),
	@Value	VARCHAR(250)
AS
	SET NOCOUNT ON

	IF EXISTS(SELECT * FROM Setting WHERE Name = @Name) 
		UPDATE	Setting
		SET		Data = @Value
		WHERE	Name = @Name
	ELSE
		INSERT INTO Setting (Name, Data)
		VALUES (@Name, @Value)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
PRINT 'Setting up security on procedure '
IF EXISTS (SELECT * FROM SysUsers WHERE NAME = 'R2k_Administrator')
	GRANT EXECUTE ON dbo.usp_Setting_Update TO R2k_Administrator
IF EXISTS (SELECT * FROM SysUsers WHERE NAME = 'Dev - JNCC SQL')
   	GRANT EXECUTE ON dbo.usp_Setting_Update TO "Dev - JNCC SQL"
GO