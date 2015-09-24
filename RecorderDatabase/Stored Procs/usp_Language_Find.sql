/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Language_Find') AND SysStat & 0xf = 4)
    DROP PROCEDURE [dbo].[usp_Language_Find]
GO

/*===========================================================================*\
  Description:	Finds the language matching the given name. If not found, the
	default language key (en - English) is returned.

  Parameters:

  Created:	February 2008

  Last revision information:
    $Revision: 1 $
    $Date: 6/02/08 16:27 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Language_Find]
	@Key	VARCHAR(4)	OUTPUT,
	@Name	VARCHAR(50)
AS
	SET NOCOUNT ON

	SET 	@Key = NULL

	SELECT	@Key = Language_Key
	FROM	Language
	WHERE	Item_Name  = @Name

	IF @Key IS NULL BEGIN
		SELECT	@Key = Language_Key
		FROM	Language
		WHERE	Item_Name LIKE '%' + @Name + '%'

		IF @Key IS NULL
			SELECT	@Key = Language_Key
			FROM	Language
			WHERE	Language_Key = 'en'
	END
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Language_Find') AND SysStat & 0xf = 4)
BEGIN
   	PRINT 'Setting up security on procedure usp_Language_Find'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Language_Find TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Language_Find TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
		GRANT EXECUTE ON dbo.usp_Language_Find TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
       	GRANT EXECUTE ON dbo.usp_Language_Find TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
       	GRANT EXECUTE ON dbo.usp_Language_Find TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
       	GRANT EXECUTE ON dbo.usp_Language_Find TO [Dev - JNCC SQL]
END
GO
