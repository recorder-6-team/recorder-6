/*===========================================================================*\
  Drop function before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * 
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[ufn_GetFormattedName]')
	   AND    Type IN ('FN', 'IF', 'TF'))
	DROP FUNCTION ufn_GetFormattedName
GO


/*===========================================================================*\
  Description:	Returns the formatted name for the given name key.
		The rule for individual name format is:
			Title + (Forename | Initials) + Surname

		The rule for organisation name format is
			Acronym + ', ' + Full_Name

		Null fields are omitted.

  Parameters:	@NameKey

  Created:	August 2003

  Last revision information:
    $Revision: 2 $
    $Date: 6/05/04 15:29 $
    $Author: Anthonysimpson $

\*===========================================================================*/
CREATE FUNCTION [dbo].[ufn_GetFormattedName]
	(@NameKey char(16))
RETURNS varchar(100)
AS
BEGIN
	DECLARE	@FormattedName varchar(100)
	SET @FormattedName = ''

	IF EXISTS(SELECT * FROM [Name] WHERE Name_Key = @NameKey AND Organisation = 0)
	BEGIN
		SELECT	@FormattedName = 
			CASE WHEN Forename IS NULL THEN
				CASE WHEN Initials IS NULL THEN
					CASE WHEN Title IS NULL THEN Surname
					ELSE Title + ' ' + Surname END
				ELSE Initials + ' ' + Surname END
			ELSE Forename + ' ' + Surname END
		FROM	Individual
		WHERE	Name_Key = @NameKey
	END ELSE BEGIN
		SELECT	@FormattedName = 
			CASE WHEN Acronym IS NOT NULL THEN Acronym + ', ' + Full_Name
			ELSE Full_Name END
		FROM Organisation 
		WHERE Name_Key = @NameKey
	END
	RETURN @FormattedName
END

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * 
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[ufn_GetFormattedName]')
	   AND    Type IN ('FN', 'IF', 'TF'))
	BEGIN
    	PRINT 'Setting up security on function ufn_GetFormattedName'
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
	        	GRANT EXECUTE ON dbo.ufn_GetFormattedName TO [R2k_AddOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
			GRANT EXECUTE ON dbo.ufn_GetFormattedName TO [R2k_Administrator]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
			GRANT EXECUTE ON dbo.ufn_GetFormattedName TO [R2k_FullEdit]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
			GRANT EXECUTE ON dbo.ufn_GetFormattedName TO [R2k_ReadOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
			GRANT EXECUTE ON dbo.ufn_GetFormattedName TO [R2k_RecordCardsOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
	        	GRANT EXECUTE ON dbo.ufn_GetFormattedName TO [Dev - JNCC SQL]
	END
GO
