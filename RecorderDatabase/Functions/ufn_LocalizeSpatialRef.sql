/*============================================================================*\
Drop function before re-creating.
\*============================================================================*/
IF OBJECT_ID('dbo.ufn_LocalizeSpatialRef') IS NOT NULL
	DROP FUNCTION dbo.ufn_LocalizeSpatialRef
GO

/*============================================================================*\
Description:	Localizes a location's spatial reference using the given
				list and decimal separators.

Parameters:		@spatial_ref			The spatial reference to be localized.
				@list_separator			The new list separator.
				@decimal_separator		The new decimal separator.

Created:		March 2009

Last revision information:
	$Revision: 2 $
	$Date: 20/03/09 18:00 $
	$Author: Andrewkemp $
\*============================================================================*/
CREATE FUNCTION dbo.ufn_LocalizeSpatialRef(
	@spatial_ref						VARCHAR(40),
	@list_separator						CHAR,
	@decimal_separator					CHAR)
	RETURNS								VARCHAR(40)
AS
BEGIN
	DECLARE		@result					VARCHAR(40),
				@pos					INT,
				@char					CHAR

	SELECT		@pos					=	1,
				@result					=	''

	WHILE @pos <= LEN(@spatial_ref)
	BEGIN
		SELECT		@char					=	SUBSTRING(
														@spatial_ref,
														@pos,
														1),
					@pos					=	@pos + 1

		SET			@result					=	@result
												+ CASE @char
													WHEN ',' THEN @list_separator
													WHEN '.' THEN @decimal_separator
													ELSE @char
												END
	END

	RETURN		@result
END
GO

/*============================================================================*\
Grant permissions.
\*============================================================================*/
IF EXISTS (SELECT 1 FROM sysusers WHERE name = 'Dev- JNCC SQL')
	GRANT EXECUTE ON dbo.ufn_LocalizeSpatialRef TO "Dev- JNCC SQL"

IF EXISTS (SELECT 1 FROM sysusers WHERE name = 'R2k_AddOnly')
	GRANT EXECUTE ON dbo.ufn_LocalizeSpatialRef TO R2k_AddOnly

IF EXISTS (SELECT 1 FROM sysusers WHERE name = 'R2k_Administrator')
	GRANT EXECUTE ON dbo.ufn_LocalizeSpatialRef TO R2k_Administrator

IF EXISTS (SELECT 1 FROM sysusers WHERE name = 'R2k_FullEdit')
	GRANT EXECUTE ON dbo.ufn_LocalizeSpatialRef TO R2k_FullEdit

IF EXISTS (SELECT 1 FROM sysusers WHERE name = 'R2k_RecordCardsOnly')
	GRANT EXECUTE ON dbo.ufn_LocalizeSpatialRef TO R2k_RecordCardsOnly
GO