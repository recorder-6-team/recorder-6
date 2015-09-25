SET QUOTED_IDENTIFIER ON
SET ANSI_NULLS ON
GO

/*============================================================================*\
Drop function before re-creating.
\*============================================================================*/
IF OBJECT_ID('dbo.ufn_MakeKey') IS NOT NULL
	DROP FUNCTION dbo.ufn_MakeKey
GO

/*============================================================================*\
Description:	Constructs a key value by adding an offset to a base value
				and combining the result with the given site id.

Parameters:		@base_key				Base value for the key
				@offset					Offset from the base value
				@site_id				[optional] Site id (default value
										taken from the 'SiteID' setting).

Created:		March 2009

Last revision information:
	$Revision: 3 $
	$Date: 20/03/09 18:00 $
	$Author: Andrewkemp $
\*============================================================================*/
CREATE FUNCTION dbo.ufn_MakeKey(
	@base_key                           CHAR(8),
	@offset								INT,
	@site_id							CHAR(8) = NULL)
	RETURNS								CHAR(16)
AS
BEGIN
	DECLARE			@new_key			VARCHAR(16)
	
	IF @site_id IS NULL
	BEGIN
		SELECT		@site_id				=	Data
		FROM		Setting
		WHERE		Name					=	'SiteID'		
	END

	SELECT			@new_key			=	dbo.ufn_Base36Sum(
													@base_key,
													@offset)
	IF LEN(@new_key) > 8
	BEGIN
		-- failure; duplicate the base key to cause a primary key violation
		SET				@new_key			=	@base_key
	END

	RETURN			@site_id + RIGHT('00000000' + @new_key, 8)
END
GO

/*============================================================================*\
Grant permissions.
\*============================================================================*/
IF EXISTS (SELECT 1 FROM sysusers WHERE name = 'Dev- JNCC SQL')
	GRANT EXECUTE ON dbo.ufn_MakeKey TO "Dev- JNCC SQL"

IF EXISTS (SELECT 1 FROM sysusers WHERE name = 'R2k_AddOnly')
	GRANT EXECUTE ON dbo.ufn_MakeKey TO R2k_AddOnly

IF EXISTS (SELECT 1 FROM sysusers WHERE name = 'R2k_Administrator')
	GRANT EXECUTE ON dbo.ufn_MakeKey TO R2k_Administrator

IF EXISTS (SELECT 1 FROM sysusers WHERE name = 'R2k_FullEdit')
	GRANT EXECUTE ON dbo.ufn_MakeKey TO R2k_FullEdit

IF EXISTS (SELECT 1 FROM sysusers WHERE name = 'R2k_RecordCardsOnly')
	GRANT EXECUTE ON dbo.ufn_MakeKey TO R2k_RecordCardsOnly
GO