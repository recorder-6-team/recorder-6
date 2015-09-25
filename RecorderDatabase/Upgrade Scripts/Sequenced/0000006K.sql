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
	$Revision: 2 $
	$Date: 16/03/09 16:10 $
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

/*============================================================================*\
Drop function before re-creating.
\*============================================================================*/
IF OBJECT_ID('dbo.ufn_GetImportWizardLocationName') IS NOT NULL
	DROP FUNCTION dbo.ufn_GetImportWizardLocationName
GO

/*============================================================================*\
Description:	Gets the value to use for the location name in imported data.

Parameters:		@import_location_name	"Location Name" column type value from
										the source data
				@import_location		"Location" column type value from the
										source data
				@matched_location_key	Location key derived from the source
										data
				@join_names				1 =>	combine location name and
												location values
										0 =>	use location name if not null,
												otherwise use location

Created:		March 2009

Last revision information:
	$Revision: 2 $
	$Date: 16/03/09 16:10 $
	$Author: Andrewkemp $
\*============================================================================*/
CREATE FUNCTION dbo.ufn_GetImportWizardLocationName(
	@import_location_name				VARCHAR(500),
	@import_location					VARCHAR(500),
	@matched_location_key				VARCHAR(16),
	@join_names							BIT)
	RETURNS								VARCHAR(202)
AS
BEGIN
	DECLARE		@name					VARCHAR(100),
				@location				VARCHAR(100),
				@proper_location		VARCHAR(100)

	SELECT		@name					=	ISNULL(@import_location_name, ''),
				@location				=	ISNULL(@import_location, '')

	IF @matched_location_key IS NOT NULL
	BEGIN
		SELECT		@proper_location		=	Item_Name
		FROM		Location_Name
		WHERE		Location_Key			=	@matched_location_key
		AND			Preferred				=	1
		
		IF @proper_location = @location
			SET			@location				=	NULL
	END

	RETURN		CASE
					WHEN @name = ''			THEN @location
					WHEN @location = ''		THEN @name
					WHEN @join_names = 1	THEN @location + ', ' + @name
					ELSE @name
				END
END
GO

/*============================================================================*\
Grant permissions.
\*============================================================================*/
IF EXISTS (SELECT 1 FROM sysusers WHERE name = 'Dev- JNCC SQL')
	GRANT EXECUTE ON dbo.ufn_GetImportWizardLocationName TO "Dev- JNCC SQL"

IF EXISTS (SELECT 1 FROM sysusers WHERE name = 'R2k_AddOnly')
	GRANT EXECUTE ON dbo.ufn_GetImportWizardLocationName TO R2k_AddOnly

IF EXISTS (SELECT 1 FROM sysusers WHERE name = 'R2k_Administrator')
	GRANT EXECUTE ON dbo.ufn_GetImportWizardLocationName TO R2k_Administrator

IF EXISTS (SELECT 1 FROM sysusers WHERE name = 'R2k_FullEdit')
	GRANT EXECUTE ON dbo.ufn_GetImportWizardLocationName TO R2k_FullEdit

IF EXISTS (SELECT 1 FROM sysusers WHERE name = 'R2k_RecordCardsOnly')
	GRANT EXECUTE ON dbo.ufn_GetImportWizardLocationName TO R2k_RecordCardsOnly
GO

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
	$Date: 16/03/09 16:10 $
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

/*============================================================================*\
Drop stored proc before re-creating.
\*============================================================================*/
IF OBJECT_ID('dbo.usp_ReserveKeys') IS NOT NULL
	DROP PROCEDURE dbo.usp_ReserveKeys
GO

/*============================================================================*\
Description:	Reserves a number of key values in the specified table.

Parameters:		@table_name				The table for which to reserve the keys.
				@number_of_keys			The number of keys to reserve.
				@last_key				[output] The key value immediately
										before the first reserved key.

Created:		March 2009

Last revision information:
	$Revision: 2 $
	$Date: 16/03/09 16:10 $
	$Author: Andrewkemp $
\*============================================================================*/
CREATE PROCEDURE dbo.usp_ReserveKeys
	@table_name							VARCHAR(30),
	@number_of_keys						INT,
	@last_key							CHAR(8) OUTPUT
AS
	DECLARE		@next_key				VARCHAR(16)	

	UPDATE		LAST_KEY
	SET			@last_key				=	LAST_KEY_TEXT,
				LAST_KEY_TEXT			=	RIGHT(
													'0000000'
													+ dbo.ufn_Base36Sum(
															LAST_KEY_TEXT,
															@number_of_keys),
													8)
	WHERE		TABLE_NAME				=	@table_name
	AND			dbo.ufn_Base36Sum(
						LAST_KEY_TEXT,
						@number_of_keys)	<>	'#ERROR!'

	IF @@ROWCOUNT = 0
	BEGIN
		SET			@last_key				=	'00000000'

		INSERT INTO	LAST_KEY (
					TABLE_NAME,
					LAST_KEY_TEXT)
		VALUES		(@table_name,
					dbo.ufn_Base36Sum(@last_key, @number_of_keys))
	END
GO

/*============================================================================*\
Grant permissions.
\*============================================================================*/
IF EXISTS (SELECT 1 FROM sysusers WHERE name = 'Dev- JNCC SQL')
	GRANT EXECUTE ON dbo.usp_ReserveKeys TO "Dev- JNCC SQL"

IF EXISTS (SELECT 1 FROM sysusers WHERE name = 'R2k_AddOnly')
	GRANT EXECUTE ON dbo.usp_ReserveKeys TO R2k_AddOnly

IF EXISTS (SELECT 1 FROM sysusers WHERE name = 'R2k_Administrator')
	GRANT EXECUTE ON dbo.usp_ReserveKeys TO R2k_Administrator

IF EXISTS (SELECT 1 FROM sysusers WHERE name = 'R2k_FullEdit')
	GRANT EXECUTE ON dbo.usp_ReserveKeys TO R2k_FullEdit

IF EXISTS (SELECT 1 FROM sysusers WHERE name = 'R2k_RecordCardsOnly')
	GRANT EXECUTE ON dbo.usp_ReserveKeys TO R2k_RecordCardsOnly
GO