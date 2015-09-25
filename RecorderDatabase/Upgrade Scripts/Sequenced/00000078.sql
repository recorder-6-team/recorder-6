SET QUOTED_IDENTIFIER ON
SET ANSI_NULLS ON
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
	$Date: 22/06/09 11:52 $
	$Author: Ericsalmon $
\*============================================================================*/
CREATE FUNCTION dbo.ufn_GetImportWizardLocationName(
	@import_location_name				VARCHAR(500),
	@import_location					VARCHAR(500),
	@matched_location_key				VARCHAR(16),
	@join_names							BIT)
	RETURNS								VARCHAR(100)
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
			SET		@location				=	''  -- Not NULL, since comparing to empty string later.
	END

	RETURN		CASE
					WHEN @name = ''			THEN @location
					WHEN @location = ''		THEN @name
					WHEN @join_names = 1	THEN LEFT(@location + ', ' + @name, 100)  -- In case concatenation exceeds 100 chars.
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

SET QUOTED_IDENTIFIER ON
SET ANSI_NULLS ON
GO

/*============================================================================*\
Drop function before re-creating.
\*============================================================================*/
IF OBJECT_ID('dbo.ufn_TrimWhiteSpaces') IS NOT NULL
	DROP FUNCTION dbo.ufn_TrimWhiteSpaces
GO

/*===========================================================================*\
  Description:
	Trims the given text and converts TAB, Carriage Return and Line Feed to spaces.
	This replicates the trimming function used with the Import Wizard.

  Parameters:
	@string		The text to trim. Set to VARCHAR(500) as it is currently
				the maximum length for text fields specified in IW_Column_Type.

  Created:	

  Last revision information:
    $Revision: 2 $
    $Date: 22/06/09 11:52 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE FUNCTION dbo.ufn_TrimWhiteSpaces(
	@string VARCHAR(500))
	RETURNS VARCHAR(500)
AS
BEGIN	
	DECLARE	@trimmedString VARCHAR(500)

	-- Remove leading and trailing spaces.
	SET	@trimmedString = LTRIM(RTRIM(@string))

	-- Replace all TAB, CR and LF characters with a single space.
	SET @trimmedString = REPLACE(@trimmedString, CHAR(9), ' ')
	SET @trimmedString = REPLACE(@trimmedString, CHAR(10), ' ')
	SET @trimmedString = REPLACE(@trimmedString, CHAR(13), ' ')

	-- Since we can now have duplicate spaces, replace these with a single space.
	SET @trimmedString = REPLACE(@trimmedString, '  ', ' ')

	RETURN @trimmedString
END
GO

/*============================================================================*\
Grant permissions.
\*============================================================================*/
IF EXISTS (SELECT 1 FROM sysusers WHERE name = 'Dev- JNCC SQL')
	GRANT EXECUTE ON dbo.ufn_TrimWhiteSpaces TO "Dev- JNCC SQL"

IF EXISTS (SELECT 1 FROM sysusers WHERE name = 'R2k_AddOnly')
	GRANT EXECUTE ON dbo.ufn_TrimWhiteSpaces TO R2k_AddOnly

IF EXISTS (SELECT 1 FROM sysusers WHERE name = 'R2k_Administrator')
	GRANT EXECUTE ON dbo.ufn_TrimWhiteSpaces TO R2k_Administrator

IF EXISTS (SELECT 1 FROM sysusers WHERE name = 'R2k_FullEdit')
	GRANT EXECUTE ON dbo.ufn_TrimWhiteSpaces TO R2k_FullEdit

IF EXISTS (SELECT 1 FROM sysusers WHERE name = 'R2k_RecordCardsOnly')
	GRANT EXECUTE ON dbo.ufn_TrimWhiteSpaces TO R2k_RecordCardsOnly
GO
