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
    $Revision: 1 $
    $Date: 22/06/09 11:49 $
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