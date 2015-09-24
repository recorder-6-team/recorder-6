SET QUOTED_IDENTIFIER ON
SET ANSI_NULLS ON

/*============================================================================*\
VI 27462 -- surveys must always have a media key
\*============================================================================*/
UPDATE      dbo.Survey
SET         Survey_Media_Key            =   'NBNSYS0000000001'
WHERE       Survey_Media_Key            IS NULL
GO

ALTER TABLE		dbo.Survey
ALTER COLUMN	Survey_Media_Key		CHAR(16) NOT NULL
GO

SET QUOTED_IDENTIFIER ON
SET ANSI_NULLS ON
GO

/*============================================================================*\
Drop stored proc before re-creating.
\*============================================================================*/
IF OBJECT_ID('dbo.usp_SurveyMedia_Select') IS NOT NULL
	DROP PROCEDURE dbo.usp_SurveyMedia_Select
GO

/*============================================================================*\
Description:	Selects the specified survey media record.

Parameters:		@key					Identifies the SURVEY_MEDIA record.

Created:		May 2013
\*============================================================================*/
CREATE PROCEDURE dbo.usp_SurveyMedia_Select
	@Key								CHAR(16)
AS
	SET NOCOUNT ON
	
	SELECT		Survey_Media_Key		AS	Item_Key,
				Short_Name				AS	Item_Name
	FROM		dbo.Survey_Media
	WHERE		Survey_Media_Key		=	@Key	
GO

/*============================================================================*\
Grant permissions.
\*============================================================================*/
IF EXISTS (SELECT 1 FROM sysusers WHERE name = 'Dev- JNCC SQL')
	GRANT EXECUTE ON dbo.usp_SurveyMedia_Select TO "Dev- JNCC SQL"

IF EXISTS (SELECT 1 FROM sysusers WHERE name = 'R2k_AddOnly')
	GRANT EXECUTE ON dbo.usp_SurveyMedia_Select TO R2k_AddOnly

IF EXISTS (SELECT 1 FROM sysusers WHERE name = 'R2k_Administrator')
	GRANT EXECUTE ON dbo.usp_SurveyMedia_Select TO R2k_Administrator

IF EXISTS (SELECT 1 FROM sysusers WHERE name = 'R2k_FullEdit')
	GRANT EXECUTE ON dbo.usp_SurveyMedia_Select TO R2k_FullEdit

IF EXISTS (SELECT 1 FROM sysusers WHERE name = 'R2k_RecordCardsOnly')
	GRANT EXECUTE ON dbo.usp_SurveyMedia_Select TO R2k_RecordCardsOnly
GO