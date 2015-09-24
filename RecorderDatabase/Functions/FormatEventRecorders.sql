If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[FormatEventRecorders]') AND OBJECTPROPERTY(Id, N'IsScalarFunction') = 1)
    BEGIN
        PRINT 'Dropping function FormatEventRecorders'
        DROP FUNCTION [dbo].[FormatEventRecorders]
    END
GO

    PRINT 'Creating function FormatEventRecorders'
GO

    /*
    $History: FormatEventRecorders.sql $
 * 
 * *****************  Version 3  *****************
 * User: Anthonysimpson Date: 20/05/04   Time: 14:57
 * Updated in $/JNCC/Development/Build/SQL Scripts/Functions
 * Permissions corrected.
 * 
 * *****************  Version 2  *****************
 * User: Bencollier   Date: 11/11/02   Time: 12:37
 * Updated in $/JNCC/Development/Build/SQL Scripts/Functions
 * Removed unicode from variables.
 * 
 * *****************  Version 1  *****************
 * User: Bencollier   Date: 11/11/02   Time: 9:24
 * Created in $/JNCC/Development/Build/SQL Scripts/Functions
 * Initial Build

    */

CREATE FUNCTION dbo.FormatEventRecorders(@SampleKey char(16))
RETURNS varchar(8000)
--
--	DESCRIPTION
--	Function to return a semi-colon sperated string of all the sample recorders.
--	
--
--	PARAMETERS
--	NAME				DESCRIPTION
--	@SampleKey			Sample Key to perform manipulation on.
--
--
--	AUTHOR:	Ben Collier, Dorset Software
--	CREATED: 08/11/2002
--

AS
BEGIN

--****************************************************************************************************
DECLARE @ReturnString varchar(8000)
DECLARE @ItemString varchar(70)
DECLARE @Title char(10)
DECLARE @Initials varchar(8)
DECLARE @Forename varchar(20)
DECLARE @Surname varchar(30)

DECLARE csrEventRecorder CURSOR
FOR
SELECT DISTINCT INDIVIDUAL.TITLE, INDIVIDUAL.INITIALS, INDIVIDUAL.FORENAME, INDIVIDUAL.SURNAME
FROM
(SAMPLE_RECORDER
LEFT JOIN
	(SURVEY_EVENT_RECORDER
	LEFT JOIN
		INDIVIDUAL
	ON SURVEY_EVENT_RECORDER.NAME_KEY = INDIVIDUAL.NAME_KEY)
ON SAMPLE_RECORDER.SE_RECORDER_KEY = SURVEY_EVENT_RECORDER.SE_RECORDER_KEY)
WHERE SAMPLE_RECORDER.SAMPLE_KEY = @SampleKey

OPEN csrEventRecorder

FETCH NEXT FROM csrEventRecorder INTO @Title, @Initials, @Forename, @Surname
IF @@FETCH_STATUS = 0 SELECT @ReturnString = dbo.FormatIndividual(@Title, @Initials, @Forename, @Surname)

WHILE @@FETCH_STATUS = 0
BEGIN
	FETCH NEXT FROM csrEventRecorder INTO @Title, @Initials, @Forename, @Surname
	SELECT @ItemString = dbo.FormatIndividual(@Title, @Initials, @Forename, @Surname)
	IF @@FETCH_STATUS = 0 SELECT @ReturnString = @ReturnString + ';' + @ItemString
END

CLOSE csrEventRecorder
DEALLOCATE csrEventRecorder

RETURN @ReturnString
--****************************************************************************************************

END
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * 
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[FormatEventRecorders]')
	   AND    Type IN ('FN', 'IF', 'TF'))
	BEGIN
    	PRINT 'Setting up security on function FormatEventRecorders'
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
	        	GRANT EXECUTE ON dbo.FormatEventRecorders TO [R2k_AddOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
			GRANT EXECUTE ON dbo.FormatEventRecorders TO [R2k_Administrator]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
			GRANT EXECUTE ON dbo.FormatEventRecorders TO [R2k_FullEdit]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
			GRANT EXECUTE ON dbo.FormatEventRecorders TO [R2k_ReadOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
			GRANT EXECUTE ON dbo.FormatEventRecorders TO [R2k_RecordCardsOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
	        	GRANT EXECUTE ON dbo.FormatEventRecorders TO [Dev - JNCC SQL]
	END
GO