/****** Changes in Connection with increaseing length of Forename field ******/


ALTER TABLE [dbo].[Individual]
ALTER COLUMN [Forename] varchar(30) NULL

GO

/****** Object:  UserDefinedFunction [dbo].[FormatEventRecorders]    Script Date: 08/23/2016 05:41:32 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

    /*
    $History: 0000000Z.sql $
 * 
 * *****************  Version 1  *****************
 * User: Anthonysimpson Date: 20/05/04   Time: 15:01
 * Created in $/JNCC/Development/Build/SQL Scripts/Upgrade Scripts/Sequenced
 * Initial check in by Anthonysimpson.
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

ALTER FUNCTION [dbo].[FormatEventRecorders](@SampleKey char(16))
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
DECLARE @Forename varchar(30)
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

/****** Object:  UserDefinedFunction [dbo].[FormatIndividual]    Script Date: 08/23/2016 05:43:43 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

    /*
    $History: 0000000Z.sql $
 * 
 * *****************  Version 1  *****************
 * User: Anthonysimpson Date: 20/05/04   Time: 15:01
 * Created in $/JNCC/Development/Build/SQL Scripts/Upgrade Scripts/Sequenced
 * Initial check in by Anthonysimpson.
 * 
 * *****************  Version 2  *****************
 * User: Bencollier   Date: 11/11/02   Time: 13:13
 * Updated in $/JNCC/Development/Build/SQL Scripts/Functions
 * Removed unicode from variables.
 * 
 * *****************  Version 1  *****************
 * User: Bencollier   Date: 6/11/02    Time: 12:13
 * Created in $/JNCC/Development/Build/SQL Scripts/Functions
 * Initial Build

    */

ALTER FUNCTION [dbo].[FormatIndividual](@Title char(10), @Initials varchar(8), @Forename varchar(30), @Surname varchar(30))
RETURNS varchar(100)
--
--	DESCRIPTION
--	Function to return a formatted string of an individuals title, initials, forename and surename.
--	
--
--	PARAMETERS
--	NAME			DESCRIPTION
--	@Title			Individual's Title
--	@Initials		Individual's Initials
--	@Forename		Individual's Forename
--	@Surname		Individual's Surname
--
--
--	AUTHOR:	Ben Collier, Dorset Software
--	CREATED: 06/11/2002
--      Updated August 2016 for change in length of Forename to 30 characters

AS
BEGIN

--****************************************************************************************************
IF @Forename IS NULL
	IF @Initials IS NULL
		IF @Title IS NULL
			RETURN @Surname
		ELSE
			RETURN CAST(@Title AS varchar) + ' ' + @Surname
	ELSE
		RETURN @Initials + ' ' + @Surname
ELSE
	RETURN @Forename + ' ' + @Surname
RETURN ''
--****************************************************************************************************

END


GO

/****** Object:  UserDefinedFunction [dbo].[FormatIndividualFull]    Script Date: 08/23/2016 05:45:40 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO



CREATE FUNCTION [dbo].[FormatIndividualFull](@Title varchar(10), @Initials varchar(8), @Forename varchar(30), @Surname varchar(30))
RETURNS varchar(100)
--
--	DESCRIPTION
--	Function to return a formatted string of an individuals title, forename, initials and surname.
--	Includes all elements unless null - Use where full information needs to be displayed. 
--
--	PARAMETERS
--	NAME			DESCRIPTION
--	@Title			Individual's Title
--	@Initials		Individual's Initials
--	@Forename		Individual's Forename
--	@Surname		Individual's Surname
--
--
--	AUTHOR:	Mike Weideli
--	CREATED: August 2016
--

AS
BEGIN

--****************************************************************************************************

RETURN  ISNULL(@Title +' ','') + ISNULL(@Forename + ' ','') + isnull(@initials + ' ','') + @Surname  

--****************************************************************************************************

END


GO 

GRANT EXECUTE ON [dbo].[FormatIndividualFull] TO PUBLIC

GO





