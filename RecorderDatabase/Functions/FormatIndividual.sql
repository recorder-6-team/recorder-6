If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[FormatIndividual]') AND OBJECTPROPERTY(Id, N'IsScalarFunction') = 1)
    BEGIN
        PRINT 'Dropping function FormatIndividual'
        DROP FUNCTION [dbo].[FormatIndividual]
    END
GO

    PRINT 'Creating function FormatIndividual'
GO

    /*
    $History: FormatIndividual.sql $
 * 
 * *****************  Version 3  *****************
 * User: Anthonysimpson Date: 20/05/04   Time: 14:57
 * Updated in $/JNCC/Development/Build/SQL Scripts/Functions
 * Permissions corrected.
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

CREATE FUNCTION dbo.FormatIndividual(@Title char(10), @Initials varchar(8), @Forename varchar(20), @Surname varchar(30))
RETURNS varchar(70)
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
--

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

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * 
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[FormatIndividual]')
	   AND    Type IN ('FN', 'IF', 'TF'))
	BEGIN
    	PRINT 'Setting up security on function FormatIndividual'
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
	        	GRANT EXECUTE ON dbo.FormatIndividual TO [R2k_AddOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
			GRANT EXECUTE ON dbo.FormatIndividual TO [R2k_Administrator]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
			GRANT EXECUTE ON dbo.FormatIndividual TO [R2k_FullEdit]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
			GRANT EXECUTE ON dbo.FormatIndividual TO [R2k_ReadOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
			GRANT EXECUTE ON dbo.FormatIndividual TO [R2k_RecordCardsOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
	        	GRANT EXECUTE ON dbo.FormatIndividual TO [Dev - JNCC SQL]
	END
GO