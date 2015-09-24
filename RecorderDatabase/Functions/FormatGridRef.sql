If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[FormatGridRef]') AND OBJECTPROPERTY(Id, N'IsScalarFunction') = 1)
    BEGIN
        PRINT 'Dropping function FormatGridRef'
        DROP FUNCTION [dbo].[FormatGridRef]
    END
GO

    PRINT 'Creating function FormatGridRef'
GO

    /*
    $History: FormatGridRef.sql $
 * 
 * *****************  Version 3  *****************
 * User: Anthonysimpson Date: 20/05/04   Time: 14:57
 * Updated in $/JNCC/Development/Build/SQL Scripts/Functions
 * Permissions corrected.
 * 
 * *****************  Version 2  *****************
 * User: Bencollier   Date: 11/11/02   Time: 13:10
 * Updated in $/JNCC/Development/Build/SQL Scripts/Functions
 * Removed unicode from variables.
 * 
 * *****************  Version 1  *****************
 * User: Bencollier   Date: 6/11/02    Time: 12:13
 * Created in $/JNCC/Development/Build/SQL Scripts/Functions
 * Initial Build

    */

CREATE FUNCTION dbo.FormatGridRef(@GridRef varchar(20), @GridSys varchar(4), @IS1km bit = 1)
RETURNS varchar(20)
--
--	DESCRIPTION
--	Function to accept a grid reference string and return a reference string of a different resolution
--	
--
--	PARAMETERS
--	NAME			DESCRIPTION
--	@GridRef		Grid reference string
--	@GridSys		Grid system
--	@IS1km			Resolution of grid reference string to be returned
--
--
--	AUTHOR:	Ben Collier, Dorset Software
--	CREATED: 05/11/2002
--

AS
BEGIN

--****************************************************************************************************
DECLARE @TrimmedGridRef varchar(20)
SELECT @TrimmedGridRef = REPLACE(@GridRef, ' ', '')

IF @IS1km = 0
BEGIN
	--Return 10km square
	IF @GridSys = 'OSNI'
	BEGIN
		IF LEN(@TrimmedGridRef) >= 3
		BEGIN
			RETURN SUBSTRING(@TrimmedGridRef, 1, 2) + SUBSTRING(@TrimmedGridRef, (LEN(@TrimmedGridRef) -1)/2 + 2, 1)
		END
	END
	ELSE
	IF @GridSys = 'OSGB'
	BEGIN
		IF LEN(@TrimmedGridRef) >= 4
		BEGIN
			RETURN SUBSTRING(@TrimmedGridRef, 1, 3) + SUBSTRING(@TrimmedGridRef, (LEN(@TrimmedGridRef) -2)/2 + 3, 1)
		END
	END
END
ELSE
BEGIN
	--Return 1km square
	IF @GridSys = 'OSNI'
	BEGIN
		IF LEN(@TrimmedGridRef) >= 5
		BEGIN
			RETURN SUBSTRING(@TrimmedGridRef, 1, 3) + SUBSTRING(@TrimmedGridRef, (LEN(@TrimmedGridRef) -1)/2 + 2, 2)
		END
	END
	ELSE
	IF @GridSys = 'OSGB'
	BEGIN
		IF LEN(@TrimmedGridRef) >= 6
		BEGIN
			RETURN SUBSTRING(@TrimmedGridRef, 1, 4) + SUBSTRING(@TrimmedGridRef, (LEN(@TrimmedGridRef) -2)/2 + 3, 2)
		END
	END
END

RETURN ''
--****************************************************************************************************

END
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * 
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[FormatGridRef]')
	   AND    Type IN ('FN', 'IF', 'TF'))
	BEGIN
    	PRINT 'Setting up security on function FormatGridRef'
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
	        	GRANT EXECUTE ON dbo.FormatGridRef TO [R2k_AddOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
			GRANT EXECUTE ON dbo.FormatGridRef TO [R2k_Administrator]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
			GRANT EXECUTE ON dbo.FormatGridRef TO [R2k_FullEdit]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
			GRANT EXECUTE ON dbo.FormatGridRef TO [R2k_ReadOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
			GRANT EXECUTE ON dbo.FormatGridRef TO [R2k_RecordCardsOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
	        	GRANT EXECUTE ON dbo.FormatGridRef TO [Dev - JNCC SQL]
	END
GO
