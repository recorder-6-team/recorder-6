/*===========================================================================*\
  Description:	Upgrade script to set permissions correctly for functions 
		that are saved in the JNCC folders.

  Created:	May 2004

  Last revision information:
    $Revision: 1 $
    $Date: 20/05/04 15:01 $
    $Author: Anthonysimpson $

\*===========================================================================*/

If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[FormatDatePart]') AND OBJECTPROPERTY(Id, N'IsScalarFunction') = 1)
    BEGIN
        PRINT 'Dropping function FormatDatePart'
        DROP FUNCTION [dbo].[FormatDatePart]
    END
GO

    PRINT 'Creating function FormatDatePart'
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
 * User: Johnvanbreda Date: 18/03/03   Time: 13:58
 * Updated in $/JNCC/Development/Build/SQL Scripts/Functions
 * JNCC600
 * Fixed old dates

    */

CREATE FUNCTION dbo.FormatDatePart(@VagueDateStart int, @VagueDateEnd int, @VagueDateType varchar(2), @ISMonth bit = 1)
RETURNS int
--
--	DESCRIPTION
--	Function to return either a year or month number given a vague date start, end and type.
--	
--
--	PARAMETERS
--	NAME			DESCRIPTION
--	@VagueDateStart		Vague date start
--	@VagueDateEnd		Vague date end
--	@VagueDateType		Vague date type
--	@ISMonth			Return month or year
--
--
--	AUTHOR:	Ben Collier, Dorset Software
--	CREATED: 05/11/2002
--

AS
BEGIN

--****************************************************************************************************
--constants
declare @D1 int
set @D1=365 							-- days in 1 year
declare @D4 int
set @D4 = @D1 * 4 + 1			-- days in 4 years, inc leap year
declare @D100 int
set @D100 = @D4 * 25 - 1 	-- days in 100 years (no leap year on 100th year)
declare @D400 int
set @D400 = @D100 * 4 + 1	-- days in 400 years - every 400 years you do  get a leap year
--variables
declare @T int
declare @Y int
declare @M int
declare @D int
declare @I int
-- get number of days since 1/1/01 
set @T = @VagueDateStart+693593
set @Y=1
-- find number of whole 400 year blocks
set @Y = @T / @D400
set @T = @T - @Y * @D400
set @Y = @Y * 400 + 1
set @I = @T / @D100
set @D = @T - @I * @D100
if @I=4 begin
  set @I = @I - 1
  set @D = @D + @D100
end
set @Y = @Y + @I * 100
set @I = @D / @D4
set @D = @D - @I * @D4
set @Y = @Y + @I * 4
set @I = @D / @D1
set @D = @D - @I * @D1
if @I = 4 begin
  set @I = @I - 1
  set @D = @D + @D1
end
set @Y=@Y + @I
if @IsMonth=1 begin
  set @M=1
  while 1=1 begin
    set @I = case @M
		when 1 then 31
		when 2 then 28
		when 3 then 31
		when 4 then 30
	  	when 5 then 31
		when 6 then 30
		when 7 then 31
		when 8 then 31
		when 9 then 30
		when 10 then 31
		when 11 then 30
		when 12 then 31
    end
    if @D<@I break
    set @D = @D - @I
    set @M = @M + 1
  end
  return @M
end
else
  return @Y

RETURN ''
--****************************************************************************************************

END

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * 
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[FormatDatePart]')
	   AND    Type IN ('FN', 'IF', 'TF'))
	BEGIN
    	PRINT 'Setting up security on function FormatDatePart'
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
	        	GRANT EXECUTE ON dbo.FormatDatePart TO [R2k_AddOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
			GRANT EXECUTE ON dbo.FormatDatePart TO [R2k_Administrator]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
			GRANT EXECUTE ON dbo.FormatDatePart TO [R2k_FullEdit]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
			GRANT EXECUTE ON dbo.FormatDatePart TO [R2k_ReadOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
			GRANT EXECUTE ON dbo.FormatDatePart TO [R2k_RecordCardsOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
	        	GRANT EXECUTE ON dbo.FormatDatePart TO [Dev - JNCC SQL]
	END
GO

/*===========================================================================*\
  Drop function before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * 
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[ufn_GetFormattedName]')
	   AND    Type IN ('FN', 'IF', 'TF'))
	DROP FUNCTION ufn_GetFormattedName
GO


/*===========================================================================*\
  Description:	Returns the formatted name for the given name key.
		The rule for individual name format is:
			Title + (Forename | Initials) + Surname

		The rule for organisation name format is
			Acronym + ', ' + Full_Name

		Null fields are omitted.

  Parameters:	@NameKey

  Created:	August 2003

  Last revision information:
    $Revision: 1 $
    $Date: 20/05/04 15:01 $
    $Author: Anthonysimpson $

\*===========================================================================*/
CREATE FUNCTION [dbo].[ufn_GetFormattedName]
	(@NameKey char(16))
RETURNS varchar(100)
AS
BEGIN
	DECLARE	@FormattedName varchar(100)
	SET @FormattedName = ''

	IF EXISTS(SELECT * FROM [Name] WHERE Name_Key = @NameKey AND Organisation = 0)
	BEGIN
		SELECT	@FormattedName = 
			CASE WHEN Forename IS NULL THEN
				CASE WHEN Initials IS NULL THEN
					CASE WHEN Title IS NULL THEN Surname
					ELSE Title + ' ' + Surname END
				ELSE Initials + ' ' + Surname END
			ELSE Forename + ' ' + Surname END
		FROM	Individual
		WHERE	Name_Key = @NameKey
	END ELSE BEGIN
		SELECT	@FormattedName = 
			CASE WHEN Acronym IS NOT NULL THEN Acronym + ', ' + Full_Name
			ELSE Full_Name END
		FROM Organisation 
		WHERE Name_Key = @NameKey
	END
	RETURN @FormattedName
END

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * 
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[ufn_GetFormattedName]')
	   AND    Type IN ('FN', 'IF', 'TF'))
	BEGIN
    	PRINT 'Setting up security on function ufn_GetFormattedName'
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
	        	GRANT EXECUTE ON dbo.ufn_GetFormattedName TO [R2k_AddOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
			GRANT EXECUTE ON dbo.ufn_GetFormattedName TO [R2k_Administrator]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
			GRANT EXECUTE ON dbo.ufn_GetFormattedName TO [R2k_FullEdit]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
			GRANT EXECUTE ON dbo.ufn_GetFormattedName TO [R2k_ReadOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
			GRANT EXECUTE ON dbo.ufn_GetFormattedName TO [R2k_RecordCardsOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
	        	GRANT EXECUTE ON dbo.ufn_GetFormattedName TO [Dev - JNCC SQL]
	END
GO

/*===========================================================================*\
  Drop function before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE Id = OBJECT_ID(N'[dbo].[ufn_FormatEventOwners]')
	   AND OBJECTPROPERTY(Id, N'IsScalarFunction') = 1)
  DROP FUNCTION [dbo].[ufn_FormatEventOwners]
GO

/*===========================================================================*\
  Description:	Returns the list of survey event owners as a semi-colon 
		separated string

  Parameters:	@Key	survey event key

  Created:	Jan 2004

  Last revision information:
    $Revision: 1 $
    $Date: 20/05/04 15:01 $
    $Author: Anthonysimpson $

\*===========================================================================*/   
CREATE FUNCTION dbo.ufn_FormatEventOwners(@Key char(16))
RETURNS varchar(8000)

AS
BEGIN

--****************************************************************************************************
DECLARE @ReturnString varchar(8000)
DECLARE @ItemString varchar(70)
DECLARE @NameKey char(16)
DECLARE @OwnerType varchar(20)

DECLARE csrEventOwner CURSOR
FOR
  SELECT EO.NAME_KEY, OT.SHORT_NAME
	FROM SURVEY_EVENT_OWNER EO
	INNER JOIN SURVEY_EVENT_OWNER_TYPE OT ON OT.SURVEY_EVENT_OWNER_TYPE_KEY = EO.SURVEY_EVENT_OWNER_TYPE_KEY
	WHERE EO.SURVEY_EVENT_KEY=@Key

OPEN csrEventOwner

FETCH NEXT FROM csrEventOwner INTO @NameKey, @OwnerType

IF @@FETCH_STATUS = 0 
  SELECT @ReturnString = dbo.ufn_GetFormattedName(@NameKey) + ' (' + @OwnerType + ')'

WHILE @@FETCH_STATUS = 0
BEGIN
	FETCH NEXT FROM csrEventOwner INTO @NameKey, @OwnerType
	SELECT @ItemString = dbo.ufn_GetFormattedName(@NameKey) + ' (' + @OwnerType + ')'
	IF @@FETCH_STATUS = 0 
		SELECT @ReturnString = @ReturnString + ';' + @ItemString
END

CLOSE csrEventOwner
DEALLOCATE csrEventOwner

RETURN @ReturnString
--****************************************************************************************************

END
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * 
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[ufn_FormatEventOwners]')
	   AND    Type IN ('FN', 'IF', 'TF'))
	BEGIN
    	PRINT 'Setting up security on function ufn_FormatEventOwners'
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
	        	GRANT EXECUTE ON dbo.ufn_FormatEventOwners TO [R2k_AddOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
			GRANT EXECUTE ON dbo.ufn_FormatEventOwners TO [R2k_Administrator]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
			GRANT EXECUTE ON dbo.ufn_FormatEventOwners TO [R2k_FullEdit]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
			GRANT EXECUTE ON dbo.ufn_FormatEventOwners TO [R2k_ReadOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
			GRANT EXECUTE ON dbo.ufn_FormatEventOwners TO [R2k_RecordCardsOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
	        	GRANT EXECUTE ON dbo.ufn_FormatEventOwners TO [Dev - JNCC SQL]
	END
GO


if exists (Select * from sysobjects where 
type = 'fn' and uid = user_ID('dbo') and name =
'IncrementKey')
drop function dbo.IncrementKey
go

--Function used in creation of keys for tables
-- Author: Polly Shaw
-- Created on : 6 Jan 2003
    /*
    $History: 0000000Z.sql $
 * 
 * *****************  Version 1  *****************
 * User: Anthonysimpson Date: 20/05/04   Time: 15:01
 * Created in $/JNCC/Development/Build/SQL Scripts/Upgrade Scripts/Sequenced
 * Initial check in by Anthonysimpson.
 * 
 * *****************  Version 1  *****************
 * User: Pollyshaw    Date: 6/02/03    Time: 14:14
 * Created in $/JNCC/Development/Build/SQL Scripts/Functions
 * Function generating new key for tables
*/

create function dbo.IncrementKey(

@OldKey Char(8))

returns Char(8)
as
begin
Declare @ChangeSection as Char(8),
	@iCurrentChar as integer,
	@chNewChar as Char,
	@Result as Char(8)
Set @iCurrentChar = 8
set @ChangeSection = @OldKey
set @chNewChar = dbo.IncrementChar(Substring(@ChangeSection, @iCurrentChar, 1))
set @ChangeSection = Stuff(@ChangeSection, @iCurrentChar,1, @chNewChar)
while (@chNewChar = '0') and (@iCurrentChar >0)
begin
	set @iCurrentChar = @iCurrentChar -1;
	set @chNewChar = dbo.IncrementChar(Substring(@ChangeSection, @iCurrentChar, 1))
	set @ChangeSection = Stuff(@ChangeSection, @iCurrentChar ,1, @chNewChar)

end
if @iCurrentChar = 0
--run out of keys
--can't raise an error, so return same key
--will cause insert error later
set @ChangeSection = @OldKey

return @ChangeSection

end
go

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * 
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[IncrementKey]')
	   AND    Type IN ('FN', 'IF', 'TF'))
	BEGIN
    	PRINT 'Setting up security on function IncrementKey'
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
	        	GRANT EXECUTE ON dbo.IncrementKey TO [R2k_AddOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
			GRANT EXECUTE ON dbo.IncrementKey TO [R2k_Administrator]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
			GRANT EXECUTE ON dbo.IncrementKey TO [R2k_FullEdit]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
			GRANT EXECUTE ON dbo.IncrementKey TO [R2k_ReadOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
			GRANT EXECUTE ON dbo.IncrementKey TO [R2k_RecordCardsOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
	        	GRANT EXECUTE ON dbo.IncrementKey TO [Dev - JNCC SQL]
	END
GO

if exists (Select * from sysobjects where 
type = 'fn' and uid = user_ID('dbo') and name =
'IncrementChar')
drop function dbo.IncrementChar
go

--Function used in creation of keys for tables
-- Author: Polly Shaw
-- Created on : 6 Jan 2003
    /*
    $History: 0000000Z.sql $
 * 
 * *****************  Version 1  *****************
 * User: Anthonysimpson Date: 20/05/04   Time: 15:01
 * Created in $/JNCC/Development/Build/SQL Scripts/Upgrade Scripts/Sequenced
 * Initial check in by Anthonysimpson.
 * 
 * *****************  Version 1  *****************
 * User: Pollyshaw    Date: 6/02/03    Time: 14:14
 * Created in $/JNCC/Development/Build/SQL Scripts/Functions
 * Function generating new key for tables
*/

create function dbo.IncrementChar( 
@IncChar  Char) returns Char


begin

declare @Result as Char
if @incChar = '9'  
	set @Result = 'A'
else if @incChar = 'Z'
	set @Result = '0'
else 
	set @Result = Char(Ascii(@incChar) + 1);

return @Result

end
go

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * 
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[IncrementChar]')
	   AND    Type IN ('FN', 'IF', 'TF'))
	BEGIN
    	PRINT 'Setting up security on function IncrementChar'
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
	        	GRANT EXECUTE ON dbo.IncrementChar TO [R2k_AddOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
			GRANT EXECUTE ON dbo.IncrementChar TO [R2k_Administrator]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
			GRANT EXECUTE ON dbo.IncrementChar TO [R2k_FullEdit]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
			GRANT EXECUTE ON dbo.IncrementChar TO [R2k_ReadOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
			GRANT EXECUTE ON dbo.IncrementChar TO [R2k_RecordCardsOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
	        	GRANT EXECUTE ON dbo.IncrementChar TO [Dev - JNCC SQL]
	END
GO

If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[FormatTaxonStatusKind]') AND OBJECTPROPERTY(Id, N'IsScalarFunction') = 1)
    BEGIN
        PRINT 'Dropping function FormatTaxonStatusKind'
        DROP FUNCTION [dbo].[FormatTaxonStatusKind]
    END
GO

    PRINT 'Creating function FormatTaxonStatusKind'
GO

    /*
    $History: 0000000Z.sql $
 * 
 * *****************  Version 1  *****************
 * User: Anthonysimpson Date: 20/05/04   Time: 15:01
 * Created in $/JNCC/Development/Build/SQL Scripts/Upgrade Scripts/Sequenced
 * Initial check in by Anthonysimpson.
 * 
 * *****************  Version 1  *****************
 * User: Bencollier   Date: 7/11/02    Time: 16:31
 * Created in $/JNCC/Development/Build/SQL Scripts/Functions
 * Initial Build
    */

CREATE FUNCTION dbo.FormatTaxonStatusKind(@TaxonListItemKey char(16))
RETURNS varchar(8000)
--
--	DESCRIPTION
--	Function to return a semi-colon sperated string of all the taxon lists that the 
--	specified taxon appears in.
--	
--
--	PARAMETERS
--	NAME				DESCRIPTION
--	@TaxonListItemKey	Taxon List Item Key to perform manipulation on
--
--
--	AUTHOR:	Ben Collier, Dorset Software
--	CREATED: 07/11/2002
--

AS
BEGIN

--****************************************************************************************************
DECLARE @ReturnString varchar(8000)
DECLARE @ItemName varchar(200)

DECLARE csrStatusListName CURSOR
FOR
SELECT DISTINCT TAXON_LIST.ITEM_NAME
FROM
	(TAXON_LIST_ITEM AS TLI
	LEFT JOIN
		(TAXON_VERSION
		LEFT JOIN
			(TAXON
			LEFT JOIN
				(TAXON_VERSION AS TV2
				LEFT JOIN
					(TAXON_LIST_ITEM AS TLI2
					LEFT JOIN
						(TAXON_LIST_VERSION
						LEFT JOIN
							TAXON_LIST
						ON TAXON_LIST_VERSION.TAXON_LIST_KEY = TAXON_LIST.TAXON_LIST_KEY)
					ON TLI2.TAXON_LIST_VERSION_KEY = TAXON_LIST_VERSION.TAXON_LIST_VERSION_KEY)
				ON TV2.TAXON_VERSION_KEY = TLI2.TAXON_VERSION_KEY)
			ON TAXON.TAXON_KEY = TV2.TAXON_KEY)
		ON TAXON_VERSION.TAXON_KEY = TAXON.TAXON_KEY)
	ON TLI.TAXON_VERSION_KEY = TAXON_VERSION.TAXON_VERSION_KEY) 
where TLI.taxon_list_item_key = @TaxonListItemKey

OPEN csrStatusListName

FETCH NEXT FROM csrStatusListName INTO @ReturnString
WHILE @@FETCH_STATUS = 0
BEGIN
	FETCH NEXT FROM csrStatusListName INTO @ItemName
	IF @@FETCH_STATUS = 0 SELECT @ReturnString = @ReturnString + ';' + @ItemName
END

CLOSE csrStatusListName
DEALLOCATE csrStatusListName

RETURN @ReturnString
--****************************************************************************************************

END
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * 
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[FormatTaxonStatusKind]')
	   AND    Type IN ('FN', 'IF', 'TF'))
	BEGIN
    	PRINT 'Setting up security on function FormatTaxonStatusKind'
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
	        	GRANT EXECUTE ON dbo.FormatTaxonStatusKind TO [R2k_AddOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
			GRANT EXECUTE ON dbo.FormatTaxonStatusKind TO [R2k_Administrator]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
			GRANT EXECUTE ON dbo.FormatTaxonStatusKind TO [R2k_FullEdit]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
			GRANT EXECUTE ON dbo.FormatTaxonStatusKind TO [R2k_ReadOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
			GRANT EXECUTE ON dbo.FormatTaxonStatusKind TO [R2k_RecordCardsOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
	        	GRANT EXECUTE ON dbo.FormatTaxonStatusKind TO [Dev - JNCC SQL]
	END
GO

If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[FormatIndividual]') AND OBJECTPROPERTY(Id, N'IsScalarFunction') = 1)
    BEGIN
        PRINT 'Dropping function FormatIndividual'
        DROP FUNCTION [dbo].[FormatIndividual]
    END
GO

    PRINT 'Creating function FormatIndividual'
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

If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[FormatGridRef]') AND OBJECTPROPERTY(Id, N'IsScalarFunction') = 1)
    BEGIN
        PRINT 'Dropping function FormatGridRef'
        DROP FUNCTION [dbo].[FormatGridRef]
    END
GO

    PRINT 'Creating function FormatGridRef'
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

If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[FormatEventRecorders]') AND OBJECTPROPERTY(Id, N'IsScalarFunction') = 1)
    BEGIN
        PRINT 'Dropping function FormatEventRecorders'
        DROP FUNCTION [dbo].[FormatEventRecorders]
    END
GO

    PRINT 'Creating function FormatEventRecorders'
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