/******Change to length of Forename and Surname Field ******/
ALTER TABLE INDIVIDUAL
ALTER COLUMN FORENAME varchar(35)
GO
ALTER TABLE INDIVIDUAL
ALTER COLUMN SURNAME varchar(35)
GO
/****** Object:  UserDefinedFunction [dbo].[FormatIndividualFull]    Script Date: 01/04/2020 22:26:26 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
ALTER FUNCTION [dbo].[FormatIndividualFull](@Title varchar(10), @Initials varchar(8), @Forename varchar(35), @Surname varchar(35))
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
--      REVISED Mike Weideli January 2020 - Change field lengths
AS
BEGIN

--****************************************************************************************************

RETURN     @Surname +  ISNULL(' ' +@Title ,'') + ISNULL(' ' + @Forename ,'') +
 isnull(' ' +@initials,'')

--****************************************************************************************************

END

GO
/****** Object:  UserDefinedFunction [dbo].[FormatEventRecorders]    Script Date: 01/04/2020 22:30:09 ******/
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
 * Alter for field length Surname and Forename 
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
DECLARE @Forename varchar(35)
DECLARE @Surname varchar(35)

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
/****** Object:  UserDefinedFunction [dbo].[FormatIndividual]    Script Date: 01/04/2020 22:32:24 ******/
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

ALTER FUNCTION [dbo].[FormatIndividual](@Title char(10), @Initials varchar(8), @Forename varchar(35), @Surname varchar(35))
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
--      Updated August 2020 for change in length of Surname & Forename to 35 characters

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
/****** Object:  UserDefinedFunction [dbo].[ufn_CompareNames]    Script Date: 01/04/2020 22:35:58 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

/*===========================================================================*\
  Description:	Returns a numeric value showing how well
  two names compare. Is using Title,ForeName,Initials and Surname.
  Takes into account that either of the two names sets may have null values,
  however, surnames must be the same to match - assumes that at least one
  of the surnames is not null. 
  Returns 0 if not a possible match 
		  8 surname only matching
		  9 surname + title             
		  10 surname + initials                
		  11 surname + title + initials or   surname + title + long(initials)
		  12 surname + forename 
		  13 surname + title +  forename
		  14 surname + Initial +  forename
		  15 surname + Initial + Forename  + title
		  16 surname + long Initial + Forename  + title 	  
 
  Parameters Title,Forename,Initials,Surname
  Title2,Forename2,Initials2,Surname2
       
  Created:	December 2018

  Mike Weideli:
    

\*===========================================================================*/
ALTER FUNCTION [dbo].[ufn_CompareNames]
 (@Title varchar(4),
 @Forename varchar(35),
 @Initials varchar(8),
 @Surname varchar(35),
 @Title2 varchar(4),	
 @ForeName2 varchar(35),
 @Initials2 varchar(8),
 @Surname2 varchar(35))	

RETURNS integer
AS
BEGIN
  DECLARE @Compare integer 
  Set @Compare = 0
 -- try to deal with situations around Forenames and Initials
 -- Make the initials 

 if @Initials is null and @Forename is not null set @Initials = left(@Forename,1)
 if @Initials2 is null and @Forename2 is not null set @Initials2 = left(@Forename2,1)
 
  -- If there is a Forename then Initial is often the middle initial
  
  if @Forename is not null and Left(@forename,1) <> Left(@Initials,1) 
    set @Initials = left(@Forename,1) + '.' + isnull(@Initials,'') 
    
  if @Forename2 is not null and Left(@forename2,1) <> Left(@Initials2,1) 
    set @Initials2 = left(@Forename2,1) + '.' + isnull(@Initials2,'') 
     
  -- Deal with full stop
 
  SET @Title = REPLACE(@Title,'.', '' ) 
  SET @Title2 = REPLACE(@Title2,'.', '' ) 

  SET @Title = REPLACE(@Title,'Miss', 'Ms' ) 
  SET @Title2 = REPLACE(@Title2,'Miss', 'Ms' ) 
  
  SET @Initials = REPLACE(@Initials,'.', '' ) 
  SET @Initials2 = REPLACE(@Initials2,'.', '' )

  

  -- Deal with spaces
  
  SET @Initials = REPLACE(@Initials,' ', '' ) 
  SET @Initials2 = REPLACE(@Initials2,' ', '' ) 
  
  -- Deal with different lengths of initials 
  
  If len(@Initials) > len(@Initials2)  set @Initials = left(@initials,len(@Initials2))
  If len(@Initials2) > len(@Initials)  set @Initials2 = left(@initials2,len(@Initials))
  
   -- Deal with different lengths of forename but
   -- only if over 3 characters 
  
  If len(@Forename) > len(@Forename2) AND len(@Forename2) > 3 
    set @Forename = left(@Forename,len(@Forename2))
  If len(@Forename2) > len(@Forename) AND len(@Forename) > 3 
    set @Forename2 = left(@Forename2,len(@Forename))
   
  -- The main process 
  if (isnull(@Title,'') = isnull(@Title2,'') 
    or @Title + @Title2 IS NULL)
  AND (isnull(@Initials,'') = isnull(@Initials2,'') 
    or @Initials + @Initials2 IS NULL)
  AND (isnull(@Forename,'') = isnull(@ForeName2,'')  
    or @Forename+@Forename2 IS NULL) 
  AND (ISNULL(@Surname,'') = ISNULL(@Surname2,'')) 
  BEGIN
    Set @Compare = 8 
    IF @Title= @Title2  set @Compare = @Compare + 1
    IF @Initials = @Initials2 set  @Compare = @Compare + 2
    IF @Initials = @Initials2 and len(@Initials) > 1 set  @Compare = @Compare + 1
    IF @Forename = @Forename2 set @Compare = @Compare + 4
    IF @Surname IN ('Unknown','Withheld') set @Compare = 15 
  END
  Return  @Compare   
     
  
END

GO
/****** Object:  UserDefinedFunction [dbo].[ufn_IWParseImportValue]    Script Date: 01/04/2020 22:38:41 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

ALTER FUNCTION [dbo].[ufn_IWParseImportValue](@ImportValue varchar(100),@Namepart integer)

RETURNS varchar(75)

--	DESCRIPTION
--	 Parses the name as held in the Import_Value (which is after parsing by R6 so shoudl be
--  Title/Initials/ForeName and Surname are output as a ; separated string  
--	@ImportValue @Namepart 1 = Title,2 =Forename,3 =Initials,4 = Surname  	
    
--	AUTHOR:	Mike Weideli December 2018 
--  UPDATED Jan 2020 for Field Length Change 

AS
BEGIN

DECLARE @Key char(16),
		@Surname varchar(35),
		@Forename varchar(35),
		@Initials varchar(8),
		@Title varchar(4),
		@Working varchar(100),
		@Part varchar(50),
		@CurrentPos int,
		@NextPos int,
		@Result varchar(100)
-- Remove any ; in name just in case 
SET @IMPORTVALUE = REPLACE(@IMPORTVALUE,';',':')

	/*===================================================*\
	  Break down the ImportValue into name constituents.
	\*===================================================*/
	SET	@Surname = ''
	SET	@Working = @ImportValue
	SET 	@CurrentPos = 1
	SET	@NextPos = 1

	-- Rule 1. Check for 'xxx' or 'van xxx'
	IF CharIndex('.', @Working) = 0 BEGIN
		SET @NextPos = CharIndex(' ', @Working)
		IF @NextPos = 0
			SET @Surname = @Working
		ELSE BEGIN
			SET @Surname = SubString(@Working, @CurrentPos, @NextPos - 1)
			IF @Surname IN ('van', 'von') BEGIN
				SET @NextPos = CharIndex(' ', @Working, @NextPos + 1)
				IF @NextPos = 0
					SET @Surname = @Working
				ELSE
					SET @Surname = ''
			END ELSE
				SET @Surname = ''
		END
		IF @Surname <> ''
			SET @Working = ''
	END

	-- Rule 2. Check for 'xxx, ###' or 'van xxx, ###'
	IF (@Surname = '') BEGIN
		SET @NextPos = CharIndex(',', @Working)
		IF @NextPos <> 0 BEGIN
			SET @Surname = SubString(@Working, 1, @NextPos - 1)
			SET @Working = LTrim(SubString(@Working, @NextPos + 1, Len(@Working)))
		END
	END

	-- Rule 3. Check for '### xxx'/'###.xxx' or '### van xxx'/'###.van xxx'
	IF (@Surname = '') AND (Right(@Working, 1) <> '.') BEGIN
		SET @Working = Reverse(@Working)
		SET @NextPos = CharIndex(' ', @Working)
		IF (@NextPos = 0) OR (CharIndex('.', SubString(@Working, 1, @NextPos - 1)) <> 0)
			SET @NextPos = CharIndex('.', @Working)
		-- Surname is last word.
		SET @Surname = Reverse(SubString(@Working, 1, @NextPos - 1))
		SET @CurrentPos = @NextPos + 1
		-- Check for 'van/von' too
		SET @NextPos = CharIndex(' ', @Working, @CurrentPos)
		IF (@NextPos = 0) OR (CharIndex('.', SubString(@Working, @CurrentPos, @NextPos - 1)) <> 0)
			SET @NextPos = CharIndex('.', @Working, @CurrentPos)
		SET @Part = RTrim(SubString(@Working, @CurrentPos, Abs(@CurrentPos - @NextPos)))
		IF Reverse(@Part) IN ('van', 'von') BEGIN
			SET @Surname = LTrim(Reverse(SubString(@Working, 1, @NextPos - 1)))
			SET @CurrentPos = @NextPos + 1
		END

		SET @Working = Reverse(LTrim(SubString(@Working, @CurrentPos, Len(@Working))))
	END

	-- Rule 4. Check for 'xxx #.'/'xxx ###.' or 'van xxx #.'/'van xxx ###.'
	IF @Surname = '' BEGIN
		SET @NextPos = CharIndex(' ', @Working)
		If @NextPos > 0 
		BEGIN  
		  SET @Surname = SubString(@Working, 1, @NextPos - 1)
		  IF @Surname IN ('van', 'von') BEGIN
		 	SET @NextPos = CharIndex(' ', @Working, @NextPos + 1)
			SET @Surname = SubString(@Working, 1, @NextPos - 1)
		  END
        END 
		SET @Working = SubString(@Working, @NextPos + 1, Len(@Working))
	END

	-- Parse remainder into Forename, Initials and Title
	SET @Part     = '';
	SET @Forename = '';
	SET @Initials = '';
	SET @Title    = '';
	SET @CurrentPos = 1

	WHILE @CurrentPos <= Len(@Working) BEGIN
		IF SubString(@Working, @CurrentPos, 1) IN (' ', '.') BEGIN
			IF Len(@Part) = 1
				SET @Initials = @Initials + @Part + '.'
			ELSE
			IF @Part IN ('mr', 'mrs' ,'ms' ,'miss' ,'dr' ,'sir' ,'lord' ,'rev' ,'col')
				SET @Title = @Part
			ELSE
				SET @Forename = RTrim(LTrim(@Forename + ' ' + @Part))
			SET @Part = ''
		END ELSE
			SET @Part = @Part + SubString(@Working, @CurrentPos, 1)
	
		SET @CurrentPos = @CurrentPos + 1
	END

	IF @Part <> ''  
		IF Len(@Part) = 1
			SET @Initials = @Initials + @Part + '.'
		ELSE
		IF @Part IN ('mr', 'mrs' ,'ms' ,'miss' ,'dr' ,'sir' ,'lord' ,'rev' ,'col')
			SET @Title = @Part
		ELSE
			SET @Forename = RTrim(LTrim(@Forename + ' ' + @Part))
  
    If @Title ='' set @Title = Null
    If @ForeName = '' set @ForeName = Null
    If @Initials = '' set  @Initials = Null         
    If @Surname = '' set @Surname = Null            
             
    if @Namepart = 1 SET  @Result = @Title
    if @Namepart = 2 SET  @Result = @ForeName
    if @Namepart = 3 SET  @Result = @Initials
    if @Namepart = 4 SET  @Result = @surname 
  
    RETURN @Result 


END

GO
/****** Object:  StoredProcedure [dbo].[usp_Individual_Insert]    Script Date: 01/04/2020 22:41:55 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

/*===========================================================================*\
  Description:
	Inserts a new individual record.

  Parameters:
	@Key			New key, output.
	@Title
	@Forename
	@Initials
	@Honorifics
	@Surname
	@Comment
	@DOBStart
	@DOBEnd
	@DOBType
	@DODStart
	@DODEnd
	@DODType
	@Floreat
	@EnteredBy
	@DepartmentKey

  Created:	July 2009
  Modified  January 2020 for length of Surname and Forename
  Last revision information:
    $Revision: 1 $
    $Date: 16/07/09 11:51 $
    $Author: Ericsalmon $

\*===========================================================================*/

ALTER PROCEDURE [dbo].[usp_Individual_Insert]
	@Key			CHAR(16)	OUTPUT,
	@Title			VARCHAR(4)	= NULL,
	@Forename		VARCHAR(35)	= NULL,
	@Initials		VARCHAR(8)	= NULL,
	@Honorifics		VARCHAR(20)	= NULL,
	@Surname		VARCHAR(35),
	@Comment		TEXT		= NULL,
	@DOBStart		INT			= NULL,
	@DOBEnd			INT			= NULL,
	@DOBType		VARCHAR(2)	= NULL,
	@DODStart		INT			= NULL,
	@DODEnd			INT			= NULL,
	@DODType		VARCHAR(2)	= NULL,
	@Floreat		VARCHAR(12)	= NULL,
	@EnteredBy		CHAR(16),
	@DepartmentKey	CHAR(16)	= NULL
AS
	SET NOCOUNT ON

	EXECUTE spNextKey 'Name', @Key OUTPUT

	-- Handles the First Run issue. The user is going to be the new one being created.
	IF ISNULL(@EnteredBy, '') = ''
		SET @EnteredBy = @Key

	INSERT INTO "Name" (
		Name_Key,
		Organisation,
		Entered_By
	) VALUES (
		@Key,
		0,
		@EnteredBy
	)

	INSERT INTO Individual (
		Name_Key,
		Title,
		Forename,
		Initials,
		Honorifics,
		Surname,
		Comment,
		Born_Vague_Date_Start,
		Born_Vague_Date_End,
		Born_Vague_Date_Type,
		Died_Vague_Date_Start,
		Died_Vague_Date_End,
		Died_Vague_Date_Type,
		Person_Floreat,
		Entered_By,
		Organisation_Department_Key
	) VALUES (
		@Key,
		@Title,
		@Forename,
		@Initials,
		@Honorifics,
		@Surname,
		@Comment,
		@DOBStart,
		@DOBEnd,
		@DOBType,
		@DODStart,
		@DODEnd,
		@DODType,
		@Floreat,
		@EnteredBy,
		@DepartmentKey
	)
		
GO

UPDATE IW_MATCH_RULE SET TABLE_CREATE_SQL = 'CREATE TABLE #Names(
   Import_Value VARCHAR(100) COLLATE SQL_Latin1_General_CP1_CI_AS PRIMARY KEY,
   Match_Count INT,
   Match_Value VARCHAR(86) COLLATE SQL_Latin1_General_CP1_CI_AS,
   Match_Key CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS,
   Manual_Match BIT DEFAULT 0,
   Remembered BIT DEFAULT 0,
   Notes VARCHAR(100)  COLLATE SQL_Latin1_General_CP1_CI_AS,
   Title Varchar(4) COLLATE SQL_Latin1_General_CP1_CI_AS,
   ForeName VARCHAR (35) COLLATE SQL_Latin1_General_CP1_CI_AS,
   Initials VARCHAR (8) COLLATE SQL_Latin1_General_CP1_CI_AS,
   Surname VARCHAR (35) COLLATE SQL_Latin1_General_CP1_CI_AS 
   )' WHERE IW_Match_Rule_Key = 'SYSTEM0100000000'

