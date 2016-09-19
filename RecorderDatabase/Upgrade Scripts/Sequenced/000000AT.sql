/******  Corrections to 6.26 following testing ******/

/****** Object:  UserDefinedFunction [dbo].[FormatIndividualFull]    Script Date: 09/14/2016 15:49:41 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO



ALTER FUNCTION [dbo].[FormatIndividualFull](@Title varchar(10), @Initials varchar(8), @Forename varchar(30), @Surname varchar(30))
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
--      REVISED Mike Weideli September 2016 - Alter order to facilitate searching

AS
BEGIN

--****************************************************************************************************

RETURN   ISNULL(@Forename + ' ','') +  @Surname + ISNULL(' ' + @Title ,'') +
 isnull(' ' +@initials,'')

--****************************************************************************************************

END

GO

/****** Object:  UserDefinedFunction [dbo].[ufn_GetDobandDod]    Script Date: 09/14/2016 19:29:11 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

/*===========================================================================*\
  Description:	Returns the formatted Dob and dod for a given name key.

  Parameters:	@NameKey

  Created:	September 2016

  Last revision information:
    $Author: MikeWeideli $
    
\*===========================================================================*/
ALTER FUNCTION [dbo].[ufn_GetDobandDod]
	(@NameKey char(16))
RETURNS varchar(100)
AS
BEGIN
    DECLARE	@DobDod varchar(100)
	SET @DobDod = ''
    select @DobDod =  isnull([dbo].[LCReturnVagueDateShort]
                  (BORN_VAGUE_DATE_START, BORN_VAGUE_DATE_END, BORN_VAGUE_DATE_TYPE),'')
                   + ' - ' +
                   isnull([dbo].[LCReturnVagueDateShort] 
                   (DIED_VAGUE_DATE_START, DIED_VAGUE_DATE_END, DIED_VAGUE_DATE_TYPE),'')
                    FROM INDIVIDUAL 
                    WHERE Name_Key = @NameKey                  
 
          
    RETURN @DobDOd  
END