/****** Changes associated with adding the address to the name search ******/



/****** Object:  UserDefinedFunction [dbo].[ufn_GetAddress]    Script Date: 09/03/2016 19:13:06 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO


/*===========================================================================*\
  Description:	Returns the formatted Address for the given name key.
	
  Parameters:	@NameKey

  Created:	September 2016

  Last revision information:
    $Author: Mike Weideli 
    
\*===========================================================================*/
CREATE FUNCTION [dbo].[ufn_GetAddress]
	(@NameKey char(16))
RETURNS varchar(180)
AS
BEGIN
	DECLARE	@FormattedAddress varchar(180)
	

	SELECT	@FormattedAddress  = 
		[dbo].[ufn_Format_Line](Address_1) +
		[dbo].[ufn_Format_Line](Address_2) +
		[dbo].[ufn_Format_Line](Address_3) + 
		[dbo].[ufn_Format_Line](Address_4) + 
		[dbo].[ufn_Format_Line](Address_Country) +
		[dbo].[ufn_Format_Line](Address_PostCode) 
		FROM	Address 
		WHERE	Name_Key = @NameKey and Preferred = 1
	
	if right(@FormattedAddress,1) = ',' SET @FormattedAddress = 
	left(@FormattedAddress,len(@FormattedAddress)-1) 
	
	if isnull(@FormattedAddress,'')  = ''  SET @FormattedAddress = 'Address not available'
	
	RETURN @FormattedAddress
END

GO
/****** Object:  UserDefinedFunction [dbo].[ufn_Format_Line]    Script Date: 09/03/2016 17:45:15 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

GRANT EXECUTE ON [dbo].[ufn_GetAddress] TO PUBLIC

GO

/*===========================================================================*\
  Description:	Returns the formatted line for a  comma separated list
	
  Parameters:	@Line

  Created:	September 2016

  Last revision information:
    $Author: Mike Weideli 
    
\*===========================================================================*/
CREATE FUNCTION [dbo].[ufn_Format_Line]
	(@Line varchar(40))
RETURNS varchar(41)
AS
BEGIN
     DECLARE @NewLine varchar(41) 

     if isnull(@Line,'') = '' 
       SET @NewLine = ''
    else
       SET @Newline = @Line + ','
       
    RETURN @NewLine   
END

GO

GRANT EXECUTE ON [dbo].[ufn_Format_Line] TO PUBLIC


GO

/****** Object:  UserDefinedFunction [dbo].[ufn_GetDobandDod]    Script Date: 09/04/2016 08:44:11 ******/
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
CREATE FUNCTION [dbo].[ufn_GetDobandDod]
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
                    WHERE Name_Key = @NameKey ;
                    
   
    if @DobDod = ' - ' set @DobDod = ''
       
    RETURN @DobDOd  
END

GO

GRANT EXECUTE ON [dbo].[ufn_GetDobandDod] TO PUBLIC
