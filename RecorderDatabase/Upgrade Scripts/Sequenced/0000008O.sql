/****** Object:  UserDefinedFunction [dbo].[LCReturnTopLevelLocation]    4/12/2005 10:34:21 ******/


IF  EXISTS (SELECT * FROM sysobjects WHERE id = OBJECT_ID(N'[dbo].[LCReturnTopLevelLocation]') AND type in (N'FN', N'TF'))
DROP FUNCTION [dbo].[LCReturnTopLevelLocation]

GO


SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

/*===========================================================================*\
  Description:	Function to return ths Top level of a Location
  Parameters:
		@LOCCKey-  location key 
             
  Created:	April 2012
  Author: MikeWeideli

\*=========================================================================== */

CREATE FUNCTION [dbo].[LCReturnTopLevelLocation]
(@LOCCKey char(16))
RETURNS varchar(100)

AS
BEGIN

--****************************************************************************************************
DECLARE @ReturnString varchar(100)
DECLARE @LocationName varchar(100)
DECLARE @ParentKey char(16)
SET @LocationName = (SELECT Item_Name FROM LOCATION_NAME LN WHERE LN.LOCATION_KEY = 
@LOCCKEY AND LN.Preferred = 1 ) 
SET @ParentKey = (SELECT Parent_Key FROM LOCATION WHERE LOCATION_KEY = 
@LOCCKEY) 

WHILE  @ParentKey IS NOT NULL
  BEGIN
    SET @LocationName = (SELECT Item_Name FROM LOCATION_NAME LN WHERE LOCATION_KEY = 
    @ParentKey AND LN.Preferred = 1 ) 
    SET @ParentKey = (SELECT Parent_Key FROM LOCATION WHERE LOCATION_KEY = 
    @ParentKey) 
  END 
--****************************************************************************************************
SET @ReturnString = @LocationName  

RETURN @ReturnString
END


GO

GRANT EXECUTE  ON [dbo].[LCReturnTopLevelLocation] TO PUBLIC

GO

IF  EXISTS (SELECT * FROM sysobjects WHERE id = OBJECT_ID(N'[dbo].[LCLocationTopParent]') AND type in (N'FN', N'TF'))
DROP FUNCTION [dbo].[LCLocationTopParent] 

GO


SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

CREATE FUNCTION [dbo].[LCLocationTopParent](@LocationKey char(16),  @ReturnKey bit = 1)
RETURNS varchar(100)

AS
BEGIN
DECLARE @ParentKey varchar(16)
DECLARE @CurrentKey varchar(16)
DECLARE @ReturnString varchar(100)

set @Currentkey = @LocationKey
WHILE 1 = 1
BEGIN  
  SELECT @ParentKey =  PARENT_KEY FROM Location where Location_Key = @CurrentKey
  if  @ParentKey is null BREAK  
  set  @CurrentKey = @ParentKey     
      
END

IF @ReturnKey = 0 
BEGIN
   SELECT @ReturnString = ITEM_NAME FROM LOCATION_NAME WHERE LOCATION_KEY = @CurrentKey  AND PREFERRED = 1
END

ELSE
    SET @ReturnString = @CurrentKey 
  

RETURN @ReturnString


END

GO

GRANT EXECUTE  ON [dbo].[LCLocationTopParent] TO PUBLIC



