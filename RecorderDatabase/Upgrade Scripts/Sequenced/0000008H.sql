
/****** Object:  UserDefinedFunction [dbo].[LCOrgCreateSort]    Script Date: 10/08/2012 18:04:43 ******/


IF  EXISTS (SELECT * FROM sysobjects WHERE id = OBJECT_ID(N'[dbo].[LCOrgCreateSort]') AND type in (N'FN', N'TF'))
DROP FUNCTION [dbo].[LCOrgCreateSort]

GO

SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO
/*===========================================================================*\
  Description:	Function to turn the Organism Lineage into a Sort Order
  Parameters:
		@Lineage - eg (1/1/ZZ) 
              

  Created:	July 2012
  Author: MikeWeideli 

\*=========================================================================== */

CREATE function [dbo].[LCOrgCreateSort] (@Lineage varchar(30))
Returns Char(30)
as
BEGIN 
  Declare @Work varchar(30)
  Declare @S integer
  Declare @Temp varchar(30)
  Declare @SortLevel varchar(30)
  SET @Work = Rtrim(@Lineage) 
  SET @S = charindex('\',@work)
  SET @sortlevel = ''
  
  WHILE @S <> 0 
  BEGIN
     SET @Temp = Left(@work,@S-1)
     If len(@Temp) = 2 SET @SortLevel = @SortLevel + @Temp
     ELSE  SET @SortLevel = @SortLevel + '0' +  @Temp   
  
     SET @Work = Right(@Work,Len(@work) - @S)
     SET @S = charindex('\',@work)
   
  END   

    SET @SortLevel = @SortLevel + left('00000000000000000000000000000',30-len(@sortLevel))
  
  RETURN  @Sortlevel
  
END



GO

GRANT EXECUTE ON [dbo].[LCOrgCreateSort] TO PUBLIC 