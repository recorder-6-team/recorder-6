
IF  EXISTS (SELECT * FROM sysobjects WHERE id = OBJECT_ID(N'[dbo].[LCOrgIncrementLineage]') AND type in (N'FN', N'TF'))
DROP FUNCTION [dbo].[LCOrgIncrementLineage] 

GO


/****** Object:  UserDefinedFunction [dbo].[LCOrgIncrementLineage]    Script Date: 10/10/2012 09:23:14 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

/*===========================================================================*\
  Description:	Increments the element within lineage used in the stored procedure 
  which creates lineage and sort order for the Organism table
  Only works for the two characters used in  Lineage    
 Parameters:
	The previous key @PrevKey

  Created:	October 2012

  Last revision information:
  Author: MikeWeideli 

\*=========================================================================== */

Create function [dbo].[LCOrgIncrementLineage] (

@PrevKey VarChar(2))

returns varChar(2)
as
BEGIN 
  Declare @NuKey varchar(2) 
  SET @NuKey = @PrevKey
  IF Right(@NuKey, 1) = 'Z'
  BEGIN 
     IF Len(@NuKey) = 1  SET @NuKey = '10'
        ELSE SET @NuKey = [dbo].[IncrementChar](Left(@NuKey, 1)) + '0'
  END
  ELSE  
    If Len(@NuKey) = 1 SET  @NuKey =[dbo].[IncrementChar](Left(@NuKey, 1))
    ELSE  SET @NuKey = Left(@NuKey, 1) + [dbo].[IncrementChar](Right(@NuKey, 1))
 
  
  RETURN @NuKey 
  
END



GO

GRANT EXECUTE ON [dbo].[LCOrgIncrementLineage] TO Public
