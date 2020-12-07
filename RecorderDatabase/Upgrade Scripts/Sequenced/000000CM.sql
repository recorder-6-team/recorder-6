/*Changes necessary to add 5 additional levels into the taxonomic hierarchy ******/
ALTER TABLE ORGANISM
ALTER COLUMN LINEAGE varchar(54)
Go
ALTER TABLE ORGANISM
ALTER COLUMN Sort_Order varchar(50)

GO
/****** Object:  UserDefinedFunction [dbo].[LCOrgCreateSort]    Script Date: 10/23/2020 10:26:07 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
/*===========================================================================*\
  Description:	Function to turn the Organism Lineage into a Sort Order
  Parameters:
		@Lineage - eg (1/1/ZZ) 
              

  Created:	July 2012
  Updated: March 2016 to increase lenth of lineage (40 char) and sort order 36 char
  Updated  October 2020 to increase lenth of lineage (54 char) and sort order 50 char 
  Author: MikeWeideli 


\*=========================================================================== */

ALTER function [dbo].[LCOrgCreateSort] (@Lineage varchar(54))
Returns Char(50)
as
BEGIN 
  Declare @Work varchar(54)
  Declare @S integer
  Declare @Temp varchar(54)
  Declare @SortLevel varchar(54)
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

    SET @SortLevel = @SortLevel + left('0000000000000000000000000000000000000000000000000',50-len(@sortLevel))
  
  RETURN  @Sortlevel
  
END






