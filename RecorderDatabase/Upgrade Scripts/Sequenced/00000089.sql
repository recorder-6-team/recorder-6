/****** Object:  UserDefinedFunction [dbo].[LCENtoGR]    Script Date: 07/14/2012 21:32:20 ******/


IF  EXISTS (SELECT * FROM sysobjects WHERE id = OBJECT_ID(N'[dbo].[LCENtoGR]') AND type in (N'FN', N'TF'))
DROP FUNCTION [dbo].[LCENtoGR] 


GO

/*===========================================================================*\
  Description:	Function to Return a UK Grid Ref given Eastings and Northings
  If not valid returns  NULL 
   
  Parameters:
	Eastings  @Eastings
        Northings @Northings
        Precision @Precision - len of numeric part of grid ref to be returned
 

  Created:	October 2012

  Last revision information:
    $Revision: 2 $
    $Date: 10/10/12 09:30 $
    $Author: MikeWeideli $

\*=========================================================================== */

SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO


CREATE FUNCTION [dbo].[LCENtoGR] 
(@Eastings float, @Northings float,@Precision Integer)
RETURNS varchar(25)

AS
BEGIN
Declare @ReturnValue varchar(25)

Declare @lLE integer
Declare @lLN integer 
Declare @lRestE integer
Declare @lRestN integer 
Declare @sA  varchar(2) 
Declare @lOffN  integer 
Declare @lOffE  integer 
Declare @lSqN  integer
Declare @lSqe  integer 
Declare @lEastings as integer
Declare @lNorthings as integer
Declare @Five1 as varchar(5)
Declare @Five2 as varchar(5)

set @lEastings = CEILING(@Eastings)
set @lNorthings = CEILING(@Northings)

set @lLE = Floor(@lEastings / 100000)
set @lRestE = @lEastings - (@lLE * 100000)

   
set @lLN = Floor(@lNorthings / 100000)
set @lRestN = @lnorthings - (@lLN * 100000)
   
-- get letters
    If @lLN < 5 
    BEGIN
       If @lLE < 5 
         BEGIN 
          set @sA = 'S'
          set @lOffN = 0
          set @lOffE = 0
         END  
         ELSE 
         BEGIN
          set @sA = 'T'
          set @lOffN = 0
          set @lOffE = 5
         END
      
    END 
    If @lLN >= 5 And @lLN < 10 
    BEGIN
          If @lLE < 5 
          BEGIN 
          
          set @sA = 'N'
          set  @lOffE = 0
          set  @loFfN = 5
          END 
          ELSE
          BEGIN 
           set @sA = 'O'
           set @lOffE = 5
           set @lOFfN = 5
          END      
    END
    
    If @lLN >= 10 
    BEGIN
       If @lLE < 5 
       BEGIN
          set @sA = 'H'    
          set @lOffE = 0
          set @loffN = 10
       END 
       ELSE
       BEGIN
          set @sA = 'J'
          set @lOffE = 5
          set @lOffN = 10
       END 
  END 
  set @lOffE = @lOffE * 100000
  set @lOffN = @lOffN * 100000
  set @lSqN = @lnorthings - @lOffN - @lRestN
  set @lSqe = @leastings - @lOffE - @lRestE
 
     
  If @lSqN = 400000 set @sA = @sA + Char((@lSqe / 100000) + 65)
      
  If @lSqN = 200000 set @sA = @sA + Char((@lSqe / 100000) + 76)
 
  If @lSqN = 100000 set @sA = @sA + Char((@lSqe / 100000) + 81)

  If @lSqN = 0  set @sA = @sA + Char((@lSqe / 100000) + 86)

  If @lSqN = 300000 AND @lSqe < 300000 set @sA = @sA + Char((@lSqe / 100000) + 70)
  If @lSqN = 300000 AND  @lSqe >= 300000  set @sA = @sA + Char((@lSqe / 100000) + 71)
 
  set @Five1 = cast(@lrestE as varchar(5))
  set @Five2 = cast(@lrestN as varchar(5))
  while len(@five1) < 5 
  BEGIN
     set @Five1 = '0' + @Five1 
  END     
   while len(@five2) < 5 
  BEGIN
     set @Five2 = '0' + @Five2 
  END  
  SET @ReturnValue =
  CASE @Precision 
     
     WHEN  8 THEN   @sA + left(@Five1,4) +  left(@Five2,4) 
     WHEN  6 THEN   @sA + left(@Five1,3) +  left(@Five2,3)   
  ELSE
     @sA + @Five1 +  @Five2
     
  END      
 
RETURN @ReturnValue
END



GO

GRANT EXECUTE ON [dbo].[LCENtoGR] TO PUBLIC 
