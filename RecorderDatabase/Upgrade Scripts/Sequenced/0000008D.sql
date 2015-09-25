/****** Object:  UserDefinedFunction [dbo].[LCToRataDieFromER]    Script Date: 07/14/2012 21:19:20 ******/


IF  EXISTS (SELECT * FROM sysobjects WHERE id = OBJECT_ID(N'[dbo].[LCToRataDieFromER]') AND type in (N'FN', N'TF'))
DROP FUNCTION [dbo].[LCToRataDieFromER]


GO

SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO


/*===========================================================================*\
  Description:	Takes a date in the format mm dd, YYYY   and returns the number used in Recorder 6
  Parameters:
	Date in format mm dd,YYYY  @DateString

  Created:	October 2012
  Author: MikeWeideli 

\*=========================================================================== */

CREATE FUNCTION [dbo].[LCToRataDieFromER]
(@DateString varchar(12))
RETURNS int

AS
BEGIN
 

declare @RD int
declare @Y int
declare @M int
declare @D int
declare @A int
declare @MMM char(3)

set @MMM = left(@Datestring,3)
SET @M = 
case @MMM 
    WHEN  'JAN' then 1
    WHEN  'FEB' then 2
    WHEN  'MAR' then 3
    WHEN  'APR' then 4
    WHEN  'MAY' then 5
    WHEN  'JUN' then 6
    WHEN  'JUL' then 7
    WHEN  'AUG' then 8
    WHEN  'SEP' then 9
    WHEN  'OCT' then 10
    WHEN  'NOV' then 11
    WHEN  'DEC' then 12
    ELSE 1
END    
        

set @Y = cast(right( @Datestring,4) as int)
set @D = cast(substring(@datestring,5, (CHARINDEX(',',@datestring)-5)) as int)


if @M < 3 
   Begin 
	set @M = @M + 12 
            set @Y = @Y - 1 
end 

set @A = 153*@M-457
set @RD = @D + @A/5 + 365*@Y + @Y/4 - @Y/100 + @Y/400 - 306
set @RD = @RD  - 693594 
 


return @RD


END


GO

GRANT EXECUTE ON [dbo].[LCToRataDieFromER] TO PUBLIC

