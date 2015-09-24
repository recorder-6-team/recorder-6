/****** Object:  UserDefinedFunction [dbo].[LCValidateUKGR]    Script Date: 07/14/2012 21:18:52 ******/

IF  EXISTS (SELECT * FROM sysobjects WHERE id = OBJECT_ID(N'[dbo].[LCValidateUKGR]') AND type in (N'FN', N'TF'))
DROP FUNCTION [dbo].[LCValidateUKGR]

GO

SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

/*===========================================================================*\
  Description:	Checks to see if provided text is valid UK Grid Ref
  Parameters:
	Spatial Ref  @Gridref
        Minimum length allowed @MiniumLength

  Created:	July 2012
  Author: MikeWeideli 

\*=========================================================================== */

CREATE FUNCTION [dbo].[LCValidateUKGR]
(@GridREF varchar(100), @MinimumLength int)
RETURNS int

AS
BEGIN
Declare @Reason int
Declare @x int 
set @Reason = 0
If charindex(left(@GridRef,1 ),'HNSTO') = 0   set @reason = 1
If @Reason = 0

BEGIN
   If substring(@GridRef,2,1 ) < 'A' or substring(@GridRef,2,1 ) > 'Z' OR substring(@GridRef,2,1 ) =  'I' set @Reason = 2
END

IF @REASON = 0
BEGIN
  Set @Reason = 
  CASE 
     When left(@GridRef,1 ) = 'H' AND charindex(substring(@GridRef,2,1),'PUYZ') = 0 then 7
     When left(@GridRef,1 ) = 'O' AND substring(@GridRef,2,1) <> 'V' then  7  
     When left(@GridRef,1 ) = 'T' AND charindex(substring(@GridRef,2,1),'ABFGLMQRV') = 0 then 7  
  Else
      0
  END       
     

END  

If @Reason = 0
BEGIN
   if len(@GridRef) < @MinimumLength set @Reason = 3
   if len(@GridRef) <> 12 and len(@GridRef) <> 10 and len(@GridRef) <> 8 and  len(@GridRef) <> 6 and len(@GridRef) <> 5 AND len(@GridRef) <> 4 set @Reason = 3
END
If @Reason = 0
BEGIN
 if len(@GridRef) <> 5 
 BEGIN
	set @x = 2
	WHILE @x < len(@GridRef) 
	BEGIN
		set @X = @x + 1
		if substring(@GridRef,@x,1 ) > '9' OR  substring(@GridRef,@x,1 ) < '0'  set @reason = 4
    
	END 
 END
 ELSE
 BEGIN 
 set @x = 2
	WHILE @x < 5 
	BEGIN
		set @X = @x + 1
		if substring(@GridRef,@x,1 ) > '9' OR  substring(@GridRef,@x,1 ) < '0'  set @reason = 4
    
	END 
    If  (substring(@GridRef,5,1 ) < 'A' OR  substring(@GridRef,5,1 ) > 'Z') OR  substring(@GridRef,5,1 ) = 'O'  set @reason = 4
   
 END 

END

 RETURN @Reason

END


GO

GRANT EXECUTE ON [dbo].[LCValidateUKGR] TO PUBLIC


