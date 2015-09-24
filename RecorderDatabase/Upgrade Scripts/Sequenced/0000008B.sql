/****** Object:  UserDefinedFunction [dbo].[LCGRToLLGBNI]    Script Date: 07/14/2012 21:17:24 ******/


IF  EXISTS (SELECT * FROM sysobjects WHERE id = OBJECT_ID(N'[dbo].[LCGRToLLGBNI]') AND type in (N'FN', N'TF'))
DROP FUNCTION [dbo].[LCGRToLLGBNI]
GO


SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO
/*===========================================================================*\
  Description:	Converts a UK or Irish Grid Ref to Lat Long as used in Recorder 6
  If not valid returns -9999999 
  Parameters:
	Spatial_Ref	@SpatialRef  
        Spatial_Ref_System    @SpatialRefSystem 
        If 1 then ruturns latitude otherwise longitude @isLat bit 
        If 1 then centre of square is used. If 0 then SW corner  @C bit 

  Created:	July  2012
  Author: MikeWeideli 

\*=========================================================================== */

CREATE FUNCTION [dbo].[LCGRToLLGBNI]
(@SpatialRef Varchar(20), @SpatialRefSystem varchar(4), @isLat bit,@C bit )
RETURNS float
AS
BEGIN

DECLARE @ReturnValue float
Declare @a float
Declare @b float  
Declare @N float 
Declare @k float
Declare @EZero float
Declare @NZero float 
Declare @Lattrueorigin float
Declare @Longtrueorigin float
declare @ESquared float
declare @LatTrueOriginRad float
declare @LongTrueOriginRad float
declare @Pi float
declare @iNorthings float 
declare @iEastings float
declare @Meridian  float
declare @sink float
declare @tank float
declare @Cosk float
declare @v float 
declare @rho float
declare @etaSquared float
declare @etrue float
declare @Jthree float
declare @jFour float
declare @jFive float
declare @latitude float
declare @longitude float
declare @JSix float
declare @JSEven float
declare @JEight float
declare @JNine float
Declare  @phiminus float
Declare  @phiPlus  float 
declare  @NUtemp float 
declare @REmember float 
Declare @FiveFour float
Declare @Fifteeneight float 
Declare @TwentyOneEight float 
Declare @ThirtyFiveTwentyFour float 
set @FiveFour = 1.25
set  @Fifteeneight = 1.875 
set  @TwentyOneEight = 2.625
set  @ThirtyFiveTwentyFour = 1.45833333333333333

SET @ReturnValue = -999999
If @SpatialRefSystem = 'OSGB' or @SpatialRefSystem = 'OSNI' 

BEGIN
set @iNorthings =  [dbo].[LCReturnNorthingsV2](@SpatialRef,@SpatialRefSystem,@C)
set @iEastings =  [dbo].[LCReturnEastingsV2](@SpatialRef,@SpatialRefSystem,@C)

if  @SpatialRefSystem = 'OSGB'     
BEGIN 
	set   @a = 6375020.481 
	set   @b = 6353722.49  
	set   @Ezero = 400000 
	set   @Nzero = -100000 

	set  @Lattrueorigin = 49
	set  @Longtrueorigin = -2
END 
ELSE
    BEGIN 
      set   @a =   6377563.396
	  set   @b = 6356256.96  
	  set   @Ezero = 200000
	  set   @Nzero = 250000

	  set  @Lattrueorigin = 53.5
	  set  @Longtrueorigin = -8
    END
      
      
set @eSquared = (SQUARE(@a) - SQUARE(@b)) / SQUARE(@a)
      
Set @N = (@a - @b) / (@a + @b)

set @LatTrueOriginRad = RADIANS(@Lattrueorigin)
set @LongTrueOriginRad =  RADIANS(@Longtrueorigin)

set @k = ((@iNorthings - @Nzero) / @a) + @LatTrueOriginRad

set @phiminus = @k - @LatTrueOriginRad 
set @phiPlus = @k + @LatTrueOriginRad  
set @NUTemp = ((1 + @N + ((@FiveFour) * (POWER(@N , 2))) + (@FiveFour) * (POWER(@N , 3))) 
* @phiminus) - ((3 * @N + 3 * (POWER(@N , 2)) + (@TwentyOneEight) 
* (POWER(@N , 3))) * Sin(@phiminus) * Cos(@phiPlus)) + (((@FifteenEight) 
* (POWER(@N , 2)) + (@FifteenEight) * (POWER(@N, 3))) 
* Sin(2 * @phiminus) * Cos(2 * @phiPlus)) - (((@ThirtyFiveTwentyfour) * (POWER(@N , 3))) 
* Sin(3 * @phiminus) * Cos(3 * @phiPlus))

set @meridian = @B * @nutemp


While Abs(@iNorthings - @Nzero - @meridian) > 0.0001  
BEGIN  
      set @k = @k + ((@iNorthings - @Nzero - @meridian) / @a)
      set @phiminus = @k - @LatTrueOriginRad 
      set @phiPlus = @k + @LatTrueOriginRad  
      set @NUTemp = ((1 + @N + ((@FiveFour) * (POWER(@N , 2))) + (@FiveFour) * (POWER(@N , 3))) * @phiminus) - ((3 * @N + 3 * (POWER(@N , 2)) + (@TwentyOneEight) * (POWER(@N , 3))) * Sin(@phiminus) * Cos(@phiPlus)) + (((@FifteenEight) * (POWER(@N , 2)) + (@FifteenEight) * (POWER(@N, 3))) * Sin(2 * @phiminus) * Cos(2 * @phiPlus)) - (((@ThirtyFiveTwentyfour) * (POWER(@N , 3))) * Sin(3 * @phiminus) * Cos(3 * @phiPlus))
      set @meridian = @B * @nutemp
END
set @sinK = SIN(@k)
set @tanK = TAN(@k)
set @cosK = COS(@k)

set @v = @a / Sqrt(1 - (@eSquared * (POWER(@sinK, 2) )))

set @rho = @v * (1 - @eSquared) / (1 - (@eSquared * (POWER(@sinK, 2))))

set   @etaSquared = @v / @rho - 1
set  @eTrue = @iEastings - @Ezero

set @JThree = @tanK / (2 * @rho * @v)
set @JFour = (@tanK / (24 * @rho * (POWER(@v , 3)))) * (5 + 3 * POWER(@tanK , 2) + @etaSquared - 9 * (POWER(@tanK,2)) * @etaSquared)
set @JFive = (@tanK / (720 * @rho * (POWER(@v , 5))) * (61 + 90 * (POWER(@tanK , 2)) + 45 * (POWER(@tanK , 4))))
 
set @Latitude = (@k - @JThree * (POWER(@eTrue , 2)) + (POWER(@eTrue , 4)) * @JFour - (POWER(@eTrue , 6)) * @JFive)




set  @JSix = 1.00000000000000 / (@cosK * @v)
set @JSeven = (1.000000000000 / (@cosK * 6 * (POWER(@v , 3)))) * ((@v / @rho) + 2 * (POWER(@tanK , 2)))
set  @JEight = (1.00000000000 / (@cosK * 120 * (POWER(@v, 5)))) * (5 + 28 * (POWER(@tanK , 2))  + 24 * (POWER(@tanK , 4)))
set @JNine = (1 / (@cosK * 5040 * (POWER(@v, 7)))) * (61 + 662 * (POWER(@tanK, 2)) + 1320 * (POWER(@tanK , 4)) + 720 * (POWER(@tanK,6)))
 

set @Longitude = @LongTrueOriginRad + (@eTrue * @JSix) - ((POWER(@eTrue,3)) * @JSeven) + ((POWER(@eTrue, 5)) * @JEight) - (POWER(@eTrue , 7) * @JNine)
  

          
if @isLat  = 1
    set @Returnvalue = DEGREES(@Latitude)
ELSE
    set @Returnvalue = DEGREES(@LOngitude)  


--****************************************************************************************************
end 
RETURN @ReturnValue
 

    


END



GO


GRANT EXECUTE ON [dbo].[LCGRToLLGBNI] TO PUBLIC 

